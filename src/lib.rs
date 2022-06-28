//! This library provides the `StrippedPartialEq` derive macro
//! used to automatically implement the `StrippedPartialEq` comparison
//! trait defined in the `locspan` library.
//!
//! ## Usage
//!
//! ```
//! use locspan::Loc;
//! use locspan_derive::StrippedPartialEq;
//!
//! // Implement `StrippedPartialEq` for the `Foo` type.
//! // Type parameters will be required to implement
//! // `StrippedPartialEq` themselves unless they are marked
//! // with `#[stripped]`.
//! #[derive(StrippedPartialEq)]
//! struct Foo<T, #[stripped] S, #[stripped] P> {
//!   a: Loc<T, S, P>,
//!
//!   // Files are compared using `StrippedPartialEq`
//!   // unless they are marked with `#[stripped]`, in
//!   // which case `PartialEq` is used.
//!   #[stripped]
//!   b: std::path::PathBuf
//! }
//! ```
use proc_macro::{Span, TokenStream};
use proc_macro_error::{emit_error, proc_macro_error};
use quote::quote;
use std::collections::HashSet;
use syn::{parse, parse_macro_input, punctuated::Punctuated, token, DeriveInput};

struct Idents {
	_paren: token::Paren,
	idents: Punctuated<syn::Ident, token::Comma>,
}

impl parse::Parse for Idents {
	fn parse(input: parse::ParseStream) -> syn::Result<Self> {
		let content;
		Ok(Self {
			_paren: syn::parenthesized!(content in input),
			idents: Punctuated::parse_terminated(&content)?,
		})
	}
}

#[proc_macro_error]
#[proc_macro_derive(StrippedPartialEq, attributes(stripped, stripped_loc))]
pub fn derive_stripped_partial_eq(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);

	let ident = input.ident;
	let mut generics = input.generics;

	let mut stripped_params = HashSet::new();
	for attr in input.attrs {
		if attr.path.is_ident("stripped") {
			let tokens = attr.tokens.into();
			let idents = parse_macro_input!(tokens as Idents);
			stripped_params.extend(idents.idents)
		}
	}

	for p in generics.params.iter_mut() {
		if let syn::GenericParam::Type(ty) = p {
			let mut stripped = stripped_params.contains(&ty.ident);
			let attrs = std::mem::take(&mut ty.attrs);
			ty.attrs.extend(attrs.into_iter().filter(|attr| {
				if attr.path.is_ident("stripped") {
					stripped = true;
					false
				} else {
					true
				}
			}));

			if !stripped {
				ty.bounds.push(syn::TypeParamBound::Trait(syn::TraitBound {
					paren_token: None,
					modifier: syn::TraitBoundModifier::None,
					lifetimes: None,
					path: syn::Path {
						leading_colon: Some(syn::token::Colon2::default()),
						segments: [
							syn::PathSegment {
								ident: syn::Ident::new("locspan", proc_macro2::Span::call_site()),
								arguments: syn::PathArguments::None,
							},
							syn::PathSegment {
								ident: syn::Ident::new(
									"StrippedPartialEq",
									proc_macro2::Span::call_site(),
								),
								arguments: syn::PathArguments::None,
							},
						]
						.into_iter()
						.collect(),
					},
				}));
			}
		}
	}

	let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

	let comparison = match input.data {
		syn::Data::Struct(s) => {
			fields_comparisons(s.fields.iter().enumerate().map(|(i, field)| {
				let id = match &field.ident {
					Some(ident) => quote! { #ident },
					None => {
						let index = syn::Index::from(i);
						quote! { #index }
					}
				};

				(field, (quote! { self.#id }, quote! { other.#id }))
			}))
		}
		syn::Data::Enum(e) => {
			let cases = e.variants.iter().map(|v| {
				let ident = &v.ident;
				let mut self_args = Vec::new();
				let mut other_args = Vec::new();
				let (self_pattern, other_pattern) = match &v.fields {
					syn::Fields::Named(fields) => {
						let mut self_fields = Vec::new();
						let mut other_fields = Vec::new();

						for field in &fields.named {
							let ident = &field.ident.as_ref().unwrap();
							let self_ident = syn::Ident::new(
								&format!("__self_{}", ident),
								proc_macro2::Span::call_site(),
							);
							let other_ident = syn::Ident::new(
								&format!("__other_{}", ident),
								proc_macro2::Span::call_site(),
							);
							self_args.push(quote! { #self_ident });
							other_args.push(quote! { #other_ident });

							self_fields.push(quote! { #ident: #self_ident });
							other_fields.push(quote! { #ident: #other_ident })
						}

						(
							quote! { { #(#self_fields),* } },
							quote! { { #(#other_fields),* } },
						)
					}
					syn::Fields::Unnamed(fields) => {
						for i in 0..fields.unnamed.len() {
							let id = syn::Ident::new(
								&format!("__self_{}", i),
								proc_macro2::Span::call_site(),
							);
							let other_id = syn::Ident::new(
								&format!("__other_{}", i),
								proc_macro2::Span::call_site(),
							);

							self_args.push(quote! { #id });
							other_args.push(quote! { #other_id });
						}

						(
							quote! { ( #(#self_args),* ) },
							quote! { ( #(#other_args),* ) },
						)
					}
					syn::Fields::Unit => (quote! {}, quote! {}),
				};

				let comparisons =
					fields_comparisons(v.fields.iter().zip(self_args.into_iter().zip(other_args)));

				quote! {
					(Self::#ident #self_pattern, Self::#ident #other_pattern) => {
						#comparisons
					}
				}
			});

			quote! {
				match (self, other) {
					#(#cases),*
					_ => false
				}
			}
		}
		syn::Data::Union(_) => {
			emit_error!(
				Span::call_site(),
				"`StrippedPartialEq` derive for unions is not supported"
			);
			quote! { false }
		}
	};

	quote! {
		impl #impl_generics ::locspan::StrippedPartialEq for #ident #ty_generics #where_clause {
			fn stripped_eq(&self, other: &Self) -> bool {
				#comparison
			}
		}
	}
	.into()
}

fn fields_comparisons<'a>(
	fields: impl 'a
		+ IntoIterator<
			Item = (
				&'a syn::Field,
				(proc_macro2::TokenStream, proc_macro2::TokenStream),
			),
		>,
) -> proc_macro2::TokenStream {
	let mut comparisons = proc_macro2::TokenStream::new();

	for (field, (self_path, other_path)) in fields {
		let mut stripped = false;
		let mut stripped_loc = false;

		for attr in &field.attrs {
			stripped |= attr.path.is_ident("stripped");
			stripped_loc |= attr.path.is_ident("stripped_loc");
		}

		if !comparisons.is_empty() {
			comparisons.extend(quote! { && })
		}

		if stripped {
			comparisons.extend(quote! { #self_path == #other_path })
		} else if stripped_loc {
			comparisons.extend(quote! { #self_path.value() == #other_path.value() })
		} else {
			comparisons.extend(quote! { #self_path.stripped_eq(&#other_path) })
		}
	}

	if comparisons.is_empty() {
		comparisons.extend(quote! { true })
	}

	comparisons
}
