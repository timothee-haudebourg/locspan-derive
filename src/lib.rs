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
//! // with `#[stripped_ignore]`.
//! #[derive(StrippedPartialEq)]
//! #[stripped_ignore(S, P)]
//! struct Foo<T, S, P> {
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
use std::collections::HashMap;
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

enum Access {
	Direct(proc_macro2::TokenStream),
	Reference(proc_macro2::TokenStream),
}

impl Access {
	pub fn by_ref(&self) -> ByRef {
		ByRef(self)
	}

	pub fn by_deref(&self) -> ByDeref {
		ByDeref(self)
	}
}

impl quote::ToTokens for Access {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		match self {
			Access::Direct(t) => t.to_tokens(tokens),
			Access::Reference(t) => t.to_tokens(tokens),
		}
	}
}

struct ByRef<'a>(&'a Access);

impl<'a> quote::ToTokens for ByRef<'a> {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		match &self.0 {
			Access::Direct(t) => {
				tokens.extend(quote! { & });
				t.to_tokens(tokens)
			}
			Access::Reference(t) => t.to_tokens(tokens),
		}
	}
}

struct ByDeref<'a>(&'a Access);

impl<'a> quote::ToTokens for ByDeref<'a> {
	fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
		match &self.0 {
			Access::Direct(t) => {
				tokens.extend(quote! { * });
				t.to_tokens(tokens)
			}
			Access::Reference(t) => {
				tokens.extend(quote! { ** });
				t.to_tokens(tokens)
			}
		}
	}
}

#[proc_macro_error]
#[proc_macro_derive(
	StrippedPartialEq,
	attributes(
		stripped,
		stripped_deref,
		stripped_option,
		stripped_option_deref,
		stripped_ignore
	)
)]
pub fn derive_stripped_partial_eq(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);

	let ident = input.ident;
	let mut generics = input.generics;

	#[derive(Clone, Copy)]
	pub enum ParamConfig {
		Ignore,
		PartialEq,
	}

	let mut params = HashMap::new();
	for attr in input.attrs {
		if attr.path.is_ident("stripped") {
			let tokens = attr.tokens.into();
			let idents = parse_macro_input!(tokens as Idents);
			for ident in idents.idents {
				params.insert(ident, ParamConfig::PartialEq);
			}
		} else if attr.path.is_ident("stripped_ignore") {
			let tokens = attr.tokens.into();
			let idents = parse_macro_input!(tokens as Idents);
			for ident in idents.idents {
				params.insert(ident, ParamConfig::Ignore);
			}
		}
	}

	for p in generics.params.iter_mut() {
		if let syn::GenericParam::Type(ty) = p {
			match params.get(&ty.ident) {
				Some(ParamConfig::Ignore) => (),
				Some(ParamConfig::PartialEq) => {
					ty.bounds.push(syn::TypeParamBound::Trait(syn::TraitBound {
						paren_token: None,
						modifier: syn::TraitBoundModifier::None,
						lifetimes: None,
						path: syn::Path {
							leading_colon: Some(syn::token::Colon2::default()),
							segments: [
								syn::PathSegment {
									ident: syn::Ident::new("core", proc_macro2::Span::call_site()),
									arguments: syn::PathArguments::None,
								},
								syn::PathSegment {
									ident: syn::Ident::new("cmp", proc_macro2::Span::call_site()),
									arguments: syn::PathArguments::None,
								},
								syn::PathSegment {
									ident: syn::Ident::new(
										"PartialEq",
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
				None => {
					ty.bounds.push(syn::TypeParamBound::Trait(syn::TraitBound {
						paren_token: None,
						modifier: syn::TraitBoundModifier::None,
						lifetimes: None,
						path: syn::Path {
							leading_colon: Some(syn::token::Colon2::default()),
							segments: [
								syn::PathSegment {
									ident: syn::Ident::new(
										"locspan",
										proc_macro2::Span::call_site(),
									),
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

				(
					field,
					(
						Access::Direct(quote! { self.#id }),
						Access::Direct(quote! { other.#id }),
					),
				)
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
							self_args.push(Access::Reference(quote! { #self_ident }));
							other_args.push(Access::Reference(quote! { #other_ident }));

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

							self_args.push(Access::Reference(quote! { #id }));
							other_args.push(Access::Reference(quote! { #other_id }));
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
	fields: impl 'a + IntoIterator<Item = (&'a syn::Field, (Access, Access))>,
) -> proc_macro2::TokenStream {
	let mut comparisons = proc_macro2::TokenStream::new();

	for (field, (self_path, other_path)) in fields {
		pub enum Method {
			Ignore,
			StrippedPartialEq,
			PartialEq,
			DerefThenPartialEq,
			UnwrapThenPartialEq,
			UnwrapThenDerefThenPartialEq,
		}

		let mut method = Method::StrippedPartialEq;

		for attr in &field.attrs {
			if attr.path.is_ident("stripped") {
				method = Method::PartialEq
			}

			if attr.path.is_ident("stripped_ignore") {
				method = Method::Ignore
			}

			if attr.path.is_ident("stripped_deref") {
				method = Method::DerefThenPartialEq
			}

			if attr.path.is_ident("stripped_option") {
				method = Method::UnwrapThenPartialEq
			}

			if attr.path.is_ident("stripped_option_deref") {
				method = Method::UnwrapThenDerefThenPartialEq
			}
		}

		if !matches!(method, Method::Ignore) && !comparisons.is_empty() {
			comparisons.extend(quote! { && })
		}

		match method {
			Method::Ignore => (),
			Method::StrippedPartialEq => {
				let other_path = other_path.by_ref();
				comparisons.extend(quote! { #self_path.stripped_eq(#other_path) })
			}
			Method::PartialEq => comparisons.extend(quote! { #self_path == #other_path }),
			Method::DerefThenPartialEq => {
				let self_path = self_path.by_deref();
				let other_path = other_path.by_deref();
				comparisons.extend(quote! { #self_path == #other_path })
			}
			Method::UnwrapThenPartialEq => comparisons.extend(
				quote! { #self_path.as_ref().zip(#other_path.as_ref()).map(|(a, b)| *a == *b).unwrap_or(true) },
			),
			Method::UnwrapThenDerefThenPartialEq => comparisons.extend(
				quote! { #self_path.as_ref().zip(#other_path.as_ref()).map(|(a, b)| **a == **b).unwrap_or(true) },
			),
		}
	}

	if comparisons.is_empty() {
		comparisons.extend(quote! { true })
	}

	comparisons
}
