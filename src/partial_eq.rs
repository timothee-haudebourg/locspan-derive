use crate::{Access, Method, ParamConfig};
use proc_macro2::TokenStream;
use quote::quote;

pub enum Error {
	Union,
}

pub fn derive(input: syn::DeriveInput) -> Result<TokenStream, Error> {
	let ident = input.ident;
	let mut generics = input.generics;
	let params = crate::read_params_config(input.attrs);

	for p in generics.params.iter_mut() {
		if let syn::GenericParam::Type(ty) = p {
			match params.get(&ty.ident) {
				Some(ParamConfig::Ignore) => (),
				Some(ParamConfig::Stripped) => {
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
		syn::Data::Struct(s) => fields_comparisons(crate::fields_access_pairs(&s.fields)),
		syn::Data::Enum(e) => {
			let cases = e.variants.iter().map(|v| {
				let (self_pattern, self_args) = crate::variant_pattern(v, |arg| {
					Some(syn::Ident::new(
						&format!("__self_{}", arg),
						proc_macro2::Span::call_site(),
					))
				});

				let (other_pattern, other_args) = crate::variant_pattern(v, |arg| {
					Some(syn::Ident::new(
						&format!("__other_{}", arg),
						proc_macro2::Span::call_site(),
					))
				});

				let comparisons =
					fields_comparisons(v.fields.iter().zip(
						self_args
							.into_iter()
							.map(Access::Reference)
							.zip(other_args.into_iter().map(Access::Reference)),
					));

				quote! {
					(Self::#self_pattern, Self::#other_pattern) => {
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
		syn::Data::Union(_) => return Err(Error::Union),
	};

	Ok(quote! {
		impl #impl_generics ::locspan::StrippedPartialEq for #ident #ty_generics #where_clause {
			fn stripped_eq(&self, other: &Self) -> bool {
				#comparison
			}
		}
	})
}

fn fields_comparisons<'a>(
	fields: impl 'a + IntoIterator<Item = (&'a syn::Field, (Access, Access))>,
) -> proc_macro2::TokenStream {
	let mut comparisons = proc_macro2::TokenStream::new();

	for (field, (self_path, other_path)) in fields {
		if let Some(c) = field_comparisons(field, self_path, other_path) {
			if !comparisons.is_empty() {
				comparisons.extend(quote! { && })
			}

			comparisons.extend(c)
		}
	}

	if comparisons.is_empty() {
		comparisons.extend(quote! { true })
	}

	comparisons
}

fn field_comparisons(
	field: &syn::Field,
	self_path: Access,
	other_path: Access,
) -> Option<proc_macro2::TokenStream> {
	let method = crate::read_method(&field.attrs);

	match method {
		Method::Ignore => None,
		Method::Normal => {
			let other_path = other_path.by_ref();
			Some(quote! { #self_path.stripped_eq(#other_path) })
		}
		Method::Stripped => Some(quote! { #self_path == #other_path }),
		Method::DerefThenStripped => {
			let self_path = self_path.by_deref();
			let other_path = other_path.by_deref();
			Some(quote! { #self_path == #other_path })
		}
		Method::UnwrapThenStripped => Some(
			quote! { #self_path.as_ref().zip(#other_path.as_ref()).map(|(a, b)| *a == *b).unwrap_or(true) },
		),
		Method::UnwrapThenDerefThenStripped => Some(
			quote! { #self_path.as_ref().zip(#other_path.as_ref()).map(|(a, b)| **a == **b).unwrap_or(true) },
		),
	}
}
