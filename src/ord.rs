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
									ident: syn::Ident::new("Ord", proc_macro2::Span::call_site()),
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
										"StrippedOrd",
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

	let ordering = match input.data {
		syn::Data::Struct(s) => fields_ord(crate::fields_access_pairs(&s.fields)),
		syn::Data::Enum(e) => {
			let mut cases = Vec::new();

			let variants: Vec<_> = e.variants.into_iter().collect();
			for (i, variant) in variants.iter().enumerate() {
				let (self_pattern, self_args) = crate::variant_pattern(variant, |arg| {
					Some(syn::Ident::new(
						&format!("__self_{}", arg),
						proc_macro2::Span::call_site(),
					))
				});

				let (other_pattern, other_args) = crate::variant_pattern(variant, |arg| {
					Some(syn::Ident::new(
						&format!("__other_{}", arg),
						proc_macro2::Span::call_site(),
					))
				});

				let less_patterns: Vec<_> = variants[..i]
					.iter()
					.map(|v| crate::variant_pattern(v, |_| None).0)
					.collect();
				let greater_patterns: Vec<_> = variants[(i + 1)..]
					.iter()
					.map(|v| crate::variant_pattern(v, |_| None).0)
					.collect();

				let partial_ord = fields_ord(
					variant.fields.iter().zip(
						self_args
							.into_iter()
							.map(Access::Reference)
							.zip(other_args.into_iter().map(Access::Reference)),
					),
				);

				if !less_patterns.is_empty() {
					cases.push(quote! {
						(Self::#self_pattern, #(Self::#less_patterns)|*) => ::core::cmp::Ordering::Greater
					});
				}

				cases.push(quote! {
					(Self::#self_pattern, Self::#other_pattern) => {
						#partial_ord
					}
				});

				if !greater_patterns.is_empty() {
					cases.push(quote! {
						(Self::#self_pattern, #(Self::#greater_patterns)|*) => ::core::cmp::Ordering::Less
					});
				}
			}

			quote! {
				match (self, other) {
					#(#cases),*
				}
			}
		}
		syn::Data::Union(_) => return Err(Error::Union),
	};

	Ok(quote! {
		impl #impl_generics ::locspan::StrippedOrd for #ident #ty_generics #where_clause {
			fn stripped_cmp(&self, other: &Self) -> ::core::cmp::Ordering {
				#ordering
			}
		}
	})
}

fn fields_ord<'a>(
	fields: impl 'a + IntoIterator<Item = (&'a syn::Field, (Access, Access))>,
) -> proc_macro2::TokenStream {
	let mut partial_ord = proc_macro2::TokenStream::new();

	for (field, (self_path, other_path)) in fields {
		if let Some(c) = field_ord(field, self_path, other_path) {
			if partial_ord.is_empty() {
				partial_ord = c
			} else {
				partial_ord = quote! {
					#partial_ord.then_with(|| #c)
				}
			}
		}
	}

	if partial_ord.is_empty() {
		partial_ord.extend(quote! { ::core::cmp::Ordering::Equal })
	}

	partial_ord
}

fn field_ord(
	field: &syn::Field,
	self_path: Access,
	other_path: Access,
) -> Option<proc_macro2::TokenStream> {
	let method = crate::read_method(&field.attrs);

	match method {
		Method::Ignore => None,
		Method::Normal => {
			let self_path = self_path.by_ref();
			let other_path = other_path.by_ref();
			Some(quote! { ::locspan::StrippedOrd::stripped_cmp(#self_path, #other_path) })
		}
		Method::Stripped => {
			let self_path = self_path.by_ref();
			let other_path = other_path.by_ref();
			Some(quote! { ::core::cmp::Ord::cmp(#self_path, #other_path) })
		}
		Method::DerefThenStripped => {
			let self_path = self_path.by_deref();
			let other_path = other_path.by_deref();
			Some(quote! { ::core::cmp::Ord::cmp(&#self_path, &#other_path) })
		}
		Method::Deref2ThenStripped => {
			let self_path = self_path.by_deref();
			let other_path = other_path.by_deref();
			Some(quote! { ::core::cmp::Ord::cmp(&*#self_path, &*#other_path) })
		}
		Method::UnwrapThenStripped => Some(
			quote! { #self_path.as_ref().zip(#other_path.as_ref()).map(|(a, b)| ::core::cmp::Ord::cmp(&*a, &*b)).unwrap_or(::core::cmp::Ordering::Equal) },
		),
		Method::UnwrapThenDerefThenStripped => Some(
			quote! { #self_path.as_ref().zip(#other_path.as_ref()).map(|(a, b)| ::core::cmp::Ord::cmp(&**a, &**b)).unwrap_or(::core::cmp::Ordering::Equal) },
		),
		Method::UnwrapThenDeref2ThenStripped => Some(
			quote! { #self_path.as_ref().zip(#other_path.as_ref()).map(|(a, b)| ::core::cmp::Ord::cmp(&***a, &***b)).unwrap_or(::core::cmp::Ordering::Equal) },
		),
	}
}
