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
									ident: syn::Ident::new("hash", proc_macro2::Span::call_site()),
									arguments: syn::PathArguments::None,
								},
								syn::PathSegment {
									ident: syn::Ident::new("Hash", proc_macro2::Span::call_site()),
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
										"StrippedHash",
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

	let hash = match input.data {
		syn::Data::Struct(s) => fields_hash(crate::fields_access(&s.fields)),
		syn::Data::Enum(e) => {
			let cases = e.variants.iter().map(|v| {
				let (self_pattern, self_args) = crate::variant_pattern(v, |arg| {
					Some(syn::Ident::new(
						&format!("__self_{}", arg),
						proc_macro2::Span::call_site(),
					))
				});

				let hash = fields_hash(
					v.fields
						.iter()
						.zip(self_args.into_iter().map(Access::Reference)),
				);

				quote! {
					Self::#self_pattern => {
						#hash
					}
				}
			});

			quote! {
				match self {
					#(#cases),*
				}
			}
		}
		syn::Data::Union(_) => return Err(Error::Union),
	};

	Ok(quote! {
		impl #impl_generics ::locspan::StrippedHash for #ident #ty_generics #where_clause {
			fn stripped_hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
				#hash
			}
		}
	})
}

fn fields_hash<'a>(
	fields: impl 'a + IntoIterator<Item = (&'a syn::Field, Access)>,
) -> proc_macro2::TokenStream {
	let mut partial_ord = proc_macro2::TokenStream::new();

	for (field, path) in fields {
		if let Some(c) = field_hash(field, path) {
			if !partial_ord.is_empty() {
				partial_ord.extend(quote! { ; });
			}

			partial_ord.extend(c)
		}
	}

	if partial_ord.is_empty() {
		partial_ord.extend(quote! { 0xff.hash(state) })
	}

	partial_ord
}

fn field_hash(field: &syn::Field, path: Access) -> Option<proc_macro2::TokenStream> {
	let method = crate::read_method(&field.attrs);

	match method {
		Method::Ignore => None,
		Method::Normal => {
			let path = path.by_ref();
			Some(quote! { ::locspan::StrippedHash::stripped_hash(#path, state) })
		},
		Method::Stripped => {
			let path = path.by_ref();
			Some(quote! { ::core::hash::Hash::hash(#path, state) })
		},
		Method::DerefThenStripped => {
			let path = path.by_deref();
			Some(quote! { ::core::hash::Hash::hash(&#path, state) })
		}
		Method::UnwrapThenStripped => Some(quote! {
			match #path.as_ref() {
				Some(v) => ::core::hash::Hash::hash(&*v, state),
				None => ::core::hash::Hash::hash(&0xff, state)
			}
		}),
		Method::UnwrapThenDerefThenStripped => Some(quote! {
			match #path.as_ref() {
				Some(v) => ::core::hash::Hash::hash(&**v, state),
				None => ::core::hash::Hash::hash(&0xff, state)
			}
		}),
	}
}
