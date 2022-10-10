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
use std::fmt;
use syn::{parse, parse_macro_input, punctuated::Punctuated, token, DeriveInput};

mod eq;
mod hash;
mod ord;
mod partial_eq;
mod partial_ord;

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

#[derive(Clone, Copy)]
enum ParamConfig {
	Ignore,
	Stripped,
}

fn read_params_config(attrs: Vec<syn::Attribute>) -> HashMap<proc_macro2::Ident, ParamConfig> {
	let mut params = HashMap::new();

	for attr in attrs {
		if attr.path.is_ident("stripped") {
			let tokens = attr.tokens.into();
			let idents: Idents = syn::parse(tokens).unwrap();
			for ident in idents.idents {
				params.insert(ident, ParamConfig::Stripped);
			}
		} else if attr.path.is_ident("stripped_ignore") {
			let tokens = attr.tokens.into();
			let idents: Idents = syn::parse(tokens).unwrap();
			for ident in idents.idents {
				params.insert(ident, ParamConfig::Ignore);
			}
		}
	}

	params
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

enum Method {
	Normal,
	Ignore,
	Stripped,
	DerefThenStripped,
	Deref2ThenStripped,
	UnwrapThenStripped,
	UnwrapThenDerefThenStripped,
	UnwrapThenDeref2ThenStripped,
}

fn read_method(attrs: &[syn::Attribute]) -> Method {
	let mut method = Method::Normal;

	for attr in attrs {
		if attr.path.is_ident("stripped") {
			method = Method::Stripped
		}

		if attr.path.is_ident("stripped_ignore") {
			method = Method::Ignore
		}

		if attr.path.is_ident("stripped_deref") {
			method = Method::DerefThenStripped
		}

		if attr.path.is_ident("stripped_deref2") {
			method = Method::Deref2ThenStripped
		}

		if attr.path.is_ident("stripped_option") {
			method = Method::UnwrapThenStripped
		}

		if attr.path.is_ident("stripped_option_deref") {
			method = Method::UnwrapThenDerefThenStripped
		}

		if attr.path.is_ident("stripped_option_deref2") {
			method = Method::UnwrapThenDeref2ThenStripped
		}
	}

	method
}

/// Returns an iterator over the fields and access methods for `self`.
fn fields_access(
	fields: &syn::Fields,
) -> impl std::iter::DoubleEndedIterator<Item = (&syn::Field, Access)> {
	fields.iter().enumerate().map(|(i, field)| {
		let id = match &field.ident {
			Some(ident) => quote! { #ident },
			None => {
				let index = syn::Index::from(i);
				quote! { #index }
			}
		};

		(field, Access::Direct(quote! { self.#id }))
	})
}

/// Returns an iterator over the fields and access methods for `self` and `other`.
fn fields_access_pairs(
	fields: &syn::Fields,
) -> impl std::iter::DoubleEndedIterator<Item = (&syn::Field, (Access, Access))> {
	fields.iter().enumerate().map(|(i, field)| {
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
	})
}

enum VariantArg<'a> {
	Named(&'a syn::Ident),
	Unnamed(usize),
}

impl<'a> fmt::Display for VariantArg<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Named(ident) => ident.fmt(f),
			Self::Unnamed(i) => i.fmt(f),
		}
	}
}

fn variant_pattern<B>(
	variant: &syn::Variant,
	binding: B,
) -> (proc_macro2::TokenStream, Vec<proc_macro2::TokenStream>)
where
	B: Fn(VariantArg) -> Option<syn::Ident>,
{
	let ident = &variant.ident;
	let args: Vec<_> = variant
		.fields
		.iter()
		.enumerate()
		.map(|(i, field)| {
			let arg = match field.ident.as_ref() {
				Some(ident) => VariantArg::Named(ident),
				None => VariantArg::Unnamed(i),
			};

			match binding(arg) {
				Some(ident) => quote! { #ident },
				None => quote! { _ },
			}
		})
		.collect();

	if args.is_empty() {
		(quote! { #ident }, args)
	} else {
		(quote! { #ident ( #(#args),* ) }, args)
	}
}

#[proc_macro_error]
#[proc_macro_derive(
	StrippedPartialEq,
	attributes(
		stripped,
		stripped_deref,
		stripped_deref2,
		stripped_option,
		stripped_option_deref,
		stripped_option_deref2,
		stripped_ignore
	)
)]
pub fn derive_stripped_partial_eq(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	match partial_eq::derive(input) {
		Ok(output) => output.into(),
		Err(e) => match e {
			partial_eq::Error::Union => {
				emit_error!(
					Span::call_site(),
					"`StrippedPartialEq` derive for unions is not supported"
				);
				quote! { false }.into()
			}
		},
	}
}

#[proc_macro_error]
#[proc_macro_derive(
	StrippedEq,
	attributes(
		stripped,
		stripped_deref,
		stripped_deref2,
		stripped_option,
		stripped_option_deref,
		stripped_option_deref2,
		stripped_ignore
	)
)]
pub fn derive_stripped_eq(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	eq::derive(input).into()
}

#[proc_macro_error]
#[proc_macro_derive(
	StrippedPartialOrd,
	attributes(
		stripped,
		stripped_deref,
		stripped_deref2,
		stripped_option,
		stripped_option_deref,
		stripped_option_deref2,
		stripped_ignore
	)
)]
pub fn derive_stripped_partial_ord(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	match partial_ord::derive(input) {
		Ok(output) => output.into(),
		Err(e) => match e {
			partial_ord::Error::Union => {
				emit_error!(
					Span::call_site(),
					"`StrippedPartialOrd` derive for unions is not supported"
				);
				quote! { false }.into()
			}
		},
	}
}

#[proc_macro_error]
#[proc_macro_derive(
	StrippedOrd,
	attributes(
		stripped,
		stripped_deref,
		stripped_deref2,
		stripped_option,
		stripped_option_deref,
		stripped_option_deref2,
		stripped_ignore
	)
)]
pub fn derive_stripped_ord(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	match ord::derive(input) {
		Ok(output) => output.into(),
		Err(e) => match e {
			ord::Error::Union => {
				emit_error!(
					Span::call_site(),
					"`StrippedOrd` derive for unions is not supported"
				);
				quote! { false }.into()
			}
		},
	}
}

#[proc_macro_error]
#[proc_macro_derive(
	StrippedHash,
	attributes(
		stripped,
		stripped_deref,
		stripped_deref2,
		stripped_option,
		stripped_option_deref,
		stripped_option_deref2,
		stripped_ignore
	)
)]
pub fn derive_stripped_hash(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	match hash::derive(input) {
		Ok(output) => output.into(),
		Err(e) => match e {
			hash::Error::Union => {
				emit_error!(
					Span::call_site(),
					"`StrippedHash` derive for unions is not supported"
				);
				quote! { false }.into()
			}
		},
	}
}
