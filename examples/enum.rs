use locspan::{BorrowStripped, Loc, Location};
use locspan_derive::{
	StrippedEq, StrippedHash, StrippedOrd, StrippedPartialEq, StrippedPartialOrd,
};

#[derive(
	PartialEq, StrippedPartialEq, StrippedEq, StrippedPartialOrd, StrippedOrd, StrippedHash, Debug,
)]
#[stripped_ignore(S, P)]
pub enum Foo<S, P> {
	A(#[stripped_deref] Loc<usize, S, P>),
	B(#[stripped] char, #[stripped] usize),
	C(#[stripped_deref] Loc<usize, S, P>, #[stripped] char, u8),
}

fn main() {
	let a = Foo::A(Loc(0, Location::new(0, ())));
	let b = Foo::A(Loc(0, Location::new(1, ())));

	assert_eq!(a.stripped(), b.stripped())
}
