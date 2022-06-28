use locspan::{BorrowStripped, Loc, Location};
use locspan_derive::StrippedPartialEq;

#[derive(PartialEq, StrippedPartialEq, Debug)]
#[stripped(S, P)]
pub enum Foo<S, P> {
	A(#[stripped_loc] Loc<usize, S, P>),
	B(#[stripped] char, #[stripped] usize),
}

fn main() {
	let a = Foo::A(Loc(0, Location::new(0, ())));
	let b = Foo::A(Loc(0, Location::new(1, ())));

	assert_eq!(a.stripped(), b.stripped())
}
