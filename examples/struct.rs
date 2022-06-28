use locspan::{BorrowStripped, Loc, Location};
use locspan_derive::StrippedPartialEq;

#[derive(StrippedPartialEq, Debug)]
pub struct Unit;

#[derive(StrippedPartialEq, Debug)]
pub struct A<T: Clone> {
	a: Loc<T, (), ()>,
}

#[derive(StrippedPartialEq, Debug)]
pub struct B<T> {
	a: Loc<T, (), ()>,
	#[stripped]
	b: usize,
}

#[derive(StrippedPartialEq, Debug)]
pub struct Foo<T: Clone, #[stripped] S, #[stripped] P>(Loc<T, S, P>);

fn main() {
	let a = Foo(Loc(0u32, Location::new(0u32, ())));
	let b = Foo(Loc(0u32, Location::new(1u32, ())));

	assert_eq!(a.stripped(), b.stripped())
}
