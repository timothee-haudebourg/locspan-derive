use locspan::{BorrowStripped, Loc, Location};
use locspan_derive::*;

#[derive(StrippedPartialEq, Debug)]
pub struct Unit;

#[derive(StrippedPartialEq, Debug)]
pub struct A<T: Clone> {
	a: Loc<T, (), ()>,
}

#[derive(StrippedPartialEq, StrippedPartialOrd, Debug)]
pub struct B<T> {
	a: Loc<T, (), ()>,
	#[stripped]
	b: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Hash, Debug)]
pub struct Thing<T>(T);

#[derive(StrippedPartialEq, StrippedEq, StrippedPartialOrd, StrippedHash, Debug)]
#[stripped(T)]
pub struct C<T> {
	#[stripped_option_deref]
	a: Option<Loc<Thing<T>, u32, ()>>,
}

#[derive(StrippedPartialEq, Debug)]
#[stripped_ignore(S, P)]
pub struct Foo<T: Clone, S, P>(Loc<T, S, P>);

fn main() {
	let a = C {
		a: Some(Loc(Thing(0u32), Location::new(0u32, ()))),
	};
	let b = C {
		a: Some(Loc(Thing(0u32), Location::new(1u32, ()))),
	};

	assert_eq!(a.stripped(), b.stripped());

	let a = Foo(Loc(0u32, Location::new(0u32, ())));
	let b = Foo(Loc(0u32, Location::new(1u32, ())));

	assert_eq!(a.stripped(), b.stripped())
}
