/*
Tagless final DSL.
*/

#![allow(unused)]

extern crate tagless;
use tagless::{Eval, Expr, Fun};

// Expressions in terms of base language.

// Note that it seems necessary to wrap expressions in a thunk (static
// items may not have generic parameters).  The <E: Expr> means the
// type variable E is constrained by having a trait Expr
// implementation.
//
// This is pretty annoying.  How to write libraries of functions in
// the DSL?  They all need to have thunk wrappers.
//
// Maybe they need to be wrapped in a trait as well?

fn ex1<E: Expr>() -> E::R<i32> {
    E::int(123)
}
fn ex2<E: Expr>() -> E::R<Fun<i32, i32>> {
    E::lam(|x| E::add(&x, &x))
}
fn ex3<E: Expr>() -> E::R<i32> {
    let a = E::int(123);
    let f = E::lam(|x| E::add(&x, &x));
    E::app(f, a)
}
fn ex4<E: Expr>() -> E::R<i32> {
    // The type annotation here is necessary.
    let f = ex2::<E>();
    let a = ex1::<E>();
    E::app(f, a)
}
fn ex5<E: Expr>() -> E::R<i32> {
    E::app(ex2::<E>(), E::app(ex2::<E>(), ex1::<E>()))
}

// This doesn't work: can't reuse the represented function.  All in
// all this seems to be too cumbersome.  Might work as a compilation
// target though but I do not see the point of it atm.

//fn ex5<E: Expr>() -> E::R<i32> {
//    let f = tagless::double::<E>();
//    let a = ex1::<E>();
//    E::app(f, E::app(f, a))
//}

fn main() {
    // Your program will start here.
    println!("{}", ex4::<Eval>());
}
