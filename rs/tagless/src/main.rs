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

fn main() {
    // Your program will start here.
    println!("{}", ex4::<Eval>());
}
