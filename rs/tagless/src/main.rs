/*

How to tackle this?

Ideas:
- Focus on data representation: Lua and Rust data structures should be equivalent
- Gradually replace functions and data structures with Rust code

Notes:
- https://crates.io/crates/mlua
- https://github.com/mlua-rs/mlua/blob/main/examples/module/src/lib.rs

Questions:
- How to expose as a Lua library?
- How to map Lua data structures to Rust data structures?


Something to start with:
- try to formalize all the name mangling variations
- wrap it in a Lua plugin so it can be integrated
- find a way to translate the Lua tables


Another idea is to completely fix the api: only the patch level is
lua, all the rest is Rust.  This would allow type checking already at
that level, as bad types are not representable in the rust side.  But
it would need userdata. https://docs.rs/mlua/latest/mlua/#custom-userdata


EDIT: I am starting directly with the "user interface", which is the
dsl exposed to lua.


Note: this is by itself ... because the state is completely abstract
and just passed around as a mut reference, and node references can
just be copied, i.e. no borrow nonsense.


2025-07-11 Some recent ideas:

- Tagless final GATs
  https://getcode.substack.com/p/efficient-extensible-expressive-typed

- State and params really do not show up in the types, they are always
  hidden to the inner composition mechanism so they are purely a
  compiler run-time thing, exposed as a compilation result (state
  struct, state init, param init and param value delivery code) next
  to the update code.



*/
#![allow(unused)]

// Tagless final
// https://getcode.substack.com/p/efficient-extensible-expressive-typed
type Fun<A, B> = Box<dyn Fn(A) -> B>;

// Base language trait.
trait Expr {
    // Trait is parameterized by representation type R, which is
    // itself parameterized by any Rust type T.
    type R<T>;

    fn int(i: i32) -> Self::R<i32>;
    fn add(a: &Self::R<i32>, b: &Self::R<i32>) -> Self::R<i32>;
    fn lam<A, B, F: Fn(Self::R<A>) -> Self::R<B>>(f: F) -> Self::R<Fun<A, B>>
    where
        // higher rank trait bound -- https://doc.rust-lang.org/nomicon/hrtb.html
        for<'a> F: 'a;
    fn app<F: Fn(A) -> B, A, B>(f: Self::R<F>, arg: Self::R<A>) -> Self::R<B>;
}

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

// Surface syntax via Rust macros.

struct Eval;
impl Expr for Eval {
    type R<T> = T;
    fn int(i: i32) -> Self::R<i32> {
        i
    }
    fn add(a: &Self::R<i32>, b: &Self::R<i32>) -> Self::R<i32> {
        a + b
    }
    fn lam<A, B, F: Fn(Self::R<A>) -> Self::R<B>>(f: F) -> Self::R<Fun<A, B>>
    where
        for<'a> F: 'a,
    {
        Box::new(f)
    }
    fn app<F: Fn(A) -> B, A, B>(f: Self::R<F>, arg: Self::R<A>) -> Self::R<B> {
        f(arg)
    }
}
fn main() {
    // Your program will start here.
    println!("Hello world!");
}
