// Tagless final
// https://getcode.substack.com/p/efficient-extensible-expressive-typed
pub type Fun<A, B> = Box<dyn Fn(A) -> B>;

// Base language trait.
pub trait Expr {
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

pub fn double<E: Expr>() -> E::R<Fun<i32, i32>> {
    E::lam(|x| E::add(&x, &x))
}

// Surface syntax via Rust macros.

pub struct Eval;
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
