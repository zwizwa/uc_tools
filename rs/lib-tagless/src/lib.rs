// Tagless final
// This started from this seed:
// https://getcode.substack.com/p/efficient-extensible-expressive-typed
//
// I don't find the Rust embedding particularly pleasant to use, but
// it is a working embedding, and I need both more experience writing
// Rust code plus an embedding in an existing type system to avoid
// creating inconsistent abstractions.  So let's bear with it for a
// while, because I am really stuck.
//
// Start with the most important part: the 'close' combinator, which
// converts a state update equation and an initial state into a causal
// signal.  This can be given a clean semantics by interpreting values
// as infinite sequences.  It seems best to develop the compilation
// and evaluation interpretations at the same time, because the
// expressibility of the evaluation semantics will give a good
// consistency check.

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

    fn close<F: Fn(A) -> B, Fs: Fn(S, A) -> (S, B), A, B, S>(f: Self::R<Fs>) -> Self::R<F>;
}

pub fn double<E: Expr>() -> E::R<Fun<i32, i32>> {
    E::lam(|x| E::add(&x, &x))
}

// Implementation: evaluator
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
    fn close<F: Fn(A) -> B, Fs: Fn(S, A) -> (S, B), A, B, S>(f: Self::R<Fs>) -> Self::R<F> {
        lam(|a|
}

}

// Implementation: compiler.
