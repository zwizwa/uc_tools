// Causal signal processing.
// Attempt to write the combinators in straight Rust.

// Is it possible to implement the causal feedback combinator
// directly?  Gut feeling is that the answer is no, because this needs
// hidden context.  But maybe it is possible to define signals in such
// a way that they can contain the feedback equation?

// The only way to do this is to make signals explicit and define
// combinators.

#[allow(dead_code)]
#[allow(unused_imports)]
use std::ops;

struct Sig<S: Clone, V> {
    init: S,
    next: Box<dyn Fn(&mut S) -> V>,
}

// Got it to type check up to lifetimes.  This is just too difficult
// to express and probably wrong in the first place.

// FIXME: I don't understand why I can't express this.

// impl<V: std::ops::Add<Output = V> + 'static, S1: Clone + 'static, S2: Clone + 'static>
//     ops::Add<Sig<S2, V>> for Sig<S1, V>
// {
//     type Output = Sig<(S1, S2), V>;
//     fn add(self, rhs: Sig<S2, V>) -> Sig<(S1, S2), V> {
//         Sig {
//             init: (self.init, rhs.init),
//             next: Box::new(|&mut s| {
//                 let v0 = (*self.next)(&mut s.0);
//                 let v1 = (*rhs.next)(&mut s.1);
//                 v0 + v1
//             }),
//         }
//     }
// }

pub fn test() {}
