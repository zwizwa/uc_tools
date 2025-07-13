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

//trait Sig<V> {
//    fn update(&mut self) -> V;
//}
trait Proc<I, O> {
    fn update(&mut self, i: I) -> O;
}

struct Integrator<T: Copy> {
    state: T,
}
impl<T: Copy + std::ops::Add<T, Output = T>> Proc<T, T> for Integrator<T> {
    fn update(&mut self, i: T) -> T {
        let o = self.state;
        self.state = o + i;
        o
    }
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

pub fn test() -> i32 {
    let mut x = Integrator { state: 0 };
    let y1 = x.update(1);
    let y2 = x.update(2);
    y1 + y2
}
