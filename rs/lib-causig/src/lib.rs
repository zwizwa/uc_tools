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

// Some design considerations:
//
// * It's best to define inputs as immutable references to allow
// reuse, and state as a mutable object with an update method for
// in-place update.
//
// * Processors are the base abstraction, not signals because it
// doesn't seem possible to do the lambda trick.

trait Proc<I, O> {
    fn update(&mut self, i: I) -> O;
}
struct Integrator<T: Copy> {
    state: T,
}
impl<T: Copy + for<'a> std::ops::Add<&'a T, Output = T>> Proc<&T, T> for Integrator<T> {
    fn update(&mut self, i: &T) -> T {
        let o = self.state;
        self.state = o + i;
        o
    }
}

pub fn test() -> i32 {
    let mut x = Integrator { state: 0 };
    let y1 = x.update(&1);
    let y2 = x.update(&2);
    y1 + y2
}
