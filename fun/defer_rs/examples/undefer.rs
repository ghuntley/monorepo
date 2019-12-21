// Go's defer in Rust, with a little twist!

struct Defer<F: Fn()> {
    f: F
}

impl <F: Fn()> Drop for Defer<F> {
    fn drop(&mut self) {
        (self.f)()
    }
}

// Only added this for Go-syntax familiarity ;-)
fn defer<F: Fn()>(f: F) -> Defer<F> {
    Defer { f }
}

// Changed your mind about the defer?
// (Note: This leaks the closure! Don't actually do this!)
fn undefer<F: Fn()>(token: Defer<F>) {
    use std::mem;
    mem::forget(token);
}

fn main() {
    let mut i = 1;

    // Calling it "token" ... could be something else. The lifetime of this
    // controls when the action is run.
    let token = defer(move || println!("Value is: {}", i));

    i += 1;
    println!("Value is: {}", i);

    // Oh, now I changed my mind about the previous defer:
    undefer(token);
}

// Prints:
// Value is: 2
