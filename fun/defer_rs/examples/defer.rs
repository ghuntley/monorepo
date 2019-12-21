// Go's defer in Rust!

struct Defer<F: Fn()> {
    f: F
}

impl <F: Fn()> Drop for Defer<F> {
    fn drop(&mut self) {
        (self.f)()
    }
}

// Only added this for Go-syntax familiarity ;-)
fn  defer<F: Fn()>(f: F) -> Defer<F> {
    Defer { f }
}

fn main() {
    let mut i = 1;

    // Calling it "token" ... could be something else. The lifetime of this
    // controls when the action is run.
    let _token = defer(move || println!("Value is: {}", i));

    i += 1;
    println!("Value is: {}", i);
}

// Prints:
// Value is: 2
// Value is: 1
