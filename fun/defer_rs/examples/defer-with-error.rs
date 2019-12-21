// Go's defer in Rust, with error value return.

use std::rc::Rc;
use std::sync::RwLock;

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

// Convenience type synonym. This is a reference-counted smart pointer to
// a shareable, mutable variable.
// Rust does not allow willy-nilly mutation of shared variables, so explicit
// write-locking must be performed.
type ErrorHandle<T> = Rc<RwLock<Option<T>>>;

///////////////////
// Usage example //
///////////////////

#[derive(Debug)] // Debug trait for some default way to print the type.
enum Error { DropError }

fn main() {
    // Create a place to store the error.
    let drop_err: ErrorHandle<Error> = Default::default(); // create empty error

    // Introduce an arbitrary scope block (so that we still have control after
    // the defer runs):
    {
        let mut i = 1;

        // Rc types are safe to clone and share for multiple ownership.
        let err_handle = drop_err.clone();

        // Call defer and let the closure own the cloned handle to the error:
        let token = defer(move || {
            // do something!
            println!("Value is: {}", i);

            // ... oh no, it went wrong!
            *err_handle.write().unwrap() = Some(Error::DropError);
        });

        i += 1;
        println!("Value is: {}", i);

        // token goes out of scope here - drop() is called.
    }

    match *drop_err.read().unwrap() {
        Some(ref err) => println!("Oh no, an error occured: {:?}!", err),
        None => println!("Phew, everything went well.")
    };
}

// Prints:
// Value is: 2
// Value is: 1
// Oh no, an error occured: DropError!
