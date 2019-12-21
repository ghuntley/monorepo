defer in Rust
=============

After a Hacker News discussion about implementing Go's `defer` keyword in C++,
I stumbled upon [this comment](https://news.ycombinator.com/item?id=15523589)
and more specifically this response to it by "Occivink":

> There's plenty of one-time cases where you don't want to declare an entire
> class but still enjoy scope-based functions.

Specificall the "don't want to declare an entire class" suggests that languages
like C++ have high friction for explaining your desired invariant (cleanup is
run when `$thing` is destroyed) to the compiler.

It seems like most languages either hand-wave this away (*cough* Java *cough*)
or use what seems like a workaround (`defer`).

Rust has the so-called `Drop` trait, which is a typeclass that contains a single
method with no return value that is run when a variable is dropped (i.e. goes out
of scope).

This works fine for most general cases - i.e. closing file handlers - but can
get complicated if other use-cases of `defer` are considered:

* returning an error-value by mutating a reference in the enclosing scope (oh boy)
* deferring a decision about when/whether to run cleanup to the caller

While thinking about how to do this with the `Drop` trait I realised that `defer`
can actually be trivially implemented in Rust, using `Drop`.

A simple implementation of `defer` can be seen in [defer.rs](examples/defer.rs),
an implementation using shared mutable state for error returns is in the file
[defer-with-error.rs](examples/defer-with-error.rs) and an implementation that
allows cleanup to be *cancelled* (don't _actually_ do this, it leaks a pointer)
is in [undefer.rs](examples/undefer.rs).

Whether any of this is actually useful is not up to me to decide. I haven't
actually had a real-life need for this.

You can run the examples with `cargo run --example defer`, etc.

## Notes

* `Drop` is not guaranteed to run in case of panics or program aborts, if you
  need support for that check out [scopeguard](https://github.com/bluss/scopeguard)
* `undefer` could be implemented safely by, for example, carrying a boolean that
  by default causes execution to happen but can be flipped to disable it

## Further reading:

* [The Pain Of Real Linear Types in Rust](https://gankro.github.io/blah/linear-rust/)
* [Go's defer](https://tour.golang.org/flowcontrol/12)
* [Rust's Drop](https://doc.rust-lang.org/std/ops/trait.Drop.html)
