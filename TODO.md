# Shiranui Todo

## Overall

* Develop a crate containing both a library and a binary (the REPL)
* Goal is R7RS compliance in the form of a library and an accompanying REPL

## Investigate

* Is there something like `defer` for Rust?
* Is there something like [gocheck](https://labix.org/gocheck) for Rust?

## Improvements to Make over Bakeneko

### Implementation

* The result of parsing/expanding should already be wrapped with `(begin)`.
* Parsing and expanding should be combined into one step.
    * Fine to have private API for testing purposes, but public API should be a single `fn`.
* Will want a left-leaning red-black tree implementation at some point.
    * https://github.com/stevej/rustled

### Data Types

* New Serializable trait will provide API for writing and reading elements in binary.
    * Individual things know how to serialize and deserialize themselves.
* New Runnable trait will provide a `run()` function.
    * Parsing an expression results in a Runnable that knows to invoke the interpreter.
    * Compiling an expression results in a Runnable that knows to invoke the VM.
* A Closure would be the combination of a Runnable and an Environment.
* New Arguments trait will provide behavior for lambda arguments.
    * Support proper lists, improper lists, as well as a single symbol
