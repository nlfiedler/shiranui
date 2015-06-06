# Shiranui Todo

## Overall

* Develop a crate containing both a library and a binary (the REPL).
* Goal is R7RS compliance in the form of a library and an accompanying REPL.

## Road Map

* Look through lexer code for explicit case analysis and see if `Option` combinators can be used to simplify the code [^2]
    - e.g. `map()`, `or()`, `and_then()`
* Garbage collector
    - See https://github.com/fitzgen/rajan-bacon-cc
    - See https://github.com/Manishearth/rust-gc
* Pairs, vectors, byte vectors
    - No use creating a "sequence" type
    - Only lists are allowed when calling functions
    - Most of the bakeneko code was special casing the pairs and vectors anyway
    - Consider `#[derive(Hash)]` for an object identity hash, to help with loop detection
* Environment: borrow from oxischeme
    - Environment is used during syntactic analysis (`HashMap` is sufficient)
    - Compiles into an activation for faster execution (uses a `Vector`)
        + Matt Might seems to call the activation a "store" [^1]
* Parser
    - Parser produces tuples of location and expression
    - Interpreter/Compiler can then use location in error reporting
    - Define a type for the tuple to make it easier to deal with
* Basic interpreter
* Byte code compiler
* Stack-based byte code VM
* Complex numbers (see crate https://crates.io/crates/num)
* Rational numbers (see crate https://crates.io/crates/num)
* Big integers (see crate https://crates.io/crates/num)
* Hygienic macros
* Derived expressions (e.g. `case`, `let`, `do`)
* Standard procedures
* REPL
    - Readline support: https://github.com/gwenn/rust-readline

## Design

### Garbage collection

* Define a type that is a reference to objects on the heap
    - Implement the `std::ops::Deref` trait to dereference the "pointer"
    - Similar to the `ArenaPtr` type in oxischeme
* Having a level of indirection allows for a compacting collector
* Having a compacting collector allows for a generational collector
* Generational collectors tend to have shorter pause times
* Model collection strategy after Ruby
    - An "object space" that contains the collection of heaps
        + Oxischeme calls this the "heap"
        + Oxischeme's heap contains "arenas"
        + Oxischeme's arenas are equivalent to Ruby heap pages
    - "Eden" heap is a list of heap pages with some objects in them
    - "Tomb" heap is a list of heap pages with no objects in them
    - Each heap page has a free list
    - Each heap page has its own set of mark bits
* Oxischeme design
    - `Value` is an enum of Scheme values
    - Simple things are encoded in the `Value` itself (e.g. characters)
    - Heap allocated things (e.g. strings) have a "pointer" to an arena thing
    - `ArenaPtr` forms the basis of all references in oxischeme
        + Holds mutable reference to the arena
        + Has index into arena pool
        + Arena has a pool of objects (vector)
        + Implements `Deref` to acquire pointer to element in arena pool
* Define a `Value` type that wraps a `ValueType` enum in a reference
    - Model after the data types in `mal/rust/types.env`

## Improvements to Make over Bakeneko

### Testing

* Try https://github.com/farcaller/shiny (akin to RSpec)
* Write many of the tests in Scheme itself
    - see https://code.google.com/p/chibi-scheme/
    - see https://github.com/fitzgen/oxischeme
    - Look for a Scheme testing framework

### Parser

* The result of parsing/expanding should already be wrapped with `(begin)`.
* Parsing and expanding should be combined into one step.
    - Fine to have private API for testing purposes, but public API should be a single `fn`.

### Data Types

* Environment, used for syntactic analysis only.
    - Compiled into an "activation" which is simply a vector of values (see oxischeme).
* New Serializable trait will provide API for writing and reading elements in binary.
    - Individual things know how to serialize and deserialize themselves.
* New Runnable trait will provide a `run()` function.
    - Parsing an expression results in a Runnable that knows to invoke the interpreter.
    - Compiling an expression results in a Runnable that knows to invoke the VM.
* A Closure would be the combination of a Runnable and an Environment.
* New Arguments trait will provide behavior for lambda arguments.
    - Support proper lists, improper lists, as well as a single symbol.

[^1]: http://matt.might.net/articles/cesk-machines/
[^2]: http://blog.rust-lang.org/2015/05/11/traits.html
