# Shiranui Todo

## Overall

* Develop a crate containing both a library and a binary (the REPL)
* Goal is R7RS compliance in the form of a library and an accompanying REPL

## High Level

* Lexer
* Terms and Sequences
* Garbage collector
* Parser
* Basic interpreter
* Byte code compiler
* Stack-based byte code VM
* Hygienic macros
* Derived expressions (e.g. `case`, `let`, `do`)
* Standard procedures
* REPL

## Investigate

* Find out what `Fn` and `FnMut` are about
    * Appear in `std::ops`
* Complex number support in Rust?
    * Nope, but there is a crate that provides complex, bigint, and rational...
    * https://crates.io/crates/num
* Look at `std::collections` for available collections
    * Sequences: Vec, RingBuf, DList, BitV
    * Maps: HashMap, BTreeMap, VecMap
    * Sets: HashSet, BTreeSet, BitVSet
    * Misc: BinaryHeap
* Use `static` to declare the unchanging singleton objects (e.g. `theVoid`, `theEmptyList`)
    * But not the mutable evironment vars, such as `theNullEnvironment`
        * Unlikely that rustc would consider them immutable in this case
* Is there something like `defer` for Rust?
    * Implement the `Drop` trait for such resources in order to clean up
    * In addition, there is the `Finally` trait in `std::finally`
* Is there something like [gocheck](https://labix.org/gocheck) for Rust?
    * Something akin to RSpec: https://github.com/farcaller/shiny
    * With pattern matching it should be easy to write terse test code
        * `assert_eq!("4".parse::<u32>(), Some(4));`
        * `assert_eq!("j".parse::<u32>(), None);`
    * See also `Result` in `std::result` for `.ok()`
    * See also `Option` in `std::option` for `.expect()`

## Improvements to Make over Bakeneko

### Parser

* The result of parsing/expanding should already be wrapped with `(begin)`.
* Parsing and expanding should be combined into one step.
    * Fine to have private API for testing purposes, but public API should be a single `fn`.

## Environment

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
