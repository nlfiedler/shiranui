# Shiranui Todo

## Overall

* Develop a crate containing both a library and a binary (the REPL)
* Goal is R7RS compliance in the form of a library and an accompanying REPL

## Road Map

* Garbage collector
* Pairs, vectors, byte vectors
    * No use creating a "sequence" type
    * Only lists are allowed when calling functions
    * Most of bakeneko was special casing the pairs and vectors anyway
    * Consider #[derive(Hash)] for an object identity hash, to help with loop detection
* Parser
    * Parser produces tuples of location and expression
    * Interpreter/Compiler can then use location in error reporting
    * Define a type for the tuple to make it easier to deal with
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

## Improvements to Make over Bakeneko

### Testing

* Try https://github.com/farcaller/shiny (akin to RSpec)
* Write many of the tests in Scheme itself
    * see https://code.google.com/p/chibi-scheme/
    * see https://github.com/fitzgen/oxischeme
    * Look for a Scheme testing framework

### Parser

* The result of parsing/expanding should already be wrapped with `(begin)`.
* Parsing and expanding should be combined into one step.
    * Fine to have private API for testing purposes, but public API should be a single `fn`.

### Data Types

* Environment, sometimes referred to elsewhere as an "activation"
    * Use the std::collections::BTreeMap to map symbols to values
* New Serializable trait will provide API for writing and reading elements in binary.
    * Individual things know how to serialize and deserialize themselves.
* New Runnable trait will provide a `run()` function.
    * Parsing an expression results in a Runnable that knows to invoke the interpreter.
    * Compiling an expression results in a Runnable that knows to invoke the VM.
* A Closure would be the combination of a Runnable and an Environment.
* New Arguments trait will provide behavior for lambda arguments.
    * Support proper lists, improper lists, as well as a single symbol
