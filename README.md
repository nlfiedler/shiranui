## An incomplete Scheme R7RS interpreter in Rust

Eventually this will be a complete [Scheme](http://scheme-reports.org) interpreter written in the [Rust](http://rust-lang.org) programming language. The current target is Scheme R7RS. There is much work to be done, touched on briefly in the TODO section below.

### TODO

* Garbage collector
* Parser
* Basic interpreter
* Byte code compiler
* Stack-based byte code VM
* Hygienic macros
* Derived expressions (e.g. `case`, `let`, `do`)
* Standard procedures
* REPL

See the `TODO.md` file for additional details.

### About the name

To be completely honest, I had in mind the legendary [creature](http://en.wikipedia.org/wiki/Shiranui_(optical_phenomenon)) of Japan, rather than the fictional female fighter, Mai Shiranui. Several of my projects are named after legendary creatures of Japan and this was just another name in the [long list](http://en.wikipedia.org/wiki/List_of_legendary_creatures_from_Japan).

### Why Rust and not Go?

The astute reader will notice that I have a similar project written in Go (bakeneko, another legendary creature from Japan). So why stop working on that one and start another in a new language? Let me count the ways...

* Rust has a richer type system
    - Real enums (sum types) that can be used with pattern matching
        + Go has only integers and the `iota` keyword
    - Generics to enable honest-to-goodness reusable collections
        + Avoids the dreaded `interface{}` everywhere
* Rust does not have a `nil` value, the infamous billion dollar mistake
    - While Rust has `()`, it has its own type (`unit`), so cannot be used mistakenly
* Rust has pattern matching which prevents missed cases
* Rust is expression oriented (i.e. `if` is an expression)
* Rust uses nominal subtyping instead of structural subtyping
    - Easier when reading new code, easier to maintain existing code
* Rust permits fine-grained visibility of program items via nested modules
* Additional reasons given in these well written posts:
    - http://crufter.com/2014/12/01/everyday-hassles-in-go/
    - http://yager.io/programming/go.html
    - http://bravenewgeek.com/go-is-unapologetically-flawed-heres-why-we-use-it/
    - http://vagabond.github.io/rants/2015/06/05/a-year-with-go/

### Other Scheme interpreters in Rust

There are a ton of them, but not all of them are complete, and most are not shooting for R7RS small language completeness. Besides, this project is for fun, not novelty.

* https://github.com/neunenak/rust-scheme-interpreter
* https://github.com/kenpratt/rusty_scheme
* https://github.com/fitzgen/oxischeme
* https://github.com/chenyukang/rust-scm
* https://github.com/kimhyunkang/r6.rs
* https://github.com/kimhyunkang/r5.rs
* https://github.com/LeoTestard/r7.rs-VM
