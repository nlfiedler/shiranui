/*
 * Copyright 2015 Nathan Fiedler
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
//!
//! Sequences consist of chained pairs and vectors.
//!
// TODO: write more module documentation

// TODO: remove once the code matures
#![allow(dead_code)]

use std::fmt;

trait Term {
    // TODO: define what a Term is; any atom or sequence
    fn is_boolean() -> bool;
    fn is_character() -> bool;
    fn is_complex() -> bool;
    fn is_float() -> bool;
    fn is_integer() -> bool;
    fn is_pair() -> bool;
    fn is_rational() -> bool;
    fn is_sequence() -> bool;
    fn is_string() -> bool;
    fn is_symbol() -> bool;
    fn is_vector() -> bool;
}

// TODO: finish defining a Sequence type that both Pair and Vector implement
trait Sequence {
    // TODO: figure out how to define the lifetimes for the return values below
    /// `new` constructs a new instance of a sequence.
    fn new() -> Self;
	// /// `first` returns the first term of the sequence.
	// fn first() -> Option<Term>;
	// /// `second` returns the first non-Sequence term in the Rest() sequence.
	// fn second() -> Option<Term>;
	// /// `third` returns the second non-Sequence term in the Rest() sequence.
	// fn third() -> Option<Term>;
	// /// `rest` returns the portion of the sequence following the first element.
	// fn rest() -> Option<Term>;
	// Len returns the number of things in the sequence.
	fn len() -> usize;
	// Iterator returns an iterator the sequence.
	// fn iter() -> Iterator;
    /// `id` returns a unique identifier for this sequence, helpful for detecting
    /// recursive data structures.
    fn id() -> usize;
}

struct Pair {
    // TODO: See `std::collections::DList` for the `Node<T>` type and how it works
    // TODO: change i32 to Term and figure out the lifetime business
    first: i32,
    rest: i32
    // * Think hard about changing `Pair` to use something other than `interface{}`
    // * Need a base type for all Scheme terms
    // * Maybe `Pair` has additional fields to indicate the null-ness of car and cdr
    // * e.g. a `bool` field named `null_cdr` that indicates that cdr is the empty list
    // * meanwhile, the `cdr` field would hold the actual empty list value, `theEmptyList`
    // * increases the size of the `Pair` struct, but makes the code a lot simpler
    // * or maybe not, and we just compare `cdr` to `theEmptyList` using `==` or `match`
}

/// `EmptyList` represents the Scheme empty list object.
struct EmptyList;

impl Sequence for EmptyList {
    fn new() -> EmptyList {
        EmptyList
    }

    fn len() -> usize {
        0
    }

    fn id() -> usize {
        0
    }
}

impl fmt::Display for EmptyList {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "()")
    }
}
