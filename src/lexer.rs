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
//! A lexical analyzer for Scheme R7RS, fashioned after that which was
//! presented by Rob Pike in the "Lexical Scanning in Go" talk
//! (http://cuddle.googlecode.com/hg/talk/lex.html). The general idea is
//! that the lexer produces tokens and sends them to a channel, from which
//! a parser would consume them. This allows the lexer code to be written
//! in a very straightforward and clear manner.
//!
// TODO: write better module documentation

// TODO: should be able to remove these once the code stabilizes
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use std::fmt;
use std::sync::mpsc::{self, Receiver, SyncSender};
use std::thread;

#[derive(Copy, PartialEq, Debug)]
pub enum TokenType {
    Error,
    OpenParen,
    CloseParen,
    Comment,
    String,
    Quote,
    Character,
    Identifier,
    Integer,
    Float,
    Complex,
    Rational,
    Boolean,
    Vector,
    ByteVector,
    LabelDefinition,
    LabelReference,
    EndOfFile
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::Error => write!(f, "Error"),
            TokenType::OpenParen => write!(f, "OpenParen"),
            TokenType::CloseParen => write!(f, "CloseParen"),
            TokenType::Comment => write!(f, "Comment"),
            TokenType::String => write!(f, "String"),
            TokenType::Quote => write!(f, "Quote"),
            TokenType::Character => write!(f, "Character"),
            TokenType::Identifier => write!(f, "Identifier"),
            TokenType::Integer => write!(f, "Integer"),
            TokenType::Float => write!(f, "Float"),
            TokenType::Complex => write!(f, "Complex"),
            TokenType::Rational => write!(f, "Rational"),
            TokenType::Boolean => write!(f, "Boolean"),
            TokenType::Vector => write!(f, "Vector"),
            TokenType::ByteVector => write!(f, "ByteVector"),
            TokenType::LabelDefinition => write!(f, "LabelDefinition"),
            TokenType::LabelReference => write!(f, "LabelReference"),
            TokenType::EndOfFile => write!(f, "EOF"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub val: String,
    pub row: usize,
    pub col: usize
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TODO: String behavior for Token")
    }
}

// lexer holds the state of the scanner.
struct Lexer {
    // used only for error reports
    name: String,
    // the string being scanned
    input: String,
    // start position of the current token
    start: usize,
    // current position within the input
    pos: usize,
    // width of last rune read from input
    width: usize,
    // current line of program text being read
    row: usize,
    // current column of text being read
    col: usize,
    // true if fold-case is enabled
    folding: bool,
    // channel sender for scanned tokens
    chan: SyncSender<Token>
}

impl fmt::Display for Lexer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TODO: String behavior for Lexer")
    }
}

impl Lexer {
    fn new(name: &str, input: String, chan: SyncSender<Token>) -> Lexer {
        Lexer {
            name: name.to_string(),
            input: input,
            start: 0,
            pos: 0,
            width: 0,
            row: 1,
            col: 0,
            folding: false,
            chan: chan
        }
    }

    // emit passes the current token back to the client via the channel.
    fn emit(&mut self, t: TokenType) {
        let text = self.input.as_slice().slice(self.start, self.pos);
        let _ = self.chan.send(Token {
            typ: t,
            val: text.to_string(),
            row: self.row,
            col: self.col
        });
        self.start = self.pos
    }

    // emitText passes the given token back to the client via the channel.
    fn emit_text(&mut self, t: TokenType, text: &str) {
        // TODO: duplicate code in emit, can it be helped? borrowing error if emit calls emit_text
        let _ = self.chan.send(Token {
            typ: t,
            val: text.to_string(),
            row: self.row,
            col: self.col
        });
        self.start = self.pos
    }
}

// StateFn represents the state of the scanner as a function that returns
// the next state. As a side effect of the function, tokens may be emitted.
// Cannot use recursive types, as in Go, so must wrap in a struct.
struct StateFn(fn(&mut Lexer) -> Option<StateFn>);

/// lex initializes the lexer to lex the given Scheme input text, returning
/// the channel receiver from which tokens are received.
fn lex(name: &str, input: &str) -> Receiver<Token> {
    let sanitized = sanitize_input(input);

    let (tx, rx) = mpsc::sync_channel(1);
    let thread_tx = tx.clone();
    let mut lexer = Lexer::new(name, sanitized, thread_tx);

    thread::Thread::spawn(move || {
        // inform the compiler what the type of state _really_ is
        let mut state = lex_start as fn(&mut Lexer) -> Option<StateFn>;
        loop {
            match state(&mut lexer) {
                Some(next) => {
                    let StateFn(state_fn) = next;
                    state = state_fn;
                },
                None => break
            }
        }
        // TODO: close the channel, or let it fall out of scope?
    });
    rx
}

// TODO: document
fn sanitize_input(input: &str) -> String {
    input.replace("\r\n", "\n").replace("\r", "\n")
}

// TODO: implement Drop trait on lexer object so it can clean up task and channel

// lex_start reads the next token from the input and determines
// what to do with that token, returning the appropriate state
// function.
fn lex_start(l: &mut Lexer) -> Option<StateFn> {
    // r := l.next()
    // switch r {
    // case eof:
    //     l.emit(tokenEOF)
    //     return None
    // case '(':
    //     l.emit(tokenOpenParen)
    //     return lex_start
    // case ')':
    //     l.emit(tokenCloseParen)
    //     return lex_start
    // case ' ', '\t', '\r', '\n':
    //     return lex_separator
    // case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
    //     // let lex_number sort out what type of number it is
    //     l.backup()
    //     return lex_number
    // case ';':
    //     return lex_comment
    // case '"':
    //     return lex_string
    // case '#':
    //     return lex_hash
    // case '\'', '`', ',':
    //     return lex_quote
    // case '[', ']', '{', '}':
    //     return l.errorf("use of reserved character: %c", r)
    // default:
    //     // let lex_identifier sort out what exactly this is
    //     l.backup()
    //     return lex_identifier
    // }
    l.emit(TokenType::EndOfFile);
    None
}

#[cfg(test)]
mod test {

    use super::{lex, sanitize_input, TokenType};

    #[test]
    fn test_sanitize_input() {
        // none, no change
        assert_eq!(sanitize_input("abc"), "abc");
        // mix of everything
        assert_eq!(sanitize_input("a\r\nb\rc\n"), "a\nb\nc\n");
        // DOS style
        assert_eq!(sanitize_input("a\r\nb\r\nc\r\n"), "a\nb\nc\n");
        // old Mac style
        assert_eq!(sanitize_input("a\rb\rc\r"), "a\nb\nc\n");
        // Unix style, no change
        assert_eq!(sanitize_input("a\nb\nc\n"), "a\nb\nc\n");
    }

// TODO: test receiving a token
// fn main() {
//     let rx = make_chan();
//     if let Some(token) = rx.recv().ok() {
//         println!("received token [{}, {}]: of type {} w/ val {}",
//                  token.row,
//                  token.col,
//                  token.typ,
//                  token.val);
//     }
// }

    #[test]
    fn test_empty_input() {
        let rx = lex("test", "");
        if let Some(token) = rx.recv().ok() {
            assert_eq!(token.typ, TokenType::EndOfFile);
        } else {
            assert!(false);
        }
    }
}

// TODO: * Find out what `Fn` and `FnMut` are about
// TODO:     * Appear in `std::ops`
// TODO: * Try to use the `char` type and it's helpful functions (e.g. `is_whitespace()`)
// TODO: * I like the `is_*` functions for checking if a character is whitespace, etc
// TODO:     * Look at the those in `lexer.rs` in r6.rs project
// TODO: * I like the fancy pattern matching that oxischeme uses in `read.rs`
// TODO: * Use `std::iter::Peekable` on a `std::old_io::Reader` to handle chars explicitly
// TODO:     * See `std::old_io::Chars` for reading UTF-8 encoded characters
// TODO:     * Avoid bytes and all that rune decoding business
// TODO:     * To handle `backup()`, use a simple stack, and `next()` pulls from that first
// TODO:     * Double-check that `&str` and `String` do not already provide character iteration
// TODO:     * Test if it supports accented character collation
// TODO: * Look at `std::iter` for `Skip`, `SkipWhile`, `Take`, and `TakeWhile`
// TODO:     * They may shorten the lexing code, if they are applicable
// TODO: * Be sure to use a `std::old_io::BufferedReader` when reading files

// TODO: implement and test lexing an empty string, returning EndOfFile over channel
// TODO: implement and test lexing a OpenParen
// TODO: implement and test lexing a CloseParen
// TODO: implement and test lexing a Comment
// TODO: implement and test lexing a String
// TODO: implement and test lexing a Quote
// TODO: implement and test lexing a Character
// TODO: implement and test lexing a Identifier
// TODO: implement and test lexing a Integer
// TODO: implement and test lexing a Float
// TODO: implement and test lexing a Complex
// TODO: implement and test lexing a Rational
// TODO: implement and test lexing a Boolean
// TODO: implement and test lexing a Vector
// TODO: implement and test lexing a ByteVector
// TODO: implement and test lexing a LabelDefinition
// TODO: implement and test lexing a LabelReference
