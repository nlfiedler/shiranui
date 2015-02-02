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

#![allow(dead_code)]
#![allow(unstable)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use std::fmt;
use std::sync::mpsc::{SyncSender, Receiver};
use std::sync::mpsc;
use std::thread;

#[derive(Copy, PartialEq, Show)]
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

impl fmt::String for TokenType {
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

#[derive(Copy, PartialEq, Show)]
pub struct Token<'a> {
    pub typ: TokenType,
    pub val: &'a str,
    pub row: i32,
    pub col: i32
}

impl <'a> fmt::String for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TODO: String behavior for Token")
    }
}

// lexer holds the state of the scanner.
struct Lexer<'a> {
    // used only for error reports
    name: &'a str,
    // the string being scanned
    input: &'a str,
    // start position of the current token
    start: i32,
    // current position within the input
    pos: i32,
    // width of last rune read from input
    width: i32,
    // current line of program text being read
    row: i32,
    // current column of text being read
    col: i32,
    // true if fold-case is enabled
    folding: bool,
    // channel sender for scanned tokens
    chan: SyncSender<Token<'a>>
}

impl <'a> fmt::String for Lexer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TODO: String behavior for Lexer")
    }
}

impl <'a> Lexer<'a> {
    fn new(name: &'a str, input: &'a str, chan: SyncSender<Token<'a>>) -> Lexer<'a> {
        Lexer {
            name: name,
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
}

// StateFn represents the state of the scanner as a function that returns
// the next state. As a side effect of the function, tokens may be emitted.
// Cannot use recursive types, as in Go, so must wrap in a struct.
struct StateFn(fn(&Lexer) -> StateFn);

/// lex initializes the lexer to lex the given Scheme input text, returning
/// the channel receiver from which tokens are received.
// fn lex<'a>(name: &str, input: &str) /*-> Receiver<Token<'a>>*/ {
    // let sanitized = sanitize_input(input);

    // TODO: get this one line to compile, then get the others piece by piece
    // let (tx, rx) = mpsc::sync_channel(1);
    // let thread_tx = tx.clone();
    // let lexer = Lexer::new(name, sanitized, thread_tx);

    // thread::Thread::spawn(move || {
    //     let mut state = StateFn(lex_start);
    //     loop {
    //         let StateFn(fun) = state;
    //         state = fun(&lexer);
    //         let StateFn(next) = state;
    //         match next {
    //             lex_done => break,
    //             _ => continue
    //         }
    //     }
    //     // TODO: close the channel, or let it fall out of scope?
    // });
    // rx
// }

fn sanitize_input(input: &str) -> String {
    input.replace("\r\n", "\n").replace("\r", "\n")
}

// TODO: implement Drop trait on lexer object so it can clean up task and channel

// lex_start reads the next token from the input and determines
// what to do with that token, returning the appropriate state
// function.
fn lex_start(l: &Lexer) -> StateFn {
    // r := l.next()
    // switch r {
    // case eof:
    //     l.emit(tokenEOF)
    //     return lex_done
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
    StateFn(lex_start)
}

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

fn lex_done(l: &Lexer) -> StateFn {
    StateFn(lex_done)
}

#[test]
fn test_sanitize_input() {
    // none at all
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
