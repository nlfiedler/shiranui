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
// TODO: write more module documentation explaining how the lexer works

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

/// The `Lexer` struct holds the state of the lexical analyzer.
struct Lexer<'a> {
    // used only for error reports
    name: String,
    // the string being scanned
    input: String,
    // start position of the current token
    start: usize,
    // current position within the input
    pos: usize,
    // width of last character read from input
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

impl<'a> fmt::Display for Lexer<'a> {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TODO: String behavior for Lexer")
    }
}

// TODO: implement Drop trait on lexer object so it can clean up the channel?

impl<'a> Lexer<'a> {

    /// `new` constructs an instance of `Lexer` for the named input.
    fn new(name: String, input: String, chan: SyncSender<Token>) -> Lexer<'a> {
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

    // TODO: copy next(), backup(), ignore(), and rewind() basically as-is from lexer.go
    // TODO: --> use StrExt.char_range_at() and StrExt.char_range_at_reverse()
    // TODO: --> replace utf8.DecodeRuneInString() to StrExt.char_range_at()
    // TODO: --> replace utf8.DecodeLastRuneInString() with StrExt.char_range_at_reverse()
    // TODO: --> replace utf8.RuneCountInString() with StrExt.chars().count()

    /// emit passes the current token back to the client via the channel.
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

    /// emit_text passes the given token back to the client via the channel.
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

    /// `next` returns the next rune in the input, or `None` if at the end.
    fn next(&mut self) -> Option<char> {
        if self.pos >= self.input.len() {
            // signal that nothing was read this time
            self.width = 0;
            None
        } else {
            let next = self.input.char_range_at(self.pos);
            self.width = next.next - self.pos;
            self.pos = next.next;
            // advance row/col values in lexer
            if next.ch == '\n' {
                self.row += 1;
                self.col = 0;
            } else {
                // counting characters, not bytes
                self.col += 1;
            }
            Some(next.ch)
        }
    }
}

/// `StateFn` represents the state of the scanner as a function that returns
/// the next state. As a side effect of the function, tokens may be emitted.
/// Cannot use recursive types, as in Go, so must wrap in a struct.
struct StateFn(fn(&mut Lexer) -> Option<StateFn>);

/// lex initializes the lexer to lex the given Scheme input text, returning
/// the channel receiver from which tokens are received.
fn lex(name: &str, input: &str) -> Receiver<Token> {
    let sanitized = sanitize_input(input);
    let (tx, rx) = mpsc::sync_channel(1);
    let thread_tx = tx.clone();
    let thread_name = name.to_string();

    thread::Thread::spawn(move || {
        let mut lexer = Lexer::new(thread_name, sanitized, thread_tx);
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
    });
    rx
}

/// `lex_error` simply returns `None` to signal the end of processing.
fn lex_error(l: &mut Lexer) -> Option<StateFn> {
    None
}

// errorf returns an error token and terminates the scan by passing back
// a nil pointer that will be the next state.
fn errorf(l: &mut Lexer, message: String) -> Option<StateFn> {
    l.emit_text(TokenType::Error, message.as_slice());
    Some(StateFn(lex_error))
}

// sanitize_input prepares the input program for lexing, which basically
// means converting various end-of-line character sequences to a single
// form, namely newlines.
fn sanitize_input(input: &str) -> String {
    input.replace("\r\n", "\n").replace("\r", "\n")
}

// lex_start reads the next token from the input and determines
// what to do with that token, returning the appropriate state
// function.
fn lex_start(l: &mut Lexer) -> Option<StateFn> {
    match l.next() {
        Some(ch) => {
            match ch {
                '(' => {
                    l.emit(TokenType::OpenParen);
                    return Some(StateFn(lex_start));
                },
                ')' => {
                    l.emit(TokenType::CloseParen);
                    return Some(StateFn(lex_start));
                },
                '"' => {
                    return Some(StateFn(lex_string));
                }
                _ => return None
            }
        },
        None => {
            l.emit(TokenType::EndOfFile);
            return None;
        }
    }
    // case ' ', '\t', '\r', '\n':
    //     return lex_separator
    // case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
    //     // let lex_number sort out what type of number it is
    //     l.backup()
    //     return lex_number
    // case ';':
    //     return lex_comment
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
    unreachable!();
}

/// `lex_string` expects the current character to be a double-quote and
/// scans the input to find the end of the quoted string.
fn lex_string(l: &mut Lexer) -> Option<StateFn> {
    loop {
        match l.next() {
            Some(ch) => {
                match ch {
                    // pass over escaped characters
                    '\\' => {
                        l.next();
                        continue;
                    },
                    '"' => {
                        // reached the end of the string
                        l.emit(TokenType::String);
                        return Some(StateFn(lex_start));
                    },
                    _ => continue
                }
            },
            None => {
                let start = l.start;
                return errorf(l, format!("unclosed quoted string starting at {}", start));
            }
        }
    }
    unreachable!();
}

#[cfg(test)]
mod test {

    use super::{lex, sanitize_input, TokenType};
    use std::collections::HashMap;
    use std::fmt;
    use std::vec::Vec;

    struct ExpectedResult {
        typ: TokenType,
        val: String
    }

    impl fmt::Display for ExpectedResult {

        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "ExpectedResult{{typ: {0}, val: {1}}}", self.typ, self.val)
        }
    }

    fn verify_success(input: &str, expected: Vec<ExpectedResult>) {
        let rx = lex("unit", input);
        for er in expected.iter() {
            if let Some(token) = rx.recv().ok() {
                assert_eq!(token.typ, er.typ);
                assert_eq!(token.val, er.val);
            } else {
                assert!(false, "ran out of tokens");
            }
        }
        // make sure we have reached the end of the results
        if let Some(token) = rx.recv().ok() {
            assert_eq!(token.typ, TokenType::EndOfFile);
        } else {
            assert!(false, "should have exhausted tokens");
        }
    }

    /// `verify_errors` checks that the input (map key) produces an error
    /// containing the substring given as the map value.
    fn verify_errors(inputs: HashMap<&str, &str>) {
        for (input, expected) in inputs.iter() {
            let rx = lex("unit", input);
            if let Some(token) = rx.recv().ok() {
                assert_eq!(token.typ, TokenType::Error);
                assert!(token.val.contains(expected), "expected {} error", expected);
            } else {
                assert!(false, "ran out of tokens");
            }
        }
    }

    #[test]
    fn test_sanitize_input() {
        // no EOL characters, no change
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

    #[test]
    fn test_empty_input() {
        let rx = lex("unit", "");
        if let Some(token) = rx.recv().ok() {
            assert_eq!(token.typ, TokenType::EndOfFile);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_open_close_paren() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::OpenParen, val: "(".to_string()});
        vec.push(ExpectedResult{typ: TokenType::CloseParen, val: ")".to_string()});
        verify_success("()", vec);
    }

    #[test]
    fn test_quoted_string() {
        // valid inputs
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::String, val: "\"foo\"".to_string()});
        verify_success("\"foo\"", vec);
        // error cases
        let mut map = HashMap::new();
        map.insert("\"foo", "unclosed quoted string");
        verify_errors(map);
    }
}

// TODO: implement and test lexing a Comment
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
// TODO: port over the tests from lexer_test.go in bakeneko
