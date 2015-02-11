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

// TODO: remove once the code matures
#![allow(dead_code)]

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

    // TODO: copy backup() and rewind() basically as-is from lexer.go
    // TODO: --> replace utf8.RuneCountInString() with StrExt.chars().count()
    // TODO: Try to use the `char` type and it's helpful functions (e.g. `is_whitespace()`)
    // TODO: I like the `is_*` functions for checking if a character is whitespace, etc
    // TODO: Look at the those in `lexer.rs` in r6.rs project
    // TODO: I like the fancy pattern matching that oxischeme uses in `read.rs`

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

    /// `token_length` returns the length of the current token.
    fn token_length(&mut self) -> usize {
        self.pos - self.start
    }

    /// `token_matches` returns true if the current token matches the given
    /// text exactly (case-sensitive), and false otherwise.
    fn token_matches(&mut self, query: &str, folding: bool) -> bool {
        let text = self.input.as_slice().slice(self.start, self.pos);
        if folding {
            let lower_text = fold_case(text);
            lower_text.as_slice() == query
        } else {
            text == query
        }
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

    /// `peek` returns but does not consume the next rune in the input.
    fn peek(&mut self) -> Option<char> {
        if self.pos >= self.input.len() {
            None
        } else {
            let next = self.input.char_range_at(self.pos);
            Some(next.ch)
        }
    }

    /// `ignore` skips over the pending input before this point.
    fn ignore(&mut self) {
        self.start = self.pos;
    }

    /// `accept` consumes the next rune if it's from the valid set.
    fn accept(&mut self, valid: &str) -> bool {
        match self.peek() {
            Some(ch) => {
                if valid.contains_char(ch) {
                    // consume the character
                    self.next();
                    return true;
                }
            },
            None => return false
        }
        false
    }

    /// `accept_run` consumes a run of runes from the valid set.
    fn accept_run(&mut self, valid: &str) -> bool {
        let old_pos = self.pos;
        loop {
            match self.peek() {
                Some(ch) => {
                    if valid.contains_char(ch) {
                        // consume the character
                        self.next();
                    } else {
                        break;
                    }
                },
                None => break
            }
        }
        old_pos < self.pos
    }
}

/// `StateFn` represents the state of the scanner as a function that returns
/// the next state. As a side effect of the function, tokens may be emitted.
/// Cannot use recursive types, as in Go, so must wrap in a struct.
struct StateFn(fn(&mut Lexer) -> Option<StateFn>);

/// lex initializes the lexer to lex the given Scheme input text, returning
/// the channel receiver from which tokens are received.
pub fn lex(name: &str, input: &str) -> Receiver<Token> {
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
fn lex_error(_l: &mut Lexer) -> Option<StateFn> {
    None
}

// errorf returns an error token and terminates the scan by passing back
// a nil pointer that will be the next state.
fn errorf(l: &mut Lexer, message: &str) -> Option<StateFn> {
    l.emit_text(TokenType::Error, message);
    Some(StateFn(lex_error))
}

// sanitize_input prepares the input program for lexing, which basically
// means converting various end-of-line character sequences to a single
// form, namely newlines.
fn sanitize_input(input: &str) -> String {
    input.replace("\r\n", "\n").replace("\r", "\n")
}

/// `fold_case` converts the given `str` to all lowercase.
fn fold_case(s: &str) -> String {
    s.chars().map(|c| c.to_lowercase()).collect::<String>()
}

// lex_start reads the next token from the input and determines
// what to do with that token, returning the appropriate state
// function.
fn lex_start(l: &mut Lexer) -> Option<StateFn> {
    if let Some(ch) = l.next() {
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
            },
            ' ' | '\t' | '\r' | '\n' => {
                return Some(StateFn(lex_separator));
            },
            ';' => {
                return Some(StateFn(lex_comment));
            },
            '#' => {
                return Some(StateFn(lex_hash));
            },
            '[' | ']' | '{' | '}' => {
                return errorf(l, "use of reserved character");
            },
            '\'' | '`' | ',' => {
                return Some(StateFn(lex_quote));
            }
            _ => return None
        }
    } else {
        l.emit(TokenType::EndOfFile);
        return None;
    }
    // case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
    //     // let lex_number sort out what type of number it is
    //     l.backup()
    //     return lex_number
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
    while let Some(ch) = l.next() {
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
    }
    return errorf(l, "unclosed quoted string");
}

/// `lex_separator` expects the current position to be the start of a
/// separator and advances until it finds the end of that separator.
/// No token will be emitted since separators are ignored.
fn lex_separator(l: &mut Lexer) -> Option<StateFn> {
    l.accept_run(" \t\n\r");
    l.ignore();
    Some(StateFn(lex_start))
}

/// `lex_comment` expects the current position to be the start of a
/// comment and advances until it finds the end of the line/file.
/// No token will be emitted since comments are ignored.
fn lex_comment(l: &mut Lexer) -> Option<StateFn> {
    while let Some(ch) = l.next() {
        match ch {
            '\n' | '\r' => {
                // whitespace after comment is significant (R7RS 2.2),
                // but we ignore whitespace anyway
                l.ignore();
                return Some(StateFn(lex_start));
            },
            _ => continue
        }
    }
    return Some(StateFn(lex_start));
}

/// lex_block_comment expects the current position to be the start of a block
/// comment (#|...|#) and advances until it finds the end of the comment.
/// Comments may be nested (#|..#|..|#..|#) but must be properly so, as stated
/// in R7RS 2.2.
fn lex_block_comment(l: &mut Lexer) -> Option<StateFn> {
    let mut nesting = 1;
    while let Some(ch) = l.next() {
        match ch {
            '#' => {
                if let Some(ch) = l.next() {
                    match ch {
                        '|' => nesting += 1,
                        _ => continue
                    }
                } else {
                    break
                }
            },
            '|' => {
                if let Some(ch) = l.next() {
                    match ch {
                        '#' => {
                            nesting -= 1;
                            if nesting == 0 {
                                l.ignore();
                                return Some(StateFn(lex_start))
                            }
                        },
                        _ => continue
                    }
                } else {
                    break
                }
            },
            _ => continue
        }
    }
    return errorf(l, "unclosed block comment");
}

/// `lex_hash` processes all of the # tokens.
fn lex_hash(l: &mut Lexer) -> Option<StateFn> {
    if let Some(ch) = l.next() {
        match ch {
            '|' => return Some(StateFn(lex_block_comment)),
            't' | 'f' => {
                // allow for #true and #false
                l.accept_run("aelrsu");
                if l.token_length() > 2 &&
                        !l.token_matches("#true", false) &&
                        !l.token_matches("#false", false) {
                    return errorf(l, "invalid boolean literal");
                }
                l.emit(TokenType::Boolean);
                return Some(StateFn(lex_start));
            },
            '(' => {
                l.emit(TokenType::Vector);
                return Some(StateFn(lex_start));
            },
            ';' => {
                // emit line comment; parser does the real work
                l.emit(TokenType::Comment);
                return Some(StateFn(lex_start));
            },
            'u' => {
                // byte vector support (e.g. #u8(...))
                if let Some(ch) = l.next() {
                    if ch == '8' {
                        if let Some(ch) = l.next() {
                            if ch == '(' {
                                l.emit(TokenType::ByteVector);
                                return Some(StateFn(lex_start));
                            }
                        }
                    }
                    return errorf(l, "invalid byte vector expression");
                }
                return errorf(l, "reached EOF in byte vector expression");
            },
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                l.accept_run("0123456789");
                if let Some(ch) = l.next() {
                    if ch == '#' {
                        l.emit(TokenType::LabelReference);
                    } else if ch == '=' {
                        l.emit(TokenType::LabelDefinition);
                    } else {
                        return errorf(l, "invalid label expression")
                    }
                } else {
                    return errorf(l, "reached EOF in label expression")
                }
                return Some(StateFn(lex_start));
            },
            '!' => {
                // handle #!fold-case and #!no-fold-case directives (R7RS 2.1)
                l.accept_run("no-fldcase");
                if l.token_matches("#!fold-case", false) {
                    l.folding = true;
                } else if l.token_matches("#!no-fold-case", false) {
                    l.folding = false;
                } else {
                    return errorf(l, "invalid Scheme directive");
                }
                l.ignore();
                return Some(StateFn(lex_start));
            },
            '\\' => {
                return Some(StateFn(lex_character));
            },
            _ => return errorf(l, "unrecognized hash value")
        }
    } else {
        return errorf(l, "reached EOF in hash expression")
    }
    // case 'b', 'd', 'e', 'i', 'o', 'x':
    //     // let lexNumber sort out the prefix
    //     l.rewind()
    //     return lexNumber
    unreachable!();
}

/// `lex_character` processes a character literal.
fn lex_character(l: &mut Lexer) -> Option<StateFn> {
    // check for one of the many special character names
    if l.folding {
        l.accept_run("abcdeiklmnoprstuwABCDEIKLMNOPRSTUW");
    } else {
        l.accept_run("abcdeiklmnoprstuw");
    }
    let folding = l.folding;
    if l.token_matches("#\\newline", folding) {
        l.emit_text(TokenType::Character, "#\\\n");
    } else if l.token_matches("#\\space", folding) {
        l.emit_text(TokenType::Character, "#\\ ");
    } else if l.token_matches("#\\alarm", folding) {
        l.emit_text(TokenType::Character, "#\\\x07");
    } else if l.token_matches("#\\backspace", folding) {
        l.emit_text(TokenType::Character, "#\\\x08");
    } else if l.token_matches("#\\delete", folding) {
        l.emit_text(TokenType::Character, "#\\\x7f");
    } else if l.token_matches("#\\escape", folding) {
        l.emit_text(TokenType::Character, "#\\\x1b");
    } else if l.token_matches("#\\null", folding) {
        l.emit_text(TokenType::Character, "#\\\0");
    } else if l.token_matches("#\\return", folding) {
        l.emit_text(TokenType::Character, "#\\\r");
    } else if l.token_matches("#\\tab", folding) {
        l.emit_text(TokenType::Character, "#\\\t");
    } else {
        // assert that it is a single character (e.g. #\a)
        let prev = l.input.char_range_at_reverse(l.pos);
        if !prev.ch.is_alphabetic() || l.token_length() > 3 {
            return errorf(l, "invalid character literal");
        }
        if let Some(ch) = l.peek() {
            if ch.is_alphabetic() {
                l.next();
                return errorf(l, "invalid character literal")
            }
        }
        l.emit(TokenType::Character);
    }
    Some(StateFn(lex_start))
}

/// `lex_quote` processes the special quoting characters.
fn lex_quote(l: &mut Lexer) -> Option<StateFn> {
    // we already know it's one of the quoting characters, just need
    // to check if it is the two character ,@ form
    let prev = l.input.char_range_at_reverse(l.pos);
    if prev.ch == ',' {
        if let Some(ch) = l.peek() {
            if ch == '@' {
                l.next();
            }
        } else {
            return errorf(l, "reached EOF in quote expression")
        }
    }
    l.emit(TokenType::Quote);
    Some(StateFn(lex_start))
}

#[cfg(test)]
mod test {

    use super::{lex, sanitize_input, fold_case, TokenType};
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
    fn test_fold_case() {
        assert_eq!(fold_case("abc"), "abc");
        assert_eq!(fold_case("ABC"), "abc");
        assert_eq!(fold_case("aBc"), "abc");
        assert_eq!(fold_case("AbC"), "abc");
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

    #[test]
    fn test_reserved_characters() {
        let mut map = HashMap::new();
        map.insert("[", "use of reserved character");
        map.insert("]", "use of reserved character");
        map.insert("{", "use of reserved character");
        map.insert("}", "use of reserved character");
        verify_errors(map);
    }

    #[test]
    fn test_ignore_separators() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::OpenParen, val: "(".to_string()});
        vec.push(ExpectedResult{typ: TokenType::CloseParen, val: ")".to_string()});
        verify_success("     (\n\t )\r\n", vec);
    }

    #[test]
    fn test_ignore_comments() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::OpenParen, val: "(".to_string()});
        vec.push(ExpectedResult{typ: TokenType::CloseParen, val: ")".to_string()});
        verify_success(" ; foo \n   (\n ; bar \n )\n", vec);
    }

    #[test]
    fn test_ignore_block_comments() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::OpenParen, val: "(".to_string()});
        vec.push(ExpectedResult{typ: TokenType::CloseParen, val: ")".to_string()});
        verify_success("#| outer #| nested |# outer |# ( #| bar |# )", vec);
    }

    #[test]
    fn test_quotes() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::Quote, val: ",".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Quote, val: ",@".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Quote, val: "'".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Quote, val: "`".to_string()});
        verify_success(", ,@ ' `", vec);
        let mut map = HashMap::new();
        map.insert(",", "reached EOF in quote expression");
        verify_errors(map);
    }

    #[test]
    fn test_booleans() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#t".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#true".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#f".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#false".to_string()});
        verify_success("#t #true #f #false", vec);
        let mut map = HashMap::new();
        map.insert("#tree", "invalid boolean literal");
        map.insert("#fawls", "invalid boolean literal");
        verify_errors(map);
    }

    #[test]
    fn test_vectors() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::Vector, val: "#(".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#t".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#f".to_string()});
        vec.push(ExpectedResult{typ: TokenType::CloseParen, val: ")".to_string()});
        verify_success("#(#t #f)", vec);
    }

    #[test]
    fn test_byte_vectors() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::ByteVector, val: "#u8(".to_string()});
        // TODO: replace test data with numbers so this at least appears valid
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#t".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#f".to_string()});
        vec.push(ExpectedResult{typ: TokenType::CloseParen, val: ")".to_string()});
        verify_success("#u8(#t #f)", vec);
    }

    #[test]
    fn test_comments() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::Comment, val: "#;".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#t".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Comment, val: "#;".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#f".to_string()});
        verify_success("#;  #t #;#f", vec);
    }

    #[test]
    fn test_labels() {
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::LabelDefinition, val: "#1=".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Boolean, val: "#t".to_string()});
        vec.push(ExpectedResult{typ: TokenType::LabelReference, val: "#1#".to_string()});
        verify_success("#1=#t #1#", vec);
        let mut map = HashMap::new();
        map.insert("#1+", "invalid label expression");
        map.insert("#1", "reached EOF in label expression");
        verify_errors(map);
    }

    #[test]
    fn test_characters() {
        let input = r#"#\a #\space #\newline #\t
        #\alarm #\backspace #\delete #\escape #\null #\return #\tab"#;
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\a".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\ ".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\n".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\t".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\x07".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\x08".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\x7f".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\x1b".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\0".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\r".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\t".to_string()});
        verify_success(input, vec);
        let mut map = HashMap::new();
        map.insert("#\\foo", "invalid character literal");
        map.insert("#\\1", "invalid character literal");
        verify_errors(map);
    }

    #[test]
    fn test_foldcase() {
        let input = r#"#!fold-case #\newLIne
        #!no-fold-case
        #\newline
        #!fold-case
        #\NEWLINE
        #!no-fold-case
        #\newline"#;
        // TODO: add identifiers as well, once lex_identifier is ready
        let mut vec = Vec::new();
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\n".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\n".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\n".to_string()});
        vec.push(ExpectedResult{typ: TokenType::Character, val: "#\\\n".to_string()});
        verify_success(input, vec);
    }
}

// TODO: implement and test lexing a Identifier
// TODO: implement and test lexing a Integer
// TODO: implement and test lexing a Float
// TODO: implement and test lexing a Complex
// TODO: implement and test lexing a Rational
// TODO: port over the tests from lexer_test.go in bakeneko
