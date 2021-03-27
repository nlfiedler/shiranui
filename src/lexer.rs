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
//! A lexical analyzer for Scheme R7RS.
//!
//! Fashioned after that which was presented by Rob Pike in the "Lexical
//! Scanning in Go" talk (http://cuddle.googlecode.com/hg/talk/lex.html).
//! The general idea is that the lexer produces tokens and sends them to a
//! channel, from which a parser would consume them. This allows the lexer
//! code to be written in a very straightforward and clear manner.
//!
//! The design of the lexer involves a finite state machine consisting of
//! function pointers. The starting function determines which function
//! should go next, returning the pointer to that function. This continues
//! until either `None` is returned by a function, or the end of the input
//! is reached. The "machine" itself is very simple, it continuously invokes
//! the current state function, using its return value as the next function
//! to invoke.
//!
//! As each function processes the input, it may emit one or more tokens.
//! These are sent over a channel from which the recipient, presumably a
//! parser, consumes them. The lexer runs in a separate thread, sending
//! tokens to the channel until either it fills up and blocks, or the input
//! is exhausted.
//!
//! If the input contains an invalid sequence of some sort, the lexer will
//! report the error via a special error token, sent via the channel.
//!

use std::char;
use std::fmt;
use std::str::CharIndices;
use std::sync::mpsc::{self, Receiver, SyncSender};
use std::thread;

/// Defines the type of a particular token.
///
/// Everything in Scheme is represented by a token and its corresponding
/// type, including strings, numbers, lists, vectors, quoting constructs,
/// and special symbols.
///
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenType {
    Error,
    OpenParen,
    CloseParen,
    Dot,
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
    EndOfFile,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::Error => write!(f, "Error"),
            TokenType::OpenParen => write!(f, "OpenParen"),
            TokenType::CloseParen => write!(f, "CloseParen"),
            TokenType::Dot => write!(f, "Dot"),
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

/// Represents a single token emitted by the lexer.
///
/// Tokens have a type (`TokenType`), string value, and location within the
/// input text.
///
#[derive(PartialEq, Debug)]
pub struct Token {
    /// The type of the token.
    pub typ: TokenType,
    /// Text of the token, typically taken directly from the input.
    pub val: String,
    /// Line (1-based) on which the token was encountered.
    pub row: usize,
    /// Column position (0-based) of the _end_ of the token.
    pub col: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token[{}: '{}' <{}:{}>]",
            self.typ, self.val, self.row, self.col
        )
    }
}

/// The `Lexer` struct holds the state of the lexical analyzer.
struct Lexer<'a> {
    // used only for error reports
    name: String,
    // the string being scanned
    input: &'a str,
    // iterator of the characters in the string
    iter: CharIndices<'a>,
    // the next character to return, if peek() has been called
    peeked: Option<(usize, char)>,
    // start position of the current token (in bytes)
    start: usize,
    // position of next character to read (in bytes)
    pos: usize,
    // width of last character read from input (in bytes)
    width: usize,
    // current line of program text being read
    row: usize,
    // current column of text being read (in characters)
    col: usize,
    // true if fold-case is enabled
    folding: bool,
    // channel sender for scanned tokens
    chan: SyncSender<Token>,
}

impl<'a> fmt::Display for Lexer<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Lexer for '{}' at offset {}", self.name, self.pos)
    }
}

impl<'a> Lexer<'a> {
    /// `new` constructs an instance of `Lexer` for the named input.
    fn new(name: String, input: &'a str, chan: SyncSender<Token>) -> Lexer<'a> {
        Lexer {
            name: name,
            input: input,
            iter: input.char_indices(),
            peeked: None,
            start: 0,
            pos: 0,
            width: 0,
            row: 1,
            col: 0,
            folding: false,
            chan: chan,
        }
    }

    /// emit passes the current token back to the client via the channel.
    fn emit(&mut self, t: TokenType) {
        let text = &self.input[self.start..self.pos];
        let _ = self.chan.send(Token {
            typ: t,
            val: text.to_string(),
            row: self.row,
            col: self.col,
        });
        self.start = self.pos;
    }

    /// `emit_folded` will fold the case of the token and then emit the
    /// identifier to the token channel.
    fn emit_folded(&mut self, t: TokenType) {
        let text = &self.input[self.start..self.pos];
        let _ = self.chan.send(Token {
            typ: t,
            val: to_lowercase(text),
            row: self.row,
            col: self.col,
        });
        self.start = self.pos;
    }

    /// emit_text passes the given token back to the client via the channel.
    fn emit_text(&mut self, t: TokenType, text: &str) {
        let _ = self.chan.send(Token {
            typ: t,
            val: text.to_string(),
            row: self.row,
            col: self.col,
        });
        self.start = self.pos;
    }

    /// `emit_identifier` will fold the case of the identifier if the #!fold-case
    /// directive is enabled, then emit the identifier to the token channel.
    /// Otherwise, no folding is performed before emitting the token, per the
    /// default. If the `ident` parameter is `None`, the current token text will
    /// be emitted, otherwise the value of `ident` is emitted.
    fn emit_identifier(&mut self, ident: &str) {
        let output = if self.folding {
            to_lowercase(ident)
        } else {
            ident.to_string()
        };
        let _ = self.chan.send(Token {
            typ: TokenType::Identifier,
            val: output,
            row: self.row,
            col: self.col,
        });
        self.start = self.pos;
    }

    /// `token_length` returns the length of the current token.
    fn token_length(&mut self) -> usize {
        self.pos - self.start
    }

    /// `token_matches` returns true if the current token matches the given
    /// text exactly (case-sensitive), and false otherwise. If the `folding`
    /// argument is true, the string is lowercased before comparing.
    fn token_matches(&mut self, query: &str, folding: bool) -> bool {
        let text = &self.input[self.start..self.pos];
        if folding {
            let lower_text = to_lowercase(text);
            &lower_text[..] == query
        } else {
            text == query
        }
    }

    /// `next` returns the next rune in the input, or `None` if at the end.
    fn next(&mut self) -> Option<char> {
        let next = if self.peeked.is_some() {
            self.peeked.take()
        } else {
            self.iter.next()
        };
        match next {
            Some((pos, ch)) => {
                self.width = ch.len_utf8();
                self.pos = pos + self.width;
                if ch == '\n' {
                    self.row += 1;
                    self.col = 0;
                } else {
                    self.col += 1;
                }
                Some(ch)
            }
            None => None,
        }
    }

    /// `peek` returns but does not consume the next rune in the input.
    fn peek(&mut self) -> Option<char> {
        if self.peeked.is_none() {
            self.peeked = self.iter.next();
        }
        match self.peeked {
            Some((_, ch)) => Some(ch),
            None => None,
        }
    }

    /// `look_ahead` returns true if the next characters in the input
    /// text match the given query.
    fn look_ahead(&mut self, query: &str, folding: bool) -> bool {
        let q_len = query.len();
        if self.input.len() - self.pos >= q_len {
            let text = &self.input[self.pos..self.pos + q_len];
            return if folding {
                let lower_text = to_lowercase(text);
                &lower_text[..] == query
            } else {
                text == query
            };
        }
        false
    }

    /// `ignore` skips over the pending input before this point.
    fn ignore(&mut self) {
        self.start = self.pos;
    }

    /// `rewind` moves the current position back to the start of the current token.
    fn rewind(&mut self) {
        // recompute the correct row value
        self.row -= self.input[self.start..self.pos]
            .chars()
            .filter(|c| *c == '\n')
            .count();
        self.pos = self.start;

        // recompute the correct column value
        let mut nl = 0;
        if let Some(pos) = self.input[..self.pos].rfind('\n') {
            // don't count the newline itself
            nl = pos + 1;
        }
        self.col = self.input[nl..self.pos].chars().count();

        self.width = 0;
        self.peeked = None;
        self.iter = self.input.char_indices();
        for _ in 0..self.start {
            self.iter.next();
        }
    }

    /// `accept` consumes the next rune if it's from the valid set.
    fn accept(&mut self, valid: &str) -> bool {
        match self.peek() {
            Some(ch) => {
                if valid.contains(ch) {
                    // consume the character
                    self.next();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    /// `accept_run` consumes a run of runes from the valid set.
    fn accept_run(&mut self, valid: &str) -> bool {
        let old_pos = self.pos;
        while let Some(ch) = self.peek() {
            if valid.contains(ch) {
                // consume the character
                self.next();
            } else {
                break;
            }
        }
        old_pos < self.pos
    }
}

/// `StateFn` represents the state of the scanner as a function that returns
/// the next state. As a side effect of the function, tokens may be emitted.
/// Cannot use recursive types, as in Go, so must wrap in a struct.
struct StateFn(fn(&mut Lexer) -> Option<StateFn>);

/// Initiates the lexical analysis of the given input text.
///
/// The lex function initializes the lexer to analyze the given Scheme
/// input text, returning the channel receiver from which tokens are
/// received.
///
pub fn lex(name: &str, input: &str) -> Receiver<Token> {
    let sanitized = sanitize_input(input);
    let (tx, rx) = mpsc::sync_channel(1);
    let thread_name = name.to_string();

    thread::spawn(move || {
        let mut lexer = Lexer::new(thread_name, &*sanitized, tx);
        // inform the compiler what the type of state _really_ is
        let mut state: fn(&mut Lexer) -> Option<StateFn> = lex_start;
        while let Some(next) = state(&mut lexer) {
            let StateFn(state_fn) = next;
            state = state_fn;
        }
    });
    rx
}

/// `errorf` emits an error token and returns `None` to end lexing.
fn errorf(l: &mut Lexer, message: &str) -> Option<StateFn> {
    l.emit_text(TokenType::Error, message);
    None
}

/// `lex_start` reads the next token from the input and determines what
/// to do with that token, returning the appropriate state function.
fn lex_start(l: &mut Lexer) -> Option<StateFn> {
    if let Some(ch) = l.next() {
        match ch {
            '(' => {
                l.emit(TokenType::OpenParen);
                Some(StateFn(lex_start))
            }
            ')' => {
                l.emit(TokenType::CloseParen);
                Some(StateFn(lex_start))
            }
            '"' => Some(StateFn(lex_string)),
            ' ' | '\t' | '\r' | '\n' => Some(StateFn(lex_separator)),
            ';' => Some(StateFn(lex_comment)),
            '#' => Some(StateFn(lex_hash)),
            '[' | ']' | '{' | '}' => errorf(l, "use of reserved character"),
            '\'' | '`' => Some(StateFn(lex_quote)),
            ',' => Some(StateFn(lex_unquote)),
            '0'..='9' => {
                l.rewind();
                Some(StateFn(lex_number))
            }
            '+' | '-' => Some(StateFn(lex_explicit_sign)),
            '.' => Some(StateFn(lex_dot)),
            '@' => errorf(l, "@ cannot be the start of a token"),
            '\\' => errorf(l, "\\ cannot be the start of a token"),
            '|' => Some(StateFn(lex_pipe_identifier)),
            _ => {
                // almost certainly an identifier
                l.rewind();
                Some(StateFn(lex_identifier))
            }
        }
    } else {
        l.emit(TokenType::EndOfFile);
        None
    }
}

/// `lex_string` expects the current character to be a double-quote and
/// scans the input to find the end of the quoted string.
fn lex_string(l: &mut Lexer) -> Option<StateFn> {
    let mut text = String::new();
    while let Some(ch) = l.next() {
        match ch {
            // pass over escaped characters
            '\\' => {
                if let Some(ch) = l.next() {
                    match ch {
                        '"' => text.push('"'),
                        ' ' | '\t' => text.push(ch),
                        _ => {
                            // otherwise let replace_escapes() handle it
                            text.push('\\');
                            text.push(ch);
                        }
                    }
                } else {
                    return errorf(l, "improperly terminated string");
                }
            }
            '"' => {
                // reached the end of the string
                match replace_escapes(&text[..]) {
                    Ok(escaped) => {
                        l.emit_text(TokenType::String, &escaped[..]);
                        return Some(StateFn(lex_start));
                    }
                    Err(msg) => {
                        return errorf(l, msg);
                    }
                }
            }
            _ => {
                text.push(ch);
            }
        }
    }
    errorf(l, "unclosed quoted string")
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
            }
            _ => continue,
        }
    }
    Some(StateFn(lex_start))
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
                        _ => continue,
                    }
                } else {
                    break;
                }
            }
            '|' => {
                if let Some(ch) = l.next() {
                    match ch {
                        '#' => {
                            nesting -= 1;
                            if nesting == 0 {
                                l.ignore();
                                return Some(StateFn(lex_start));
                            }
                        }
                        _ => continue,
                    }
                } else {
                    break;
                }
            }
            _ => continue,
        }
    }
    errorf(l, "unclosed block comment")
}

/// `lex_hash` processes all of the # tokens.
fn lex_hash(l: &mut Lexer) -> Option<StateFn> {
    if let Some(ch) = l.next() {
        match ch {
            '|' => Some(StateFn(lex_block_comment)),
            't' | 'f' => {
                // allow for #true and #false
                l.accept_run("aelrsu");
                if l.token_length() > 2
                    && !l.token_matches("#true", false)
                    && !l.token_matches("#false", false)
                {
                    return errorf(l, "invalid boolean literal");
                }
                if let Some(ch) = l.peek() {
                    if !is_delimiter(ch) {
                        l.next();
                        return errorf(l, "invalid boolean literal");
                    }
                }
                l.emit(TokenType::Boolean);
                Some(StateFn(lex_start))
            }
            '(' => {
                l.emit(TokenType::Vector);
                Some(StateFn(lex_start))
            }
            ';' => {
                // emit line comment; parser does the real work
                l.emit(TokenType::Comment);
                Some(StateFn(lex_start))
            }
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
                errorf(l, "reached EOF in byte vector expression")
            }
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                l.accept_run("0123456789");
                if let Some(ch) = l.next() {
                    if ch == '#' {
                        l.emit(TokenType::LabelReference);
                    } else if ch == '=' {
                        l.emit(TokenType::LabelDefinition);
                    } else {
                        return errorf(l, "invalid label expression");
                    }
                } else {
                    return errorf(l, "reached EOF in label expression");
                }
                Some(StateFn(lex_start))
            }
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
                Some(StateFn(lex_start))
            }
            '\\' => {
                if let Some(ch) = l.peek() {
                    if ch == 'x' {
                        return Some(StateFn(lex_x_character));
                    }
                } else {
                    return errorf(l, "reached EOF in character literal");
                }
                Some(StateFn(lex_character))
            }
            'b' | 'd' | 'e' | 'i' | 'o' | 'x' => {
                // let lex_number sort out the prefix
                l.rewind();
                Some(StateFn(lex_number))
            }
            _ => errorf(l, "unrecognized hash value"),
        }
    } else {
        errorf(l, "reached EOF in hash expression")
    }
}

/// `lex_x_character` determines if the #\x is just a single letter or
/// a hex scalar value (\xNNNN) and processes accordingly.
fn lex_x_character(l: &mut Lexer) -> Option<StateFn> {
    // consume the 'x' itself
    l.next();
    if let Some(ch) = l.peek() {
        if is_delimiter(ch) {
            // it is just the letter x
            l.emit(TokenType::Character);
        } else {
            // read characters until the required delimiter is found
            let mut text = String::new();
            text.push('#');
            text.push('\\');
            text.push('x');
            while let Some(ch) = l.peek() {
                if is_delimiter(ch) {
                    break;
                } else {
                    text.push(ch);
                    l.next();
                }
            }
            // as with numeric literals, some validation happens at parse time
            l.emit_text(TokenType::Character, &text[..]);
        }
    }
    Some(StateFn(lex_start))
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
    if l.token_matches("#\\newline", folding)
        || l.token_matches("#\\space", folding)
        || l.token_matches("#\\alarm", folding)
        || l.token_matches("#\\backspace", folding)
        || l.token_matches("#\\delete", folding)
        || l.token_matches("#\\escape", folding)
        || l.token_matches("#\\null", folding)
        || l.token_matches("#\\return", folding)
        || l.token_matches("#\\tab", folding)
    {
        l.emit_folded(TokenType::Character);
    } else {
        // ensure we read at least one character
        if l.token_length() < 3 && l.next() == None {
            return errorf(l, "reached EOF in character literal");
        }
        // ensure the next token is a delimiter
        if let Some(ch) = l.peek() {
            if !is_delimiter(ch) {
                l.next();
                return errorf(l, "invalid character literal");
            }
        }
        l.emit(TokenType::Character);
    }
    Some(StateFn(lex_start))
}

/// `lex_quote` processes the special quoting characters (' and `).
fn lex_quote(l: &mut Lexer) -> Option<StateFn> {
    l.emit(TokenType::Quote);
    Some(StateFn(lex_start))
}

/// `lex_unquote` processes the special unquote characters (, and ,@).
fn lex_unquote(l: &mut Lexer) -> Option<StateFn> {
    if let Some(ch) = l.peek() {
        if ch == '@' {
            l.next();
        }
    } else {
        return errorf(l, "reached EOF in quote expression");
    }
    l.emit(TokenType::Quote);
    Some(StateFn(lex_start))
}

/// `NumberLexer` is used to lexically analyze a numeric literal.
struct NumberLexer {
    is_float: bool,
    is_complex: bool,
    is_rational: bool,
    is_exact: bool,
    digits: String,
}

impl NumberLexer {
    /// `new` constructs a new instance of NumberLexer.
    fn new() -> NumberLexer {
        NumberLexer {
            is_float: false,
            is_complex: false,
            is_rational: false,
            is_exact: true,
            digits: "0123456789".to_string(),
        }
    }

    /// `accept_prefix_r` attempts to read the prefix to a numeric literal.
    fn accept_prefix_r(&mut self, l: &mut Lexer) -> Result<u8, &'static str> {
        // we expect either exactness, radix, or both, in any order;
        // however, if we see more than one of either, that's an error
        let mut base_set = 0;
        let mut exactness_set = 0;
        while l.accept("#") {
            if let Some(ch) = l.next() {
                match ch {
                    'd' | 'D' => {
                        base_set += 1;
                    }
                    'b' | 'B' => {
                        base_set += 1;
                        self.digits = "01".to_string();
                    }
                    'o' | 'O' => {
                        base_set += 1;
                        self.digits = "01234567".to_string();
                    }
                    'x' | 'X' => {
                        base_set += 1;
                        self.digits = "0123456789abcdefABCDEF".to_string();
                    }
                    'e' | 'E' => {
                        exactness_set += 1;
                    }
                    'i' | 'I' => {
                        exactness_set += 1;
                    }
                    _ => {
                        // unrecognized letter, signal an error
                        base_set = 10;
                    }
                }
            }
        }
        if base_set > 1 || exactness_set > 1 {
            return Err("malformed number prefix");
        }
        Ok(0)
    }

    /// `accept_integer_r` attempts to read an integer, possibly inexact.
    fn accept_integer_r(&mut self, l: &mut Lexer, tentative: bool) -> Result<u8, &'static str> {
        let ok = l.accept_run(&self.digits[..]);
        if !tentative && !ok {
            return Err("malformed unsigned integer");
        }
        if l.accept_run("#") {
            self.is_exact = false;
        }
        Ok(0)
    }

    /// `accept_ureal_r` attempts to read an unsigned real number, possibly inexact.
    fn accept_ureal_r(&mut self, l: &mut Lexer, tentative: bool) -> Result<u8, &'static str> {
        let pos = l.pos;
        let int_result_1 = self.accept_integer_r(l, tentative);
        if int_result_1.is_err() {
            return int_result_1;
        }
        if l.accept("/") {
            if (l.pos - pos) == 1 {
                // there has to be something before the /
                return Err("malformed rational");
            }
            self.is_rational = true;
            let int_result_2 = self.accept_integer_r(l, false);
            if int_result_2.is_err() {
                return int_result_2;
            }
        } else if self.digits.len() == 10 && l.accept(".") {
            self.is_float = true;
            if self.is_exact {
                l.accept_run(&self.digits[..]);
            } else {
                l.accept_run("#");
            }
        }
        if l.accept("dDeEfFlLsS") {
            self.is_float = true;
            l.accept("+-");
            l.accept_run(&self.digits[..]);
        }
        Ok(0)
    }

    /// `accept_real_r` attempts to read an optionally signed real number.
    fn accept_real_r(&mut self, l: &mut Lexer, tentative: bool) -> Result<u8, &'static str> {
        if l.look_ahead("inf.0", true) || l.look_ahead("nan.0", true) {
            self.is_float = true;
            // consume the text
            l.next();
            l.next();
            l.next();
            l.next();
            l.next();
            Ok(0)
        } else {
            l.accept("+-");
            self.accept_ureal_r(l, tentative)
        }
    }

    /// `accept_infnan` checks for the "inf.0" and "nan.0" cases.
    fn accept_infnan(&mut self, l: &mut Lexer) -> Result<u8, &'static str> {
        if l.look_ahead("inf.0", true) || l.look_ahead("nan.0", true) {
            self.is_float = true;
            // consume the text
            l.next();
            l.next();
            l.next();
            l.next();
            l.next();
        }
        Ok(0)
    }
}

/// `lex_number` expects the current position to be the start of a numeric
/// literal, and advances to the end of the literal. It will parse both
/// integer and floating point decimal values.
fn lex_number(l: &mut Lexer) -> Option<StateFn> {
    //
    // See R7RS 7.1.1 for detailed format for numeric constants
    //
    let mut nl = NumberLexer::new();

    // Scan for every conceivable numeric literal known to Scheme...
    let prefix_result = nl.accept_prefix_r(l);
    if prefix_result.is_err() {
        return errorf(l, prefix_result.unwrap_err());
    }
    let real_result = nl.accept_real_r(l, true);
    if real_result.is_err() {
        return errorf(l, real_result.unwrap_err());
    }
    if l.accept("@") {
        nl.is_complex = true;
        let real_result2 = nl.accept_real_r(l, false);
        if real_result2.is_err() {
            return errorf(l, real_result2.unwrap_err());
        }
    } else if l.accept("+-") {
        nl.is_complex = true;
        let ureal_result = nl.accept_ureal_r(l, true);
        if ureal_result.is_err() {
            return errorf(l, ureal_result.unwrap_err());
        }
        let infnan_result = nl.accept_infnan(l);
        if infnan_result.is_err() {
            return errorf(l, infnan_result.unwrap_err());
        }
        if !l.accept("iI") {
            return errorf(l, "malformed complex");
        }
    }
    let infnan_result = nl.accept_infnan(l);
    if infnan_result.is_err() {
        return errorf(l, infnan_result.unwrap_err());
    }
    if l.accept("iI") {
        nl.is_complex = true;
    }

    // Next character must not be alphanumeric or related to numbers in any
    // way ('.', '+', '-', '@'), which happens to be "special subsequent".
    if let Some(ch) = l.peek() {
        if !is_delimiter(ch) {
            l.next();
            return errorf(l, "malformed number suffix");
        }
    }
    if nl.is_complex {
        l.emit(TokenType::Complex);
    } else if nl.is_rational {
        l.emit(TokenType::Rational);
    } else if nl.is_float {
        l.emit(TokenType::Float);
    } else {
        l.emit(TokenType::Integer);
    }
    Some(StateFn(lex_start))
}

/// `lex_dot` decides what should be done with the dot (.) that the
/// lexer just encountered (could be an identifier or a number).
fn lex_dot(l: &mut Lexer) -> Option<StateFn> {
    if let Some(ch) = l.peek() {
        // a dot followed by dot subsequent is an identifier
        if is_dot_subsequent(ch) || is_delimiter(ch) {
            l.rewind();
            return Some(StateFn(lex_identifier));
        }
        // everything else must be a number
        l.rewind();
        return Some(StateFn(lex_number));
    }
    // and if we ran out of tokens, it is the '.' special symbol
    l.emit(TokenType::Dot);
    Some(StateFn(lex_identifier))
}

/// `lex_explicit_sign` decides what should be done with the explicit sign
/// that the lexer just encountered (could be identifier or a number).
fn lex_explicit_sign(l: &mut Lexer) -> Option<StateFn> {
    if let Some(ch) = l.peek() {
        if ch == 'i' || ch == 'I' || l.look_ahead("inf.0", true) || l.look_ahead("nan.0", true) {
            // these are ambiguous, but since 7.1.1 explicitly names
            // them as numbers, despite fitting the identifier pattern,
            // treat them as numbers
            l.rewind();
            return Some(StateFn(lex_number));
        }
        if is_sign_subsequent(ch) || is_delimiter(ch) {
            l.rewind();
            return Some(StateFn(lex_identifier));
        } else if ch == '.' {
            l.next();
            if let Some(ch) = l.peek() {
                if is_dot_subsequent(ch) {
                    l.rewind();
                    return Some(StateFn(lex_identifier));
                }
            } else {
                // if we ran out of tokens, it's an identifier
                l.rewind();
                return Some(StateFn(lex_identifier));
            }
        }
        // everything else must be a number
        l.rewind();
        return Some(StateFn(lex_number));
    }
    // if we ran out of tokens, it's an identifier
    l.rewind();
    Some(StateFn(lex_identifier))
}

/// `lex_identifier` processes the text as an identifier.
fn lex_identifier(l: &mut Lexer) -> Option<StateFn> {
    // If we reached this function, we have determined that the input
    // cannot be anything but an identifier, and we simply proceed with
    // that understanding.
    let mut ident = String::new();
    while let Some(ch) = l.peek() {
        // These conditions cover the "peculiar identifier", and "initial"
        // and "special initial" are buried in here, as well.
        if is_explicit_sign(ch)
            || is_subsequent(ch)
            || is_sign_subsequent(ch)
            || is_dot_subsequent(ch)
        {
            ident.push(ch);
            l.next();
        } else if is_delimiter(ch) {
            break;
        } else {
            return errorf(l, "improperly terminated identifier");
        }
    }
    l.emit_identifier(&ident[..]);
    Some(StateFn(lex_start))
}

/// `lex_pipe_identifier` expects the first character to be a vertical
/// bar (|) and scans the text until it finds an unescaped vertical bar.
/// The text inbetween may contain inline hex escapes and mnemonic escapes.
fn lex_pipe_identifier(l: &mut Lexer) -> Option<StateFn> {
    let mut ident = String::new();
    ident.push('|');
    // form |identifier| allows nearly anything
    while let Some(ch) = l.next() {
        if ch == '\\' {
            if let Some(ch) = l.next() {
                match ch {
                    // \ is only permitted before specific characters
                    '|' => ident.push('|'),
                    'a' => ident.push('\x07'),
                    'b' => ident.push('\x08'),
                    't' => ident.push('\t'),
                    'n' => ident.push('\n'),
                    'r' => ident.push('\r'),
                    'x' => {
                        ident.push('\\');
                        ident.push(ch);
                    }
                    // everything else is wrong
                    _ => {
                        return errorf(l, "expected x|a|b|t|n|r after \\ in escape sequence");
                    }
                }
            } else {
                return errorf(l, "reached EOF in |identifier| expression");
            }
        } else if ch == '|' {
            ident.push(ch);
            match replace_escapes(&ident[..]) {
                Ok(escaped) => {
                    l.emit_identifier(&escaped[..]);
                    return Some(StateFn(lex_start));
                }
                Err(msg) => {
                    return errorf(l, msg);
                }
            }
        } else {
            ident.push(ch);
        }
    }
    errorf(l, "reached EOF in |identifier| expression")
}

/// `is_delimiter` returns true if `ch` is a delimiter character.
fn is_delimiter(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '\r' | '\n' | '|' | '(' | ')' | '"' | ';' => true,
        _ => false,
    }
}

/// `is_initial` returns true if `ch` is an initial identifier character.
#[inline]
fn is_initial(ch: char) -> bool {
    ch.is_alphabetic() || is_special_initial(ch)
}

/// `is_special_initial` returns true if `ch` is a special subsequent for identifiers.
#[inline]
fn is_special_initial(ch: char) -> bool {
    "!$%&*/:<=>?^_~".contains(ch)
}

/// `is_subsequent` returns true if `ch` is a subsequent identifier character.
#[inline]
fn is_subsequent(ch: char) -> bool {
    is_initial(ch) || ch.is_digit(10) || is_special_subsequent(ch)
}

/// `is_explicit_sign` returns true if ch is a plus (+) or minus (-) sign.
#[inline]
fn is_explicit_sign(ch: char) -> bool {
    ch == '+' || ch == '-'
}

/// `is_special_subsequent` returns true if `ch` is a special subsequent identifier character.
#[inline]
fn is_special_subsequent(ch: char) -> bool {
    is_explicit_sign(ch) || ch == '.' || ch == '@'
}

/// `is_dot_subsequent` returns true if `ch` is a dot subsequent for identifiers.
#[inline]
fn is_dot_subsequent(ch: char) -> bool {
    is_sign_subsequent(ch) || ch == '.'
}

/// `is_sign_subsequent` returns true if `ch` is a sign subsequent for identifiers.
#[inline]
fn is_sign_subsequent(ch: char) -> bool {
    is_initial(ch) || is_explicit_sign(ch) || ch == '@'
}

/// `sanitize_input` prepares the input program for lexing, which basically
/// means converting various end-of-line character sequences to a single
/// form, namely newlines.
fn sanitize_input(input: &str) -> String {
    input.replace("\r\n", "\n").replace("\r", "\n")
}

/// `to_lowercase` converts the given string to its lowercase form.
fn to_lowercase(input: &str) -> String {
    let mut s = String::with_capacity(input.len());
    s.extend(input.chars().flat_map(|c| c.to_lowercase()));
    s
}

/// `replace_escapes` replaces any \xNNNN; escape sequences with the Unicode
/// code point identified by the NNNN hexadecimal value, where NNNN can be
/// two, three, or four hexadecimal digits. The code point must be valid.
/// Also handles the \a, \b, \t, \n, and \r escapes.
fn replace_escapes(text: &str) -> Result<String, &'static str> {
    let mut result = String::new();
    let mut iter = text.chars();
    while let Some(ch) = iter.next() {
        if ch == '\\' {
            if let Some(ch) = iter.next() {
                match ch {
                    'a' => result.push('\x07'),
                    'b' => result.push('\x08'),
                    't' => result.push('\t'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    '\\' => result.push('\\'),
                    'x' => {
                        let mut hex = String::new();
                        loop {
                            if let Some(ch) = iter.next() {
                                if ch == ';' {
                                    break;
                                }
                                hex.push(ch);
                            } else {
                                return Err("missing ; after \\x escape sequence");
                            }
                        }
                        // verify this is a valid inline hex escape value
                        match u32::from_str_radix(&hex[..], 16) {
                            Ok(code) => match char::from_u32(code) {
                                Some(x) => result.push(x),
                                None => {
                                    return Err("invalid UTF code point");
                                }
                            },
                            Err(_) => {
                                return Err("invalid hexadecimal escape code");
                            }
                        }
                    }
                    _ => {
                        return Err("expected x|a|b|t|n|r after \\ in escape sequence");
                    }
                }
            } else {
                return Err("reached EOF after \\ escape");
            }
        } else {
            result.push(ch);
        }
    }
    Ok(result)
}

#[cfg(test)]
mod test {

    use super::{lex, replace_escapes, sanitize_input, to_lowercase, TokenType};
    use std::collections::HashMap;
    use std::vec::Vec;

    /// `verify_success` lexes a program and verifies that the tokens
    /// emitted match those in the vector of tuples.
    fn verify_success(input: &str, expected: Vec<(TokenType, &str)>) {
        let rx = lex("verify_success", input);
        for er in expected.iter() {
            if let Ok(token) = rx.recv() {
                assert_eq!(token.typ, er.0);
                assert_eq!(token.val, er.1);
            } else {
                assert!(false, "ran out of tokens");
            }
        }
        // make sure we have reached the end of the results
        if let Ok(token) = rx.recv() {
            assert_eq!(token.typ, TokenType::EndOfFile);
        } else {
            assert!(false, "should have exhausted tokens");
        }
    }

    /// `verify_locations` lexes a program and verifies that the tokens
    /// emitted match in type, value, and location of those given.
    fn verify_locations(input: &str, expected: Vec<(TokenType, &str, usize, usize)>) {
        let rx = lex("verify_locations", input);
        for er in expected.iter() {
            if let Ok(token) = rx.recv() {
                assert_eq!(token.typ, er.0);
                assert_eq!(&token.val[..], er.1);
                assert_eq!(token.row, er.2);
                assert_eq!(token.col, er.3);
            } else {
                assert!(false, "ran out of tokens");
            }
        }
        // make sure we have reached the end of the results
        if let Ok(token) = rx.recv() {
            assert_eq!(token.typ, TokenType::EndOfFile);
        } else {
            assert!(false, "should have exhausted tokens");
        }
    }

    /// `verify_singles` verifies individual expressions to check for
    /// special cases in the lexer.
    fn verify_singles(inputs: HashMap<&str, (TokenType, &str)>) {
        for (input, er) in inputs.iter() {
            let rx = lex("verify_singles", input);
            if let Ok(token) = rx.recv() {
                if token.typ == TokenType::Error {
                    panic!("lex failed for {} with {}", input, token.val);
                }
                assert_eq!(token.typ, er.0);
                assert_eq!(&token.val[..], er.1);
            } else {
                assert!(false, "ran out of tokens");
            }
        }
    }

    /// `verify_errors` checks that the input (map key) produces an error
    /// containing the substring given as the map value.
    fn verify_errors(inputs: HashMap<&str, &str>) {
        for (input, expected) in inputs.iter() {
            let rx = lex("verify_errors", input);
            if let Ok(token) = rx.recv() {
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
    fn test_replace_escapes() {
        // normal cases
        assert_eq!(
            replace_escapes("foo bar baz quux").unwrap(),
            "foo bar baz quux".to_string()
        );
        assert_eq!(
            replace_escapes("foo\\x20;quux").unwrap(),
            "foo quux".to_string()
        );
        assert_eq!(
            replace_escapes("\\x65e5;\\x672c;\\x8a9e;").unwrap(),
            "日本語".to_string()
        );
        assert_eq!(replace_escapes("\\a").unwrap(), "\x07".to_string());
        assert_eq!(replace_escapes("\\b").unwrap(), "\x08".to_string());
        assert_eq!(replace_escapes("\\t").unwrap(), "\t".to_string());
        assert_eq!(replace_escapes("\\n").unwrap(), "\n".to_string());
        assert_eq!(replace_escapes("\\r").unwrap(), "\r".to_string());
        // error cases
        assert_eq!(
            replace_escapes("\\f").unwrap_err(),
            "expected x|a|b|t|n|r after \\ in escape sequence"
        );
        assert_eq!(
            replace_escapes("\\xAB").unwrap_err(),
            "missing ; after \\x escape sequence"
        );
        assert_eq!(
            replace_escapes("\\xD801;").unwrap_err(),
            "invalid UTF code point"
        );
        assert_eq!(
            replace_escapes("\\xGGGG;").unwrap_err(),
            "invalid hexadecimal escape code"
        );
        assert_eq!(
            replace_escapes("\\").unwrap_err(),
            "reached EOF after \\ escape"
        );
    }

    #[test]
    fn test_to_lowercase() {
        assert_eq!(to_lowercase("abc"), "abc");
        assert_eq!(to_lowercase("ABC"), "abc");
        assert_eq!(to_lowercase("aBc"), "abc");
        assert_eq!(to_lowercase("AbC"), "abc");
    }

    #[test]
    fn test_empty_input() {
        let rx = lex("unit", "");
        if let Ok(token) = rx.recv() {
            assert_eq!(token.typ, TokenType::EndOfFile);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_open_close_paren() {
        let mut vec = Vec::new();
        vec.push((TokenType::OpenParen, "("));
        vec.push((TokenType::CloseParen, ")"));
        verify_success("()", vec);
    }

    #[test]
    fn test_quoted_string() {
        // valid inputs
        let mut singles = HashMap::new();
        singles.insert("\"foo\"", (TokenType::String, "foo"));
        singles.insert("\"f\\ao\"", (TokenType::String, "f\x07o"));
        singles.insert("\"f\\bo\"", (TokenType::String, "f\x08o"));
        singles.insert("\"f\\to\"", (TokenType::String, "f\to"));
        singles.insert("\"f\\no\"", (TokenType::String, "f\no"));
        singles.insert("\"f\\ro\"", (TokenType::String, "f\ro"));
        singles.insert("\"foo\\  \n  bar\"", (TokenType::String, "foo  \n  bar"));
        singles.insert("\"f\\\"o\"", (TokenType::String, "f\"o"));
        singles.insert("\"f\\\\o\"", (TokenType::String, "f\\o"));
        singles.insert("\"f\\x20;o\"", (TokenType::String, "f o"));
        verify_singles(singles);
        // error cases
        let mut errors = HashMap::new();
        errors.insert("\"foo", "unclosed quoted string");
        errors.insert("\"foo\\", "improperly terminated string");
        verify_errors(errors);
    }

    #[test]
    fn test_reserved_characters() {
        let mut map = HashMap::new();
        map.insert("[", "use of reserved character");
        map.insert("]", "use of reserved character");
        map.insert("{", "use of reserved character");
        map.insert("}", "use of reserved character");
        map.insert("@", "@ cannot be the start of a token");
        map.insert("\\", "\\ cannot be the start of a token");
        verify_errors(map);
    }

    #[test]
    fn test_invalid_hashes() {
        let mut map = HashMap::new();
        map.insert("#zero", "unrecognized hash value");
        map.insert("#", "reached EOF in hash expression");
        verify_errors(map);
    }

    #[test]
    fn test_separators() {
        let mut vec = Vec::new();
        vec.push((TokenType::OpenParen, "("));
        vec.push((TokenType::CloseParen, ")"));
        verify_success("     (\n\t )\r\n", vec);
    }

    #[test]
    fn test_ignored_comments() {
        let mut vec = Vec::new();
        vec.push((TokenType::OpenParen, "("));
        vec.push((TokenType::CloseParen, ")"));
        verify_success(" ; foo \n   (\n ; bar \n )\n", vec);
    }

    #[test]
    fn test_block_comments() {
        let mut vec = Vec::new();
        vec.push((TokenType::OpenParen, "("));
        vec.push((TokenType::CloseParen, ")"));
        verify_success("#| outer #| nested |# outer |# ( #| bar |# )", vec);
        let mut map = HashMap::new();
        map.insert("#| foo", "unclosed block comment");
        verify_errors(map);
    }

    #[test]
    fn test_quotes() {
        let mut vec = Vec::new();
        vec.push((TokenType::Quote, ","));
        vec.push((TokenType::Quote, ",@"));
        vec.push((TokenType::Quote, "'"));
        vec.push((TokenType::Quote, "`"));
        verify_success(", ,@ ' `", vec);
        let mut map = HashMap::new();
        map.insert(",", "reached EOF in quote expression");
        verify_errors(map);
    }

    #[test]
    fn test_booleans() {
        let mut vec = Vec::new();
        vec.push((TokenType::Boolean, "#t"));
        vec.push((TokenType::Boolean, "#true"));
        vec.push((TokenType::Boolean, "#f"));
        vec.push((TokenType::Boolean, "#false"));
        verify_success("#t #true #f #false", vec);
        let mut map = HashMap::new();
        map.insert("#tree", "invalid boolean literal");
        map.insert("#fawls", "invalid boolean literal");
        map.insert("#truez", "invalid boolean literal");
        map.insert("#falseH", "invalid boolean literal");
        verify_errors(map);
    }

    #[test]
    fn test_vectors() {
        let mut vec = Vec::new();
        vec.push((TokenType::Vector, "#("));
        vec.push((TokenType::Boolean, "#t"));
        vec.push((TokenType::Boolean, "#f"));
        vec.push((TokenType::CloseParen, ")"));
        verify_success("#(#t #f)", vec);
    }

    #[test]
    fn test_byte_vectors() {
        let mut vec = Vec::new();
        vec.push((TokenType::ByteVector, "#u8("));
        vec.push((TokenType::Integer, "32"));
        vec.push((TokenType::Integer, "64"));
        vec.push((TokenType::Integer, "128"));
        vec.push((TokenType::CloseParen, ")"));
        verify_success("#u8(32 64 128)", vec);
        let mut map = HashMap::new();
        map.insert("#u ", "invalid byte vector expression");
        map.insert("#u8 ", "invalid byte vector expression");
        map.insert("#u", "reached EOF in byte vector expression");
        verify_errors(map);
    }

    #[test]
    fn test_comments() {
        let mut vec = Vec::new();
        vec.push((TokenType::Comment, "#;"));
        vec.push((TokenType::Boolean, "#t"));
        vec.push((TokenType::Comment, "#;"));
        vec.push((TokenType::Boolean, "#f"));
        verify_success("#;  #t #;#f", vec);
    }

    #[test]
    fn test_labels() {
        let mut vec = Vec::new();
        vec.push((TokenType::LabelDefinition, "#1="));
        vec.push((TokenType::Boolean, "#t"));
        vec.push((TokenType::LabelReference, "#1#"));
        verify_success("#1=#t #1#", vec);
        let mut map = HashMap::new();
        map.insert("#1+", "invalid label expression");
        map.insert("#1", "reached EOF in label expression");
        verify_errors(map);
    }

    #[test]
    fn test_characters() {
        let input = r#"#\a #\space #\newline #\t #\x #\y #\z #\^ #\) #\1 #\x20 #\x65e5
        #\alarm #\backspace #\delete #\escape #\null #\return #\tab"#;
        let mut vec = Vec::new();
        vec.push((TokenType::Character, "#\\a"));
        vec.push((TokenType::Character, "#\\space"));
        vec.push((TokenType::Character, "#\\newline"));
        vec.push((TokenType::Character, "#\\t"));
        vec.push((TokenType::Character, "#\\x"));
        vec.push((TokenType::Character, "#\\y"));
        vec.push((TokenType::Character, "#\\z"));
        vec.push((TokenType::Character, "#\\^"));
        vec.push((TokenType::Character, "#\\)"));
        vec.push((TokenType::Character, "#\\1"));
        vec.push((TokenType::Character, "#\\x20"));
        vec.push((TokenType::Character, "#\\x65e5"));
        vec.push((TokenType::Character, "#\\alarm"));
        vec.push((TokenType::Character, "#\\backspace"));
        vec.push((TokenType::Character, "#\\delete"));
        vec.push((TokenType::Character, "#\\escape"));
        vec.push((TokenType::Character, "#\\null"));
        vec.push((TokenType::Character, "#\\return"));
        vec.push((TokenType::Character, "#\\tab"));
        verify_success(input, vec);
        let mut map = HashMap::new();
        map.insert("#\\foo", "invalid character literal");
        map.insert("#\\", "reached EOF in character literal");
        verify_errors(map);
    }

    #[test]
    fn test_integers() {
        let inputs = r#"0 123 #d1234 #d#e1234 #o366 #i#o366 #x7b5 #b01010100 15##"#;
        let mut vec = Vec::new();
        vec.push((TokenType::Integer, "0"));
        vec.push((TokenType::Integer, "123"));
        vec.push((TokenType::Integer, "#d1234"));
        vec.push((TokenType::Integer, "#d#e1234"));
        vec.push((TokenType::Integer, "#o366"));
        vec.push((TokenType::Integer, "#i#o366"));
        vec.push((TokenType::Integer, "#x7b5"));
        vec.push((TokenType::Integer, "#b01010100"));
        vec.push((TokenType::Integer, "15##"));
        verify_success(inputs, vec);
    }

    #[test]
    fn test_floats() {
        let inputs =
            r#".01 0.1 1.00 6e4 7.91e+16 3. 12#.### 1.2345e 1.2345s 1.2345f 1.2345d 1.2345l"#;
        let mut vec = Vec::new();
        vec.push((TokenType::Float, ".01"));
        vec.push((TokenType::Float, "0.1"));
        vec.push((TokenType::Float, "1.00"));
        vec.push((TokenType::Float, "6e4"));
        vec.push((TokenType::Float, "7.91e+16"));
        vec.push((TokenType::Float, "3."));
        vec.push((TokenType::Float, "12#.###"));
        vec.push((TokenType::Float, "1.2345e"));
        vec.push((TokenType::Float, "1.2345s"));
        vec.push((TokenType::Float, "1.2345f"));
        vec.push((TokenType::Float, "1.2345d"));
        vec.push((TokenType::Float, "1.2345l"));
        verify_success(inputs, vec);
        let mut singles = HashMap::new();
        singles.insert("+inf.0", (TokenType::Float, "+inf.0"));
        singles.insert("-inf.0", (TokenType::Float, "-inf.0"));
        singles.insert("+nan.0", (TokenType::Float, "+nan.0"));
        singles.insert("-nan.0", (TokenType::Float, "-nan.0"));
        verify_singles(singles);
    }

    #[test]
    fn test_complex() {
        let inputs = r#"3+4i 3.0+4.0i 3.0@4.0 3.0-4.0i -4.0i +4.0i 3.0-i 3.0+i -i +i"#;
        let mut vec = Vec::new();
        vec.push((TokenType::Complex, "3+4i"));
        vec.push((TokenType::Complex, "3.0+4.0i"));
        vec.push((TokenType::Complex, "3.0@4.0"));
        vec.push((TokenType::Complex, "3.0-4.0i"));
        vec.push((TokenType::Complex, "-4.0i"));
        vec.push((TokenType::Complex, "+4.0i"));
        vec.push((TokenType::Complex, "3.0-i"));
        vec.push((TokenType::Complex, "3.0+i"));
        vec.push((TokenType::Complex, "-i"));
        vec.push((TokenType::Complex, "+i"));
        verify_success(inputs, vec);
        let mut singles = HashMap::new();
        // <infnan> i
        singles.insert("+inf.0i", (TokenType::Complex, "+inf.0i"));
        singles.insert("-inf.0i", (TokenType::Complex, "-inf.0i"));
        singles.insert("+nan.0i", (TokenType::Complex, "+nan.0i"));
        singles.insert("-nan.0i", (TokenType::Complex, "-nan.0i"));
        // <real R> <infnan> i
        singles.insert("1+inf.0i", (TokenType::Complex, "1+inf.0i"));
        singles.insert("1-inf.0i", (TokenType::Complex, "1-inf.0i"));
        singles.insert("1+nan.0i", (TokenType::Complex, "1+nan.0i"));
        singles.insert("1-nan.0i", (TokenType::Complex, "1-nan.0i"));
        verify_singles(singles);
        let mut errors = HashMap::new();
        errors.insert("3.0+4.0", "malformed complex");
        verify_errors(errors);
    }

    #[test]
    fn test_rationals() {
        let inputs = r#"6/10 123/456 -6/12"#;
        let mut vec = Vec::new();
        vec.push((TokenType::Rational, "6/10"));
        vec.push((TokenType::Rational, "123/456"));
        vec.push((TokenType::Rational, "-6/12"));
        verify_success(inputs, vec);
    }

    #[test]
    fn test_bad_numbers() {
        let mut map = HashMap::new();
        map.insert("0.a", "malformed number suffix");
        map.insert("0.0.0", "malformed number suffix");
        map.insert("0.0+0i+", "malformed number suffix");
        map.insert("0.0-0i-", "malformed number suffix");
        map.insert("0.0@0@", "malformed number suffix");
        map.insert("0a", "malformed number suffix");
        map.insert("#dabc", "malformed number suffix");
        map.insert("#o888", "malformed number suffix");
        map.insert("#b123", "malformed number suffix");
        map.insert("#xzyw", "malformed number suffix");
        map.insert("#b#b00", "malformed number prefix");
        map.insert("#d#d00", "malformed number prefix");
        map.insert("#e#e00", "malformed number prefix");
        map.insert("#i#i00", "malformed number prefix");
        map.insert("#o#o00", "malformed number prefix");
        map.insert("#x#x00", "malformed number prefix");
        map.insert("#b#d00", "malformed number prefix");
        map.insert("#b#o00", "malformed number prefix");
        map.insert("#b#x00", "malformed number prefix");
        map.insert("#d#d00", "malformed number prefix");
        map.insert("#d#o00", "malformed number prefix");
        map.insert("#d#x00", "malformed number prefix");
        map.insert("#o#b00", "malformed number prefix");
        map.insert("#o#d00", "malformed number prefix");
        map.insert("#o#x00", "malformed number prefix");
        map.insert("#x#b00", "malformed number prefix");
        map.insert("#x#d00", "malformed number prefix");
        map.insert("#x#o00", "malformed number prefix");
        map.insert("#e#i00", "malformed number prefix");
        map.insert("#i#e00", "malformed number prefix");
        verify_errors(map);
    }

    #[test]
    fn test_identifier_singles() {
        let mut map = HashMap::new();
        map.insert("lambda", (TokenType::Identifier, "lambda"));
        map.insert("q", (TokenType::Identifier, "q"));
        map.insert("ab12", (TokenType::Identifier, "ab12"));
        map.insert("+", (TokenType::Identifier, "+"));
        map.insert("++", (TokenType::Identifier, "++"));
        map.insert("+-", (TokenType::Identifier, "+-"));
        map.insert("+@", (TokenType::Identifier, "+@"));
        map.insert("-", (TokenType::Identifier, "-"));
        map.insert("--", (TokenType::Identifier, "--"));
        map.insert("-+", (TokenType::Identifier, "-+"));
        map.insert("-@", (TokenType::Identifier, "-@"));
        map.insert("+g", (TokenType::Identifier, "+g"));
        map.insert("+.a", (TokenType::Identifier, "+.a"));
        map.insert("+..a", (TokenType::Identifier, "+..a"));
        map.insert("-g", (TokenType::Identifier, "-g"));
        map.insert("-.a", (TokenType::Identifier, "-.a"));
        map.insert("-..a", (TokenType::Identifier, "-..a"));
        map.insert(".a", (TokenType::Identifier, ".a"));
        map.insert("..a", (TokenType::Identifier, "..a"));
        // the . is special, not really an identifier
        map.insert(".", (TokenType::Dot, "."));
        map.insert("..", (TokenType::Identifier, ".."));
        map.insert("...", (TokenType::Identifier, "..."));
        map.insert("!", (TokenType::Identifier, "!"));
        map.insert("$", (TokenType::Identifier, "$"));
        map.insert("%", (TokenType::Identifier, "%"));
        map.insert("&", (TokenType::Identifier, "&"));
        map.insert("*", (TokenType::Identifier, "*"));
        map.insert("/", (TokenType::Identifier, "/"));
        map.insert(":", (TokenType::Identifier, ":"));
        map.insert("<", (TokenType::Identifier, "<"));
        map.insert("=", (TokenType::Identifier, "="));
        map.insert(">", (TokenType::Identifier, ">"));
        map.insert("?", (TokenType::Identifier, "?"));
        map.insert("^", (TokenType::Identifier, "^"));
        map.insert("_", (TokenType::Identifier, "_"));
        map.insert("~", (TokenType::Identifier, "~"));
        map.insert("list->vector", (TokenType::Identifier, "list->vector"));
        map.insert("+soup+", (TokenType::Identifier, "+soup+"));
        map.insert("V17a", (TokenType::Identifier, "V17a"));
        map.insert("<=?", (TokenType::Identifier, "<=?"));
        map.insert("a34kTMNs", (TokenType::Identifier, "a34kTMNs"));
        map.insert("|two words|", (TokenType::Identifier, "|two words|"));
        map.insert("t-w-r-h-m-m", (TokenType::Identifier, "t-w-r-h-m-m"));
        map.insert("||", (TokenType::Identifier, "||"));
        map.insert("|foo @#$! bar|", (TokenType::Identifier, "|foo @#$! bar|"));
        map.insert("|foo\\abar|", (TokenType::Identifier, "|foo\x07bar|"));
        map.insert("|foo\\bbar|", (TokenType::Identifier, "|foo\x08bar|"));
        map.insert("|foo\\tbar|", (TokenType::Identifier, "|foo\tbar|"));
        map.insert("|foo\\nbar|", (TokenType::Identifier, "|foo\nbar|"));
        map.insert("|foo\\rbar|", (TokenType::Identifier, "|foo\rbar|"));
        map.insert("|foo\\|bar|", (TokenType::Identifier, "|foo|bar|"));
        map.insert("|foo\\x20;bar|", (TokenType::Identifier, "|foo bar|"));
        map.insert(
            "|foo\\x20;\\x20;bar|",
            (TokenType::Identifier, "|foo  bar|"),
        );
        verify_singles(map);
    }

    #[test]
    fn test_identifier_errors() {
        let mut map = HashMap::new();
        map.insert(
            "|a\\p123|",
            "expected x|a|b|t|n|r after \\ in escape sequence",
        );
        map.insert("|a\\xFF|", "missing ; after \\x escape sequence");
        map.insert("|a\\xXYZ;|", "invalid hexadecimal escape code");
        map.insert("|a\\xD801;|", "invalid UTF code point");
        map.insert("|f\\q|", "expected x|a|b|t|n|r after \\ in escape sequence");
        map.insert("|foo", "reached EOF in |identifier| expression");
        map.insert("abc]", "improperly terminated identifier");
        verify_errors(map);
    }

    #[test]
    fn test_foldcase_characters() {
        let input = r#"#!fold-case #\newLIne
        #!no-fold-case
        #\newline
        #!fold-case
        #\NEWLINE
        #!no-fold-case
        #\newline"#;
        let mut vec = Vec::new();
        vec.push((TokenType::Character, "#\\newline"));
        vec.push((TokenType::Character, "#\\newline"));
        vec.push((TokenType::Character, "#\\newline"));
        vec.push((TokenType::Character, "#\\newline"));
        verify_success(input, vec);
    }

    #[test]
    fn test_foldcase_identifiers() {
        let input = r#"#!fold-case lAMbdA
        #!no-fold-case
        lAMbdA
        #!fold-case
        LAMBDA
        #!no-fold-case
        lamBDA"#;
        let mut vec = Vec::new();
        vec.push((TokenType::Identifier, "lambda"));
        vec.push((TokenType::Identifier, "lAMbdA"));
        vec.push((TokenType::Identifier, "lambda"));
        vec.push((TokenType::Identifier, "lamBDA"));
        verify_success(input, vec);
    }

    #[test]
    fn test_invalid_directive() {
        let mut map = HashMap::new();
        map.insert("#!fold-cases", "invalid Scheme directive");
        map.insert("#!nofold-case", "invalid Scheme directive");
        map.insert("#!kazaam", "invalid Scheme directive");
        verify_errors(map);
    }

    #[test]
    fn test_location_information() {
        // The test text includes several elements that involve having
        // the lexer go backward as well as rewinding.
        let input = r#"; testing locations
(define fact
  (lambda (n)
    (if (<= n #x01)
        1
        (* n (fact (- n 1))))))"#;
        let mut vec = Vec::new();
        vec.push((TokenType::OpenParen, "(", 2, 1));
        vec.push((TokenType::Identifier, "define", 2, 7));
        vec.push((TokenType::Identifier, "fact", 2, 12));
        vec.push((TokenType::OpenParen, "(", 3, 3));
        vec.push((TokenType::Identifier, "lambda", 3, 9));
        vec.push((TokenType::OpenParen, "(", 3, 11));
        vec.push((TokenType::Identifier, "n", 3, 12));
        vec.push((TokenType::CloseParen, ")", 3, 13));
        vec.push((TokenType::OpenParen, "(", 4, 5));
        vec.push((TokenType::Identifier, "if", 4, 7));
        vec.push((TokenType::OpenParen, "(", 4, 9));
        vec.push((TokenType::Identifier, "<=", 4, 11));
        vec.push((TokenType::Identifier, "n", 4, 13));
        vec.push((TokenType::Integer, "#x01", 4, 18));
        vec.push((TokenType::CloseParen, ")", 4, 19));
        vec.push((TokenType::Integer, "1", 5, 9));
        vec.push((TokenType::OpenParen, "(", 6, 9));
        vec.push((TokenType::Identifier, "*", 6, 10));
        vec.push((TokenType::Identifier, "n", 6, 12));
        vec.push((TokenType::OpenParen, "(", 6, 14));
        vec.push((TokenType::Identifier, "fact", 6, 18));
        vec.push((TokenType::OpenParen, "(", 6, 20));
        vec.push((TokenType::Identifier, "-", 6, 21));
        vec.push((TokenType::Identifier, "n", 6, 23));
        vec.push((TokenType::Integer, "1", 6, 25));
        vec.push((TokenType::CloseParen, ")", 6, 26));
        vec.push((TokenType::CloseParen, ")", 6, 27));
        vec.push((TokenType::CloseParen, ")", 6, 28));
        vec.push((TokenType::CloseParen, ")", 6, 29));
        vec.push((TokenType::CloseParen, ")", 6, 30));
        vec.push((TokenType::CloseParen, ")", 6, 31));
        verify_locations(input, vec);
    }
}
