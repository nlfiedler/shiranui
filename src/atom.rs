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
//! Atoms (symbols, strings, numbers, booleans, characters) in Scheme.
//!

// TODO: remove once the slice vs [..] warnings settle down
#![allow(deprecated)]

// TODO: remove once the code stabilizes
#![allow(dead_code)]

use std::char;
use std::error::Error;
use std::fmt;
use std::num;

#[derive(Copy, Eq, Hash, PartialEq, Debug)]
pub enum Value {
    EmptyList,
    Character(char),
    Boolean(bool),
    // String(String),
    // Symbol(String),
    // Integer(i64),
    // Float(f64),
    // Complex(f64, f64),
    // Rational(i64, i64),
    // Pair(),
    // Vector(),
    // ByteVector(),
    // Procedure(),
    // Port(),
    // Nil or Void?
}

// TODO: YAGNI: Atom.CompareTo() is only needed for comparing symbols and numbers
// TODO: YAGNI: Atom.EqualTo() is used for equal?, eq?, and eqv? and a lot of symbol matching
// TODO: for symbol matching, would be brilliant to handle like Erlang does with atoms
//       (probably #[derive(Eq)] will do the trick already)

impl Value {

    /// `new_boolean` constructs a `Boolean` from the given `bool`.
    pub fn new_boolean(b: bool) -> Value {
        Value::Boolean(b)
    }

    /// `new_character` constructs a `Character` from the given `char`.
    pub fn new_character(ch: char) -> Value {
        Value::Character(ch)
    }

    /// `is_boolean` returns true if the value is a boolean.
    pub fn is_boolean(&self) -> bool {
        match *self {
            Value::Boolean(_) => true,
            _ => false,
        }
    }

    /// `is_boolean` returns true if the value is a character.
    pub fn is_character(&self) -> bool {
        match *self {
            Value::Character(_) => true,
            _ => false,
        }
    }

    // pub fn is_byte_vector(&self) -> bool;
    // pub fn is_complex(&self) -> bool;
    // pub fn is_float(&self) -> bool;
    // pub fn is_integer(&self) -> bool;
    // pub fn is_pair(&self) -> bool;
    // pub fn is_port(&self) -> bool;
    // pub fn is_procedure(&self) -> bool;
    // pub fn is_rational(&self) -> bool;
    // pub fn is_string(&self) -> bool;
    // pub fn is_symbol(&self) -> bool;
    // pub fn is_vector(&self) -> bool;

    /// `to_bool` extracts the boolean value; returns `None` if not Boolean.
    pub fn to_bool(&self) -> Option<bool> {
        match *self {
            Value::Boolean(b) => Some(b),
            _ => None,
        }
    }

    /// `to_char` extracts the character value; returns `None` if not Character.
    pub fn to_char(&self) -> Option<char> {
        match *self {
            Value::Character(ch) => Some(ch),
            _ => None,
        }
    }
}

impl fmt::Display for Value {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::EmptyList => write!(f, "()"),
            Value::Boolean(b) => write!(f, "#{}", b),
            Value::Character(ch) => write!(f, "#\\{}", ch),
        }
    }
}

/// `bool_from_str` returns a `bool` based on the given string input,
/// which must be one of "#t", "#f", "#true", or "#false".
pub fn bool_from_str(s: &str) -> Result<bool, ParseBoolError> {
    match s {
        "#t" | "#true"  => Ok(true),
        "#f" | "#false" => Ok(false),
        _               => Err(ParseBoolError { _priv: () }),
    }
}

/// An error returned when parsing a `bool` from a string fails.
#[derive(Debug, Clone, PartialEq)]
pub struct ParseBoolError { _priv: () }

impl fmt::Display for ParseBoolError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "provided string was not `#t`, `#true`, `#f`, or `#false`".fmt(f)
    }
}

impl Error for ParseBoolError {
    fn description(&self) -> &str {
        "failed to parse bool"
    }
}

/// `char_from_str` returns a `char` based on the given string input,
/// which must be one of the acceptable Scheme character literals.
pub fn char_from_str(s: &str) -> Result<char, ParseCharError> {
    match s {
        "#\\newline"   => Ok('\n'),
        "#\\space"     => Ok(' '),
        "#\\return"    => Ok('\r'),
        "#\\tab"       => Ok('\t'),
        "#\\alarm"     => Ok('\x07'),
        "#\\backspace" => Ok('\x08'),
        "#\\delete"    => Ok('\x7f'),
        "#\\escape"    => Ok('\x1b'),
        "#\\null"      => Ok('\0'),
        _ => {
            if s.len() > 3 && s.starts_with("#\\x") {
                match num::from_str_radix::<u32>(s.slice_from(3), 16) {
                    Ok(code) => {
                        match char::from_u32(code) {
                            Some(ch) => Ok(ch),
                            None => Err(ParseCharError{ kind: CharErrorKind::InvalidUTF })
                        }
                    },
                    Err(_) => {
                        Err(ParseCharError{ kind: CharErrorKind::InvalidHex })
                    }
                }
            } else if s.len() == 3 {
                Ok(s.char_at(2))
            } else {
                Err(ParseCharError{ kind: CharErrorKind::Unrecognized })
            }
        }
    }
}

/// An error which can be returned when parsing a character literal.
#[derive(Debug, PartialEq)]
pub struct ParseCharError { kind: CharErrorKind }

#[derive(Debug, PartialEq)]
enum CharErrorKind {
    InvalidUTF,
    InvalidHex,
    Unrecognized,
}

impl fmt::Display for ParseCharError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.description().fmt(f)
    }
}

impl Error for ParseCharError {
    fn description(&self) -> &str {
        match self.kind {
            CharErrorKind::InvalidUTF => "invalid UTF code point",
            CharErrorKind::InvalidHex => "invalid hexadecimal escape code",
            CharErrorKind::Unrecognized => "unrecognized character literal",
        }
    }
}

#[cfg(test)]
mod test {

    use super::{bool_from_str, char_from_str, CharErrorKind, ParseCharError, Value};

    #[test]
    fn test_bool_from_str() {
        assert_eq!(bool_from_str("#t").unwrap(), true);
        assert_eq!(bool_from_str("#true").unwrap(), true);
        assert_eq!(bool_from_str("#f").unwrap(), false);
        assert_eq!(bool_from_str("#false").unwrap(), false);
        // error cases
        assert!(bool_from_str("#truce").is_err());
        assert!(bool_from_str("#falls").is_err());
    }

    #[test]
    fn test_char_from_str() {
        assert_eq!(char_from_str("#\\newline").unwrap(), '\n');
        assert_eq!(char_from_str("#\\space").unwrap(), ' ');
        assert_eq!(char_from_str("#\\return").unwrap(), '\r');
        assert_eq!(char_from_str("#\\tab").unwrap(), '\t');
        assert_eq!(char_from_str("#\\alarm").unwrap(), '\x07');
        assert_eq!(char_from_str("#\\backspace").unwrap(), '\x08');
        assert_eq!(char_from_str("#\\delete").unwrap(), '\x7f');
        assert_eq!(char_from_str("#\\escape").unwrap(), '\x1b');
        assert_eq!(char_from_str("#\\null").unwrap(), '\0');
        assert_eq!(char_from_str("#\\y").unwrap(), 'y');
        assert_eq!(char_from_str("#\\x65e5").unwrap(), '日');
        // error cases
        assert_eq!(char_from_str("#\\xZZZ").unwrap_err(),
                   ParseCharError{ kind: CharErrorKind::InvalidHex });
        assert_eq!(char_from_str("#\\xD801").unwrap_err(),
                   ParseCharError{ kind: CharErrorKind::InvalidUTF });
        assert_eq!(char_from_str("#\\foobar").unwrap_err(),
                   ParseCharError{ kind: CharErrorKind::Unrecognized });
    }

    #[test]
    fn test_new_values() {
        assert_eq!(Value::new_boolean(true), Value::Boolean(true));
        assert_eq!(Value::new_character('a'), Value::Character('a'));
    }

    #[test]
    fn test_value_types() {
        assert!(Value::new_boolean(true).is_boolean());
        assert!(!Value::new_boolean(true).is_character());
        assert!(!Value::new_character('a').is_boolean());
        assert!(Value::new_character('a').is_character());
    }

    #[test]
    fn test_to_values() {
        assert_eq!(Value::new_boolean(true).to_bool().unwrap(), true);
        assert_eq!(Value::new_boolean(false).to_bool().unwrap(), false);
        assert!(Value::new_boolean(true).to_char().is_none());
        assert_eq!(Value::new_character('a').to_char().unwrap(), 'a');
        assert!(Value::new_character('a').to_bool().is_none());
    }

    #[test]
    fn test_fmt_values() {
        assert_eq!(format!("{}", Value::EmptyList), "()");
        assert_eq!(format!("{}", Value::new_boolean(true)), "#true");
        assert_eq!(format!("{}", Value::new_boolean(false)), "#false");
        assert_eq!(format!("{}", Value::new_character('a')), "#\\a");
    }
}
