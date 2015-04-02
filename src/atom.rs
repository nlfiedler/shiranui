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

use std::char;
use std::cmp::{PartialOrd, Ordering};
use std::error::Error;
use std::fmt::{self, Display, Formatter};

#[derive(Eq, Hash, PartialEq, Debug)]
pub enum Value {
    EmptyList,
    Character(char),
    Boolean(bool),
    Symbol(String),
    String(String),
    Integer(i64),
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

impl Value {

    /// `new_boolean` constructs a `Boolean` from the given `bool`.
    pub fn new_boolean(b: bool) -> Value {
        Value::Boolean(b)
    }

    /// `new_character` constructs a `Character` from the given `char`.
    pub fn new_character(ch: char) -> Value {
        Value::Character(ch)
    }

    /// `new_symbol` constructs a `Symbol` from the given value.
    pub fn new_symbol(sym: &str) -> Value {
        Value::Symbol(sym.to_string())
    }

    /// `new_string` constructs a `String` from the given value.
    pub fn new_string(sym: &str) -> Value {
        Value::String(sym.to_string())
    }

    /// `new_integer` constructs an `Integer` from the given value.
    pub fn new_integer(v: i64) -> Value {
        Value::Integer(v)
    }

    /// `is_boolean` returns true if the value is a boolean.
    pub fn is_boolean(&self) -> bool {
        match *self {
            Value::Boolean(_) => true,
            _ => false,
        }
    }

    /// `is_character` returns true if the value is a character.
    pub fn is_character(&self) -> bool {
        match *self {
            Value::Character(_) => true,
            _ => false,
        }
    }

    /// `is_symbol` returns true if the value is a symbol.
    pub fn is_symbol(&self) -> bool {
        match *self {
            Value::Symbol(_) => true,
            _ => false,
        }
    }

    /// `is_string` returns true if the value is a string.
    pub fn is_string(&self) -> bool {
        match *self {
            Value::String(_) => true,
            _ => false,
        }
    }

    /// `is_integer` returns true if the value is an integer.
    pub fn is_integer(&self) -> bool {
        match *self {
            Value::Integer(_) => true,
            _ => false,
        }
    }

    // pub fn is_byte_vector(&self) -> bool;
    // pub fn is_complex(&self) -> bool;
    // pub fn is_float(&self) -> bool;
    // pub fn is_pair(&self) -> bool;
    // pub fn is_port(&self) -> bool;
    // pub fn is_procedure(&self) -> bool;
    // pub fn is_rational(&self) -> bool;
    // pub fn is_string(&self) -> bool;
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

    /// `to_str` extracts the string value of symbols and strings.
    /// The returned value is a clone of the original.
    /// Returns `None` if the value is neither a symbol nor string.
    pub fn to_str(&self) -> Option<String> {
        match *self {
            // clone the string so the caller cannot mutate it and wreak havoc
            Value::Symbol(ref sym) => Some(sym.clone()),
            Value::String(ref s) => Some(s.clone()),
            _ => None,
        }
    }

    /// `to_int` extracts the integer value; returns `None` if not Integer.
    pub fn to_int(&self) -> Option<i64> {
        match *self {
            Value::Integer(v) => Some(v),
            _ => None,
        }
    }
}

impl Display for Value {

    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Value::EmptyList => write!(f, "()"),
            Value::Boolean(b) => write!(f, "#{}", b),
            Value::Character(ch) => write!(f, "#\\{}", ch),
            Value::Symbol(ref sym) => write!(f, "{}", sym),
            Value::String(ref s) => write!(f, "{}", s),
            Value::Integer(v) => write!(f, "{}", v),
        }
    }
}

// TODO: implement PartialEq for symbols and numbers

impl PartialOrd for Value {

    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match *self {
            Value::Symbol(ref lhs) => {
                match *other {
                    Value::Symbol(ref rhs) => Some(lhs.cmp(rhs)),
                    _ => None
                }
            },
            Value::String(ref lhs) => {
                match *other {
                    Value::String(ref rhs) => Some(lhs.cmp(rhs)),
                    _ => None
                }
            },
            Value::Integer(ref lhs) => {
                match *other {
                    Value::Integer(ref rhs) => Some(lhs.cmp(rhs)),
                    _ => None
                }
            },
            _ => None,
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

impl Display for ParseBoolError {

    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
                match u32::from_str_radix(&s[3..], 16) {
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
                Ok(s[2..].chars().next().unwrap())
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

impl Display for ParseCharError {

    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
        assert_eq!(Value::new_symbol("abc"), Value::Symbol("abc".to_string()));
        assert_eq!(Value::new_string("abc"), Value::String("abc".to_string()));
        assert_eq!(Value::new_integer(1234), Value::Integer(1234));
    }

    #[test]
    fn test_value_types() {
        // boolean
        assert!(Value::new_boolean(true).is_boolean());
        assert!(!Value::new_boolean(true).is_character());
        assert!(!Value::new_boolean(true).is_symbol());
        assert!(!Value::new_boolean(true).is_string());
        assert!(!Value::new_boolean(true).is_integer());
        // character
        assert!(Value::new_character('a').is_character());
        assert!(!Value::new_character('a').is_boolean());
        assert!(!Value::new_character('a').is_symbol());
        assert!(!Value::new_character('a').is_string());
        assert!(!Value::new_character('a').is_integer());
        // symbol
        assert!(Value::new_symbol("abc").is_symbol());
        assert!(!Value::new_symbol("abc").is_boolean());
        assert!(!Value::new_symbol("abc").is_character());
        assert!(!Value::new_symbol("abc").is_string());
        assert!(!Value::new_symbol("abc").is_integer());
        // string
        assert!(Value::new_string("abc").is_string());
        assert!(!Value::new_string("abc").is_symbol());
        assert!(!Value::new_string("abc").is_boolean());
        assert!(!Value::new_string("abc").is_character());
        assert!(!Value::new_string("abc").is_integer());
        // integer
        assert!(Value::new_integer(1234).is_integer());
        assert!(!Value::new_integer(1234).is_string());
        assert!(!Value::new_integer(1234).is_symbol());
        assert!(!Value::new_integer(1234).is_boolean());
        assert!(!Value::new_integer(1234).is_character());
    }

    #[test]
    fn test_to_values() {
        // boolean
        assert_eq!(Value::new_boolean(true).to_bool().unwrap(), true);
        assert_eq!(Value::new_boolean(false).to_bool().unwrap(), false);
        assert!(Value::new_boolean(true).to_char().is_none());
        assert!(Value::new_boolean(true).to_str().is_none());
        assert!(Value::new_boolean(true).to_int().is_none());
        // character
        assert_eq!(Value::new_character('a').to_char().unwrap(), 'a');
        assert!(Value::new_character('a').to_bool().is_none());
        assert!(Value::new_character('a').to_str().is_none());
        assert!(Value::new_character('a').to_int().is_none());
        // symbol
        assert!(Value::new_symbol("abc").to_char().is_none());
        assert!(Value::new_symbol("abc").to_bool().is_none());
        assert_eq!(Value::new_symbol("abc").to_str().unwrap(), "abc".to_string());
        assert!(Value::new_symbol("abc").to_int().is_none());
        // string
        assert!(Value::new_string("abc").to_char().is_none());
        assert!(Value::new_string("abc").to_bool().is_none());
        assert_eq!(Value::new_string("abc").to_str().unwrap(), "abc".to_string());
        assert!(Value::new_string("abc").to_int().is_none());
        // integer
        assert!(Value::new_integer(1234).to_char().is_none());
        assert!(Value::new_integer(1234).to_bool().is_none());
        assert!(Value::new_integer(1234).to_str().is_none());
        assert_eq!(Value::new_integer(1234).to_int().unwrap(), 1234);
    }

    #[test]
    fn test_fmt_values() {
        assert_eq!(format!("{}", Value::EmptyList), "()");
        assert_eq!(format!("{}", Value::new_boolean(true)), "#true");
        assert_eq!(format!("{}", Value::new_boolean(false)), "#false");
        assert_eq!(format!("{}", Value::new_character('a')), "#\\a");
        assert_eq!(format!("{}", Value::new_symbol("abc")), "abc".to_string());
        assert_eq!(format!("{}", Value::new_string("abc")), "abc".to_string());
        assert_eq!(format!("{}", Value::new_integer(1234)), "1234".to_string());
    }

    #[test]
    fn test_to_string() {
        // the #[derive(Debug)] on Value implements to_string()
        assert_eq!(Value::EmptyList.to_string(), "()");
        assert_eq!(Value::new_boolean(true).to_string(), "#true");
        assert_eq!(Value::new_boolean(false).to_string(), "#false");
        assert_eq!(Value::new_character('a').to_string(), "#\\a");
        assert_eq!(Value::new_symbol("abc").to_string(), "abc".to_string());
        assert_eq!(Value::new_string("abc").to_string(), "abc".to_string());
        assert_eq!(Value::new_integer(1234).to_string(), "1234".to_string());
    }

    #[test]
    fn test_sym_ordering() {
        assert!(Value::new_symbol("abc") < Value::new_symbol("def"));
        assert!(Value::new_symbol("xyz") > Value::new_symbol("def"));
        assert!(Value::new_symbol("abc") <= Value::new_symbol("def"));
        assert!(Value::new_symbol("xyz") >= Value::new_symbol("def"));
        assert!(Value::new_symbol("abc") == Value::new_symbol("abc"));
        assert!(Value::new_symbol("bac") != Value::new_symbol("cab"));
    }

    #[test]
    fn test_int_ordering() {
        assert!(Value::new_integer(1234) < Value::new_integer(5678));
        assert!(Value::new_integer(5678) > Value::new_integer(1234));
        assert!(Value::new_integer(1234) <= Value::new_integer(5678));
        assert!(Value::new_integer(5678) >= Value::new_integer(1234));
        assert!(Value::new_integer(1234) == Value::new_integer(1234));
        assert!(Value::new_integer(4321) != Value::new_integer(1234));
    }
}
