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

// TODO: consider if a trait for Character is needed (what to call it?)
// TODO: consider how the parser will produce locatable characters

// TODO: remove once the slice vs [..] warnings settle down
#![allow(deprecated)]

use std::char;
use std::error::Error;
use std::fmt;
use std::num;
use std::str;

#[derive(Debug, PartialEq)]
pub struct Character(char);

impl str::FromStr for Character {
    type Err = ParseCharError;

    fn from_str(s: &str) -> Result<Character, ParseCharError> {
        match s {
            "#\\newline"   => Ok(Character('\n')),
            "#\\space"     => Ok(Character(' ')),
            "#\\return"    => Ok(Character('\r')),
            "#\\tab"       => Ok(Character('\t')),
            "#\\alarm"     => Ok(Character('\x07')),
            "#\\backspace" => Ok(Character('\x08')),
            "#\\delete"    => Ok(Character('\x7f')),
            "#\\escape"    => Ok(Character('\x1b')),
            "#\\null"      => Ok(Character('\0')),
            _ => {
                if s.len() > 3 && s.starts_with("#\\x") {
                    match num::from_str_radix::<u32>(s.slice_from(3), 16) {
                        Ok(code) => {
                            match char::from_u32(code) {
                                Some(ch) => Ok(Character(ch)),
                                None => Err(ParseCharError{ kind: CharErrorKind::InvalidUTF })
                            }
                        },
                        Err(_) => {
                            Err(ParseCharError{ kind: CharErrorKind::InvalidHex })
                        }
                    }
                } else if s.len() == 3 {
                    Ok(Character(s.char_at(2)))
                } else {
                    Err(ParseCharError{ kind: CharErrorKind::Unrecognized })
                }
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

    use super::{Character, CharErrorKind, ParseCharError};

    #[test]
    fn test_char_from_str() {
        assert_eq!("#\\newline".parse().unwrap(), Character('\n'));
        assert_eq!("#\\space".parse().unwrap(), Character(' '));
        assert_eq!("#\\return".parse().unwrap(), Character('\r'));
        assert_eq!("#\\tab".parse().unwrap(), Character('\t'));
        assert_eq!("#\\alarm".parse().unwrap(), Character('\x07'));
        assert_eq!("#\\backspace".parse().unwrap(), Character('\x08'));
        assert_eq!("#\\delete".parse().unwrap(), Character('\x7f'));
        assert_eq!("#\\escape".parse().unwrap(), Character('\x1b'));
        assert_eq!("#\\null".parse().unwrap(), Character('\0'));
        // error cases
        assert_eq!("#\\xZZZ".parse::<Character>().unwrap_err(),
                   ParseCharError{ kind: CharErrorKind::InvalidHex });
        assert_eq!("#\\xD801".parse::<Character>().unwrap_err(),
                   ParseCharError{ kind: CharErrorKind::InvalidUTF });
        assert_eq!("#\\foobar".parse::<Character>().unwrap_err(),
                   ParseCharError{ kind: CharErrorKind::Unrecognized });
    }
}
