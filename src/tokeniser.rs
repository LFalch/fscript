use std::iter::Peekable;

use crate::compile::is_op;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Class {
    Identifier,
    Operator,
    UnrecognisedOperator,
    LineComment,
    String,
    Number,
    Whitespace
}

impl Class {
    pub fn should_ignore(self) -> bool {
        match self {
            Class::Whitespace => true,
            Class::LineComment => true,
            Class::String => false,
            Class::Identifier => false,
            Class::Number => false,
            Class::Operator | Class::UnrecognisedOperator => false,
        }
    }
    fn is_continue(self, c: char) -> bool {
        match self {
            Class::Identifier => c.is_alphanumeric() || c == '_',
            Class::Number => c.is_numeric() || c == '.' || c == 'e' || c == 'E',
            Class::Whitespace => c.is_whitespace(),
            Class::String => c != '"' && c != '\n',
            Class::LineComment => c != '\n',
            Class::Operator => !(c.is_alphanumeric() || c.is_whitespace() || c == '"'),
            Class::UnrecognisedOperator => unreachable!(),
        }
    }
    fn classify_start(c: char) -> Self {
        match c {
            '"' => Class::String,
            '#' => Class::LineComment,
            c if c.is_alphabetic() => Class::Identifier,
            c if c.is_numeric() => Class::Number,
            c if c.is_whitespace() => Class::Whitespace,
            _ => Class::Operator,
        }
    }
}

#[derive(Debug, Clone)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Tokeniser<I: Iterator<Item=Result<char, E>>, E> {
    iter: Peekable<I>,
    cur_token: Class,
    buf: String,
    file_loc: FileLocation,
}

impl<I: Iterator<Item=Result<char, E>>, E> Tokeniser<I, E> {
    #[inline]
    pub fn new(iter: Peekable<I>) -> Self {
        Self {
            iter,
            cur_token: Class::Whitespace,
            buf: String::new(),
            file_loc: FileLocation {
                column: 0,
                line: 1,
            }
        }
    }
    #[inline]
    pub fn from_char_iter(chars: I) -> Self {
        Self::new(chars.peekable())
    }
}

#[macro_export]
macro_rules! try_iter {
    ($e:expr, $file_loc:ident) => (
        match $e {
            Ok(s) => s,
            Err(e) => return Some(Err((*$file_loc, e)))
        }
    );
}

fn escape_char(c: char) -> char {
    match c {
        'n' => '\n',
        '"' => '\"',
        '\'' => '\'',
        '\\' => '\\',
        '0' => '\0',
        'r' => '\r',
        't' => '\t',
        _ => '\u{FFFD}',
    }
}

impl<I: Iterator<Item=Result<char, E>>, E> Iterator for Tokeniser<I, E> {
    type Item = Result<Token, (FileLocation, E)>;

    fn next(&mut self) -> Option<Self::Item> {
        let &mut Self{ref mut iter, ref mut cur_token, ref mut buf, ref mut file_loc} = self;

        let token = *cur_token;

        if buf.is_empty() {
            iter.peek()?;

            while let Some(peek_c) = iter.peek() {
                let peek_c = match peek_c {
                    Ok(c) => *c,
                    Err(_) => continue,
                };
                if let Class::String = cur_token {
                    match peek_c {
                        '\\' => {
                            let _a = try_iter!(iter.next().unwrap(), file_loc);
                            let b = try_iter!(iter.next().unwrap(), file_loc);
                            buf.push(escape_char(b));
                            continue
                        }
                        '"' => {
                            let was_empty = buf.is_empty();
                            let quote = try_iter!(iter.next().unwrap(), file_loc);
                            debug_assert_eq!(quote, '"');
                            if was_empty {
                                continue;
                            } else {
                                *cur_token = Class::Whitespace;
                                break;
                            }
                        },
                        _ => ()
                    }
                }
                if !cur_token.is_continue(peek_c) {
                    *cur_token = Class::classify_start(peek_c);
                    break
                }

                buf.push(try_iter!(iter.next().unwrap(), file_loc));
            }
        }

        if let Class::Operator = token {
            for i in buf.char_indices().map(|(i, c)| i + c.len_utf8()).rev() {
                let a = &buf[..i];

                if is_op(a) {
                    let ret: String = buf.drain(..i).collect();
                    if !buf.is_empty() {
                        *cur_token = Class::Operator;
                    }

                    return Some(Ok(Token {
                        file_loc: file_loc.advance(&ret),
                        buf: ret,
                        class: token,
                    }));
                }
            }
            if !buf.is_empty() {
                let mut ret = String::new();
                ret.push(buf.remove(0));
                *cur_token = Class::Operator;
                return Some(Ok(Token {
                    file_loc: file_loc.advance(&ret),
                    buf: ret,
                    class: Class::UnrecognisedOperator,
                }));
            }
        }

        let ret = buf.clone();
        buf.clear();

        if ret.is_empty() {
            self.next()
        } else {
            Some(Ok(Token {
                file_loc: file_loc.advance(&ret),
                buf: ret,
                class: token,
            }))
        }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct FileLocation {
    line: u32,
    column: u32,
}

impl FileLocation {
    fn advance(&mut self, s: &str) -> FileLocation {
        let loc = *self;
        match s.rfind("\n") {
            Some(i) => {
                self.line += 1;
                self.column = (s.len() - i) as u32;
            }
            None => {
                self.column += s.len() as u32;
            }
        }
        loc
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub buf: String,
    pub class: Class,
    pub file_loc: FileLocation,
} 