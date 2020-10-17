use std::iter::Peekable;

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

#[derive(Debug)]
pub struct Tokeniser<I: Iterator<Item=Result<char, E>>, E, F: FnMut(&str) -> bool> {
    iter: Peekable<I>,
    cur_token: Class,
    buf: String,
    is_op: F,
}

impl<I: Iterator<Item=Result<char, E>>, E, F: FnMut(&str) -> bool> Tokeniser<I, E, F> {
    #[inline]
    pub fn new(iter: Peekable<I>, is_op: F) -> Self {
        Self {
            iter,
            is_op,
            cur_token: Class::Whitespace,
            buf: String::new(),
        }
    }
    #[inline]
    pub fn from_char_iter(chars: I, is_op: F) -> Self {
        Self::new(chars.peekable(), is_op)
    }
}

macro_rules! try_iter {
    ($e:expr) => (
        match $e {
            Ok(s) => s,
            Err(e) => return Some(Err(e))
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

impl<I: Iterator<Item=Result<char, E>>, E, F: FnMut(&str) -> bool> Iterator for Tokeniser<I, E, F> {
    type Item = Result<(String, Class), E>;

    fn next(&mut self) -> Option<Self::Item> {
        let &mut Self{ref mut iter, ref mut cur_token, ref mut buf, ref mut is_op} = self;

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
                            let _a = try_iter!(iter.next().unwrap());
                            let b = try_iter!(iter.next().unwrap());
                            buf.push(escape_char(b));
                            continue
                        }
                        '"' => {
                            let was_empty = buf.is_empty();
                            let quote = try_iter!(iter.next().unwrap());
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

                buf.push(try_iter!(iter.next().unwrap()));
            }
        }

        if let Class::Operator = token {
            for i in buf.char_indices().map(|(i, c)| i + c.len_utf8()).rev() {
                let a = &buf[..i];

                if is_op(a) {
                    let ret = buf.drain(..i).collect();
                    if !buf.is_empty() {
                        *cur_token = Class::Operator;
                    }
                    return Some(Ok((ret, token)));
                }
            }
            if !buf.is_empty() {
                let mut ret = String::new();
                ret.push(buf.remove(0));
                *cur_token = Class::Operator;
                return Some(Ok((ret, Class::UnrecognisedOperator)));
            }
        }

        let ret = buf.clone();
        buf.clear();

        if ret.is_empty() {
            self.next()
        } else {
            Some(Ok((ret, token)))
        }
    }
}
