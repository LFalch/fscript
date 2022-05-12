use std::{
    io::Read,
    fmt::{self, Display},
};

use lazy_static::lazy_static;
use crate::stack_table;
use crate::{
    source::{
        chars::{Chars, CharsError},
        tokeniser::{Class, Tokeniser, Token, FileLocation},
        error::{Error, ErrorKind},
    },
    stack_table::StackTable,
};

type OpFuncTable<const N: usize> = StackTable<&'static str, &'static str, N>;
type OpFuncTableWithPrecedence<const N: usize> = StackTable<&'static str, (&'static str, u8), N>;

lazy_static! {
    pub static ref BINARY_OPS: OpFuncTableWithPrecedence<20> = stack_table! {
        "+" => ("add", 11),
        "-" => ("sub", 11),
        "*" => ("mul", 12),
        "/" => ("div", 12),
        "%" => ("rem", 12),
        "++" => ("concat", 10),
        "**" => ("pow", 10),
        "==" => ("eq", 3),
        "!=" => ("neq", 3),
        ">" => ("gt", 3),
        ">=" => ("gte", 3),
        "<" => ("lt", 3),
        "<=" => ("lte", 3),
        "&&" => ("and", 2),
        "||" => ("or", 1),
        "<<" => ("shl", 7),
        ">>" => ("shr", 7),
        "&" => ("bitand", 6),
        "^" => ("xor", 5),
        "|" => ("bitor", 4),
    };
    pub static ref UNARY_OPS: OpFuncTable<2> = stack_table! {
        "-" => "neg",
        "!" => "not",
    };
}

pub(crate) fn is_op(s: &str) -> bool {
    SyntaxOp::from_str(s).is_some()
        || BINARY_OPS.contains_key(&s)
        || UNARY_OPS.contains_key(&s)
}


#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SyntaxOp {
    Equal,
    Ref,
    MutRef,
    Deref,
    Member,
    End,
    StartParen,
    EndParen,
    StartIndex,
    EndIndex,
    StartBlock,
    EndBlock,
    StartType,
    EndType,
    Return,
    WithType,
    Comma,
    Keyword(Keyword),
}

impl SyntaxOp {
    #[inline]
    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "=" => SyntaxOp::Equal,
            "&" => SyntaxOp::Ref,
            "@" => SyntaxOp::MutRef,
            "*" => SyntaxOp::Deref,
            "." => SyntaxOp::Member,
            ";" => SyntaxOp::End,
            "(" => SyntaxOp::StartParen,
            ")" => SyntaxOp::EndParen,
            "[" => SyntaxOp::StartIndex,
            "]" => SyntaxOp::EndIndex,
            "{" => SyntaxOp::StartBlock,
            "}" => SyntaxOp::EndBlock,
            "<" => SyntaxOp::StartType,
            ">" => SyntaxOp::EndType,
            "->" => SyntaxOp::Return,
            ":" => SyntaxOp::WithType,
            "," => SyntaxOp::Comma,
            _ => SyntaxOp::Keyword(Keyword::from_str(s)?),
        })
    }
    #[inline]
    fn to_str(&self) -> &'static str {
        match self {
            SyntaxOp::Equal => "=",
            SyntaxOp::Ref => "&",
            SyntaxOp::MutRef => "@",
            SyntaxOp::Deref => "*",
            SyntaxOp::Member => ".",
            SyntaxOp::End => ";",
            SyntaxOp::StartParen => "(",
            SyntaxOp::EndParen => ")",
            SyntaxOp::StartIndex => "[",
            SyntaxOp::EndIndex => "]",
            SyntaxOp::StartBlock => "{",
            SyntaxOp::EndBlock => "}",
            SyntaxOp::StartType => "<",
            SyntaxOp::EndType => ">",
            SyntaxOp::Return => "->",
            SyntaxOp::WithType => ":",
            SyntaxOp::Comma => ",",
            SyntaxOp::Keyword(kw) => kw.to_str(),
        }
    }
    #[inline]
    /// ## Returns
    ///
    /// `None` if bracket, `Some(operand_mode)` if it works like a function call,
    /// where `operand_mode` is the approriate `OperandMode`
    fn operand_mode(&self) -> Option<OperandMode> {
        use self::Keyword::*;
        use SyntaxOp::*;
        match self {
            Equal => Some(OperandMode::Infix),
            Ref | MutRef => Some(OperandMode::Prefix),
            Deref => Some(OperandMode::Prefix),
            Member => Some(OperandMode::Infix),
            Return => Some(OperandMode::Prefix),
            WithType => Some(OperandMode::Infix),
            Comma => Some(OperandMode::Infix),
            End => None,
            Keyword(Fn | Let | Var) => Some(OperandMode::Prefix),
            Keyword(If) => Some(OperandMode::Prefix),
            Keyword(Else) => Some(OperandMode::Prefix),
            Keyword(Loop | While | For) => Some(OperandMode::Prefix),
            EndParen | StartIndex | EndIndex | StartType | EndType
                | StartBlock | EndBlock | StartParen => None,
        }
    }
    #[inline]
    fn last_token_kind(&self) -> LastTokenKind {
        use self::Keyword::*;
        use SyntaxOp::*;
        match self {
            Equal => LastTokenKind::OperatorLike(OperandMode::Infix),
            Ref | MutRef => LastTokenKind::OperatorLike(OperandMode::Prefix),
            Deref => LastTokenKind::OperatorLike(OperandMode::Prefix),
            Member => LastTokenKind::OperatorLike(OperandMode::Infix),
            Return => LastTokenKind::OperatorLike(OperandMode::Prefix),
            WithType => LastTokenKind::OperatorLike(OperandMode::Infix),
            Comma => LastTokenKind::OperatorLike(OperandMode::Infix),
            Keyword(Fn | Let | Var) => LastTokenKind::OperatorLike(OperandMode::Prefix),
            Keyword(If) => LastTokenKind::OperatorLike(OperandMode::Prefix),
            Keyword(Else) => LastTokenKind::OperatorLike(OperandMode::Prefix),
            Keyword(Loop | While | For) => LastTokenKind::OperatorLike(OperandMode::Prefix),
            StartIndex | StartType | StartBlock | StartParen => LastTokenKind::StartBracket,
            EndParen | EndIndex | EndType | EndBlock | End => LastTokenKind::EndBracket,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LastTokenKind {
    Value,
    StartBracket,
    EndBracket,
    OperatorLike(OperandMode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandMode {
    Prefix,
    Infix,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    If,
    Else,
    Loop,
    While,
    For,
    Let,
    Var,
}

impl Keyword {
    #[inline]
    fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "fn" => Keyword::Fn,
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "loop" => Keyword::Loop,
            "while" => Keyword::While,
            "for" => Keyword::For,
            "let" => Keyword::Let,
            "var" => Keyword::Var,
            _ => return None,
        })
    }
    #[inline]
    fn to_str(&self) -> &'static str {
        match self {
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Loop => "loop",
            Keyword::While => "while",
            Keyword::For => "for",
            Keyword::Let => "let",
            Keyword::Var => "var",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenStreamElement {
    Identifier(String, Option<(u8, OperandMode)>),
    SyntaxOp(SyntaxOp),
    NumberLiteral(String),
    StringLiteral(String),
}

impl Display for TokenStreamElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenStreamElement::*;
        match self {
            Identifier(s, None) => write!(f, "{}", s),
            Identifier(s, Some((p, OperandMode::Infix))) => write!(f, ".{}({})$", s, p),
            Identifier(s, Some((p, OperandMode::Prefix))) => write!(f, "{}({})$", s, p),
            SyntaxOp(so) => write!(f, "{}", so.to_str()),
            NumberLiteral(s) => write!(f, "{}", s),
            StringLiteral(s) => write!(f, "{:?}", s),
        }
    }
}

#[derive(Debug)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub(super) struct TokenStream<R: Read> {
    tokeniser: Tokeniser<Chars<R>, CharsError>,
    peeked: Option<(FileLocation, TokenStreamElement)>,
    last_token_kind: LastTokenKind,
}

impl<R: Read> TokenStream<R> {
    pub fn new(tokeniser: Tokeniser<Chars<R>, CharsError>) -> Self {
        TokenStream {
            tokeniser,
            peeked: None,
            last_token_kind: LastTokenKind::EndBracket,
        }
    }
    #[must_use = "This will be just like calling `next` in case there is an error"]
    pub fn peek(&mut self) -> Option<Result<&TokenStreamElement, Error>> {
        match self {
            TokenStream {
                peeked: Some(tse),
                ..
            } => Some(Ok(&tse.1)),
            TokenStream {
                tokeniser,
                peeked: peeked @ None,
                last_token_kind,
            } => match Self::pure_next(tokeniser, last_token_kind) {
                Some(Ok(tse)) => {
                    *peeked = Some(tse);
                    Some(Ok(&peeked.as_ref().unwrap().1))
                }
                Some(Err(e)) => Some(Err(e)),
                None => None,
            }
        }
    }
    fn pure_next(tokeniser: &mut Tokeniser<Chars<R>, CharsError>, last_token_kind: &mut LastTokenKind) -> Option<<TokenStream<R> as Iterator>::Item> {
        loop {
            let token = tokeniser.next()?;
    
            let Token{buf: s, class, file_loc} = match token {
                Ok(o) => o,
                Err((file_loc, e)) => break Some(Err(Error::new(file_loc, ErrorKind::Chars(e)))),
            };

            break Some(Ok((file_loc, match class {
                Class::Identifier => {
                    if let Some(kw) = Keyword::from_str(&s) {
                        *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Prefix);
                        TokenStreamElement::SyntaxOp(SyntaxOp::Keyword(kw))
                    } else {
                        *last_token_kind = LastTokenKind::Value;
                        TokenStreamElement::Identifier(s, None)
                    }
                }
                Class::Operator => {
                    let syntax_op = SyntaxOp::from_str(&s);
                    let bin_op = BINARY_OPS.get(&&*s).copied();
                    let una_op = UNARY_OPS.get(&&*s).copied();

                    // TODO: figure out wtf is going on here (make it clearer)
                    match (*last_token_kind, syntax_op, bin_op, una_op) {
                        (LastTokenKind::Value | LastTokenKind::EndBracket, _, Some((bin_op, pred)), _) => {
                            *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Infix);
                            TokenStreamElement::Identifier(bin_op.to_owned(), Some((pred, OperandMode::Infix)))
                        }
                        (LastTokenKind::Value | LastTokenKind::EndBracket, Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), Some(OperandMode::Infix)) => {
                            *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Infix);
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (LastTokenKind::StartBracket | LastTokenKind::OperatorLike(OperandMode::Prefix), _, _, Some(una_op)) => {
                            *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Prefix);
                            TokenStreamElement::Identifier(una_op.to_owned(), Some((0, OperandMode::Prefix)))
                        }
                        (LastTokenKind::EndBracket | LastTokenKind::StartBracket | LastTokenKind::OperatorLike(OperandMode::Prefix), Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), Some(OperandMode::Prefix)) => {
                            *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Prefix);
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (_, Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), None) => {
                            *last_token_kind = syntax_op.last_token_kind();
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (_, None, None, None) => return Some(Err(Error::new(file_loc, ErrorKind::UnexpectedToken))),
                        (ltk, so, bo, uo) => {
                            return Some(Err(Error::new(file_loc, ErrorKind::UnexpectedOperator(ltk, so, bo, uo))))
                        }
                    }
                }
                Class::UnrecognisedOperator => return Some(Err(Error::new(file_loc, ErrorKind::UnrecognisedOperator(s)))),
                Class::Number => {
                    *last_token_kind = LastTokenKind::Value;
                    TokenStreamElement::NumberLiteral(s)
                }
                Class::String => {
                    *last_token_kind = LastTokenKind::Value;
                    TokenStreamElement::StringLiteral(s)
                }
                Class::LineComment | Class::Whitespace => continue,
            })));
        }
    }
}

impl<R: Read> Iterator for TokenStream<R> {
    type Item = Result<(FileLocation, TokenStreamElement), Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(tse) => Some(Ok(tse)),
            None => Self::pure_next(&mut self.tokeniser, &mut self.last_token_kind),
        }
    }
}
