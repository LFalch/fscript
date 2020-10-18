use std::{
    io::Read,
    fmt::{self, Display},
};

use lazy_static::lazy_static;

#[macro_use]
use crate::stack_table;
use crate::{
    chars::{Chars, CharsError, CharsExt},
    stack_table::StackTable,
    tokeniser::{Class, Tokeniser},
    Type
};

type OpFuncTable<const N: usize> = StackTable<&'static str, &'static str, N>;
type OpFuncTableWithPrecedence<const N: usize> = StackTable<&'static str, (&'static str, u8), N>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SyntaxOp {
    Equal,
    Ref,
    Deref,
    Member,
    End,
    StartParen,
    EndParen,
    StartIndex,
    EndIndex,
    StartBlock,
    EndBlock,
    Return,
    WithType,
    Comma,
}

impl SyntaxOp {
    #[inline]
    fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "=" => SyntaxOp::Equal,
            "&" => SyntaxOp::Ref,
            "*" => SyntaxOp::Deref,
            "." => SyntaxOp::Member,
            ";" => SyntaxOp::End,
            "(" => SyntaxOp::StartParen,
            ")" => SyntaxOp::EndParen,
            "[" => SyntaxOp::StartIndex,
            "]" => SyntaxOp::EndIndex,
            "{" => SyntaxOp::StartBlock,
            "}" => SyntaxOp::EndBlock,
            "->" => SyntaxOp::Return,
            ":" => SyntaxOp::WithType,
            "," => SyntaxOp::Comma,
            _ => return None,
        })
    }
    #[inline]
    fn to_str(&self) -> &'static str {
        match self {
            SyntaxOp::Equal => "=",
            SyntaxOp::Ref => "&",
            SyntaxOp::Deref => "*",
            SyntaxOp::Member => ".",
            SyntaxOp::End => ";",
            SyntaxOp::StartParen => "(",
            SyntaxOp::EndParen => ")",
            SyntaxOp::StartIndex => "[",
            SyntaxOp::EndIndex => "]",
            SyntaxOp::StartBlock => "{",
            SyntaxOp::EndBlock => "}",
            SyntaxOp::Return => "->",
            SyntaxOp::WithType => ":",
            SyntaxOp::Comma => ",",
        }
    }
    #[inline]
    /// ## Returns
    ///
    /// `None` if bracket, `Some(operand_mode)` if it works like a function call,
    /// where `operand_mode` is the approriate `OperandMode`
    fn operand_mode(&self) -> Option<OperandMode> {
        use SyntaxOp::*;
        match self {
            Equal => Some(OperandMode::Infix),
            Ref => Some(OperandMode::Prefix),
            Deref => Some(OperandMode::Prefix),
            Member => Some(OperandMode::Infix),
            Return => Some(OperandMode::Prefix),
            WithType => Some(OperandMode::Infix),
            Comma => Some(OperandMode::Infix),
            End => None,
            EndParen | StartIndex | EndIndex
                | StartBlock | EndBlock | StartParen => None,
        }
    }
}

lazy_static! {
    static ref BINARY_OPS: OpFuncTableWithPrecedence<20> = stack_table! {
        "+" => ("add", 11),
        "-" => ("sub", 11),
        "*" => ("mul", 12),
        "/" => ("div", 13),
        "%" => ("rem", 13),
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
    static ref UNARY_OPS: OpFuncTable<2> = stack_table! {
        "-" => "neg",
        "!" => "not",
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LastTokenKind {
    Value,
    Bracket,
    OperatorLike(OperandMode),
}

#[derive(Debug)]
pub struct TokenStream<R: Read, F: FnMut(&str) -> bool> {
    tokeniser: Tokeniser<Chars<R>, CharsError, F>,
    last_token_kind: LastTokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandMode {
    Prefix,
    Infix,
}

#[derive(Debug, Clone)]
pub enum TokenStreamElement {
    Identifier(String, Option<(u8, OperandMode)>),
    SyntaxOp(SyntaxOp),
    NumberLiteral(String),
    StringLiteral(String),
}

impl TokenStreamElement {
    #[inline]
    pub fn is_end(&self) -> bool {
        matches!(*self, TokenStreamElement::SyntaxOp(SyntaxOp::End))
    }
}

impl Display for TokenStreamElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenStreamElement::*;
        match self {
            Identifier(s, mode) => match mode {
                None => write!(f, "{}", s),
                Some((p, OperandMode::Infix)) => write!(f, "`{},{}`", p, s),
                Some((p, OperandMode::Prefix)) => write!(f, "`{},{}:", p, s),
            }
            SyntaxOp(so) => write!(f, "{}", so.to_str()),
            NumberLiteral(s) => write!(f, "{}", s),
            StringLiteral(s) => write!(f, "{:?}", s),
        }
    }
}

impl<R: Read, F: FnMut(&str) -> bool> Iterator for TokenStream<R, F> {
    type Item = Result<TokenStreamElement, CharsError>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let token = self.tokeniser.next()?;
    
            let (s, class) = match token {
                Ok(o) => o,
                Err(e) => break Some(Err(e)),
            };
    
            break Some(Ok(match class {
                Class::Identifier => {
                    self.last_token_kind = LastTokenKind::Value;
                    TokenStreamElement::Identifier(s, None)
                }
                Class::Operator => {
                    let syntax_op = SyntaxOp::from_str(&s);
                    let bin_op = BINARY_OPS.get(&&*s);
                    let una_op = UNARY_OPS.get(&&*s);

                    match (self.last_token_kind, syntax_op, bin_op, una_op) {
                        (LastTokenKind::Value, _, Some(&(bin_op, pred)), _) => {
                            self.last_token_kind = LastTokenKind::OperatorLike(OperandMode::Infix);
                            TokenStreamElement::Identifier(bin_op.to_owned(), Some((pred, OperandMode::Infix)))
                        }
                        (LastTokenKind::Value, Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), Some(OperandMode::Infix)) => {
                            self.last_token_kind = LastTokenKind::OperatorLike(OperandMode::Infix);
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (LastTokenKind::Bracket | LastTokenKind::OperatorLike(OperandMode::Prefix), _, _, Some(&una_op)) => {
                            self.last_token_kind = LastTokenKind::OperatorLike(OperandMode::Prefix);
                            TokenStreamElement::Identifier(una_op.to_owned(), Some((0, OperandMode::Prefix)))
                        }
                        (LastTokenKind::Bracket | LastTokenKind::OperatorLike(OperandMode::Prefix), Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), Some(OperandMode::Prefix)) => {
                            self.last_token_kind = LastTokenKind::OperatorLike(OperandMode::Prefix);
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (_, Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), None) => {
                            self.last_token_kind = LastTokenKind::Bracket;
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (ltk, so, bo, uo) => panic!("{:?} {:?} {:?} {:?}", ltk, so, bo, uo),
                    }
                }
                Class::UnrecognisedOperator => panic!("Unrecognised op '{}'", s),
                Class::Number => {
                    self.last_token_kind = LastTokenKind::Value;
                    TokenStreamElement::NumberLiteral(s)
                }
                Class::String => {
                    self.last_token_kind = LastTokenKind::Value;
                    TokenStreamElement::StringLiteral(s)
                }
                Class::LineComment | Class::Whitespace => continue,
            }));
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    AmbigInt(u64),
    Int(i64),
    Uint(u64),
    Float(f64),
    Bool(bool),
    None
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Some(Box<Expr>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Call(String, Box<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Member(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Block(Vec<Statement>, Box<Expr>),
    Function(Vec<String>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    // includes functions, which will just have fancy sugar
    Assignment(String, Option<Type>, Expr),
    DiscardExpr(Expr),
}

pub fn tree<R: Read>(mut ts: TokenStream<R, impl FnMut(&str) -> bool>) -> Vec<Statement> {
    let mut statements = Vec::new();

    // TODO Make this actually parse the stream into a tree

    loop {
        let mut statement_tokens = Vec::new();

        loop {
            let tse = ts.next().expect("Missing semi-colon").unwrap();

            if !tse.is_end() {
                statement_tokens.push(tse);
            } else {
                break
            }
        }


    }

    statements
}

fn token_stream<R: Read>(read: R) -> TokenStream<R, impl FnMut(&str) -> bool> {
    let tokeniser = Tokeniser::from_char_iter(read.chars_iterator(), |s| {
        SyntaxOp::from_str(s).is_some()
            || BINARY_OPS.contains_key(&s)
            || UNARY_OPS.contains_key(&s)
    });

    TokenStream {
        tokeniser,
        last_token_kind: LastTokenKind::Bracket,
    }
}

pub fn compile<R: Read>(read: R) {
    for elem in token_stream(read) {
        let elem = elem.unwrap();
        print!("{}{}", elem, if elem.is_end() { "\n" } else { " " });
    }
}