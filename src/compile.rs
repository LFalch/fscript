use std::{
    io::Read,
    fmt::{self, Display},
};

use lazy_static::lazy_static;

use crate::stack_table;
use crate::{
    chars::{Chars, CharsError, CharsExt},
    stack_table::StackTable,
    tokeniser::{Class, Tokeniser, Token, FileLocation},
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
    StartType,
    EndType,
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
            "<" => SyntaxOp::StartType,
            ">" => SyntaxOp::EndType,
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
            SyntaxOp::StartType => "<",
            SyntaxOp::EndType => ">",
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
            EndParen | StartIndex | EndIndex | StartType | EndType
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
pub enum LastTokenKind {
    Value,
    Bracket,
    OperatorLike(OperandMode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandMode {
    Prefix,
    Infix,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Keyword {
    Fn,
    If,
    Else,
    Loop,
    While,
    For,
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
        }
    }
}

#[derive(Debug, Clone)]
enum TokenStreamElement {
    Identifier(String, Option<(u8, OperandMode)>),
    Keyword(Keyword),
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
            Identifier(s, None) => write!(f, "{}", s),
            Keyword(s) => write!(f, "{}", s.to_str()),
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
struct TokenStream<R: Read> {
    tokeniser: Tokeniser<Chars<R>, CharsError>,
    peeked: Option<(FileLocation, TokenStreamElement)>,
    last_token_kind: LastTokenKind,
}

impl<R: Read> TokenStream<R> {
    #[must_use = "This will be just like calling `next` in case there is an error"]
    fn peek(&mut self) -> Option<Result<&TokenStreamElement, Error>> {
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
                        TokenStreamElement::Keyword(kw)
                    } else {
                        *last_token_kind = LastTokenKind::Value;
                        TokenStreamElement::Identifier(s, None)
                    }
                }
                Class::Operator => {
                    let syntax_op = SyntaxOp::from_str(&s);
                    let bin_op = BINARY_OPS.get(&&*s).copied();
                    let una_op = UNARY_OPS.get(&&*s).copied();

                    match (*last_token_kind, syntax_op, bin_op, una_op) {
                        (LastTokenKind::Value, _, Some((bin_op, pred)), _) => {
                            *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Infix);
                            TokenStreamElement::Identifier(bin_op.to_owned(), Some((pred, OperandMode::Infix)))
                        }
                        (LastTokenKind::Value, Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), Some(OperandMode::Infix)) => {
                            *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Infix);
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (LastTokenKind::Bracket | LastTokenKind::OperatorLike(OperandMode::Prefix), _, _, Some(una_op)) => {
                            *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Prefix);
                            TokenStreamElement::Identifier(una_op.to_owned(), Some((0, OperandMode::Prefix)))
                        }
                        (LastTokenKind::Bracket | LastTokenKind::OperatorLike(OperandMode::Prefix), Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), Some(OperandMode::Prefix)) => {
                            *last_token_kind = LastTokenKind::OperatorLike(OperandMode::Prefix);
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (_, Some(syntax_op), _, _) if matches!(syntax_op.operand_mode(), None) => {
                            *last_token_kind = LastTokenKind::Bracket;
                            TokenStreamElement::SyntaxOp(syntax_op)
                        }
                        (ltk, so, bo, uo) => return Some(Err(Error::new(file_loc, ErrorKind::UnexpectedOperator(ltk, so, bo, uo)))),
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
    Identifer(String),
    Literal(Literal),
    Some(Box<Expr>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Call(String, Vec<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Member(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Block(Vec<Statement>, Box<Expr>),
    Function(Vec<String>, Box<Expr>),
    Return(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    // includes functions, which will just have fancy sugar
    Assignment(String, Option<Type>, Expr),
    DiscardExpr(Expr),
}

mod error;
pub use self::error::{Error, ErrorKind};
use self::error::FlattenToResult;

fn tree<R: Read>(mut ts: TokenStream<R>) -> Result<Vec<Statement>, Error> {
    let mut statements = Vec::new();

    // TODO Make this actually parse the stream into a tree

    loop {
        use self::SyntaxOp::*;
        use TokenStreamElement::*;
        let (file_loc, tse) = match ts.next() {
            Some(tse) => tse?,
            None => break,
        };

        statements.push(match tse {
            Identifier(ident, None) => {
                match ts.peek().flatten()? {
                    SyntaxOp(Equal) => {ts.next(); Statement::Assignment(ident, None, parse_expr(&mut None, &mut ts, false)?)},
                    SyntaxOp(End) => Statement::DiscardExpr(Expr::Identifer(ident)),
                    _ => Statement::DiscardExpr(parse_expr(&mut Some((file_loc, Identifier(ident, None))), &mut ts, false)?),
                }
            }
            SyntaxOp(End) => return Err(Error::new(file_loc, ErrorKind::EmptyStatement)),
            pre => Statement::DiscardExpr(parse_expr(&mut Some((file_loc, pre)), &mut ts, false)?),
        });

        match ts.next().flatten() {
            Ok((_, SyntaxOp(End))) => (),
            Ok((file_loc, _)) => return Err(Error::new(file_loc, ErrorKind::MissingSemicolon)),
            Err(e) => return Err(e),
        }
    }

    Ok(statements)
}

fn parse_expr<R: Read>(pre: &mut Option<(FileLocation, TokenStreamElement)>, ts: &mut TokenStream<R>, high_precedence: bool) -> Result<Expr, Error> {
    struct State<'a, R: Read> {
        pre: &'a mut Option<(FileLocation, TokenStreamElement)>,
        ts: &'a mut TokenStream<R>
    }
    impl<R: Read> State<'_, R> {
        fn next(&mut self) -> Option<Result<(FileLocation, TokenStreamElement), Error>>{
            self.pre.take().map(Ok).or_else(|| self.ts.next())
        }
        #[must_use = "This will be just like calling `next` in case there is an error"]
        fn peek(&mut self) -> Option<Result<&TokenStreamElement, Error>> {
            let State {
                pre, ts
            } = self;
            pre.as_ref().map(|(_, tse)| Ok(tse)).or_else(move || ts.peek())
        }
    }
    
    let res = {
        let mut s = State {pre, ts};

        use self::SyntaxOp::*;
        use TokenStreamElement::*;
        let (file_loc, tse) = s.next().flatten()?;

        let mut expr = match tse {
            SyntaxOp(End) => Err(Error::new(file_loc, ErrorKind::ExpectedToken)),
            // variable
            Identifier(ident, None) => {
                match s.peek().flatten()? {
                    TokenStreamElement::SyntaxOp(StartParen) => {
                        todo!("function call of {}", ident)
                    }
                    _ => Ok(Expr::Identifer(ident)),
                }
            }
            Identifier(ident, Some((_pred, OperandMode::Prefix))) => Ok(Expr::Call(ident, vec![parse_expr(&mut s.pre, &mut s.ts, true)?])),
            Identifier(_ident, Some((_, OperandMode::Infix))) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
            NumberLiteral(n) => {
                match (n.parse::<u64>(), n.parse::<i64>(), n.parse::<f64>()) {
                    (Ok(u), Ok(_), _) => Ok(Expr::Literal(Literal::AmbigInt(u))),
                    (Ok(u), Err(_), _) => Ok(Expr::Literal(Literal::Uint(u))),
                    (Err(_), Ok(i), _) => Ok(Expr::Literal(Literal::Int(i))),
                    (_, _, Ok(f)) => Ok(Expr::Literal(Literal::Float(f))),
                    (Err(_), Err(_), Err(_)) => Err(Error::new(file_loc, ErrorKind::MalformedNumber)),
                }
            }
            StringLiteral(s) => Ok(Expr::Literal(Literal::String(s))),
            SyntaxOp(Equal | Member | WithType | Comma | EndParen | EndBlock | EndIndex) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
            SyntaxOp(EndType | StartType) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
            SyntaxOp(StartIndex) => todo!(),
            SyntaxOp(StartParen) => todo!(),
            SyntaxOp(StartBlock) => todo!(),
            SyntaxOp(Ref) => Ok(Expr::Ref(Box::new(parse_expr(&mut s.pre, &mut s.ts, true)?))),
            SyntaxOp(Deref) => Ok(Expr::Deref(Box::new(parse_expr(&mut s.pre, &mut s.ts, true)?))),
            SyntaxOp(Return) => Ok(Expr::Return(Box::new(parse_expr(&mut s.pre, &mut s.ts, false)?))),
            Keyword(_) => todo!(),
        };

        let mut prefix_stack = Vec::new();

        loop { match (high_precedence, s.peek().flatten()?) {
            (false, Identifier(_, Some((_, OperandMode::Infix)))) => {
                let (_file_loc, next) = s.next().flatten()?;
                match next {
                    Identifier(ident, Some((pred, OperandMode::Infix))) => {
                        prefix_stack.push((pred, ident, parse_expr(&mut s.pre, &mut s.ts, true)?));
                    }
                    _ => unreachable!(),
                }
            }
            (_, SyntaxOp(Member)) => {
                let _ = s.next();
                let (file_loc, next) = s.next().flatten()?;
                match next {
                    // TODO Handle member function calling syntax
                    Identifier(s, None) | NumberLiteral(s) => {
                        expr = Ok(Expr::Member(Box::new(expr?), s));
                    }
                    _ => break Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
                }
            }
            _ => break Ok(prefix_stack_unroll(expr?, prefix_stack)),
        } }
    };

    debug_assert!(pre.is_none());

    res
}

fn prefix_stack_unroll(main_expr: Expr, mut prefix_stack: Vec<(u8, String, Expr)>) -> Expr {
    if prefix_stack.is_empty() {
        main_expr
    } else {
        let (split_i, _) = prefix_stack
            .iter()
            .map(|&(n, _, _)| n)
            .enumerate()
            .fold((0, u8::MAX), |(min_i, min_n), (i, n)| if n < min_n { (i, n) } else {(min_i, min_n)});
    
        let mut second_prefix_stack = prefix_stack.split_off(split_i);
        let (_max_pred, func_ident, second_expr) = second_prefix_stack.remove(0);

        Expr::Call(func_ident, vec![prefix_stack_unroll(main_expr, prefix_stack), prefix_stack_unroll(second_expr, second_prefix_stack)])
    }
}

pub(crate) fn is_op(s: &str) -> bool {
    SyntaxOp::from_str(s).is_some()
        || BINARY_OPS.contains_key(&s)
        || UNARY_OPS.contains_key(&s)
}

fn token_stream<R: Read>(read: R) -> TokenStream<R> {
    let tokeniser = Tokeniser::from_char_iter(read.chars_iterator());

    TokenStream {
        tokeniser,
        peeked: None,
        last_token_kind: LastTokenKind::Bracket,
    }
}

pub fn compile<R: Read>(read: R) -> Result<Vec<Statement>, Error> {
    tree(token_stream(read))
}

pub fn test<R: Read>(read: R) {
    for elem in token_stream(read) {
        let (_file_loc, elem) = elem.unwrap();
        print!("{}{}", elem, if elem.is_end() { "\n" } else { " " });
    }
}