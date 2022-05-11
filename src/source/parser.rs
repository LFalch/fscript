use std::{
    io::Read,
};

use lazy_static::lazy_static;
use collect_result::CollectResult;

use crate::stack_table;
use crate::{
    source::{
        tokeniser::FileLocation,
        token_stream::{Keyword, OperandMode, SyntaxOp, TokenStream, TokenStreamElement},
        ast::*,
        error::{Error, ErrorKind, FlattenToResult}
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

#[inline(always)]
/// 
pub(super) fn tree<R: Read>(mut ts: TokenStream<R>) -> Result<Statements, Error> {
    let ret = tree_mut(&mut ts)?;
    match ts.next() {
        Some(Ok((file_loc,_ ))) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
        Some(Err(e)) => Err(e),
        None => Ok(ret),
    }
}

fn tree_mut<R: Read>(ts: &mut TokenStream<R>) -> Result<Statements, Error> {
    let mut statements = Vec::new();

    loop {
        use self::Keyword::*;
        use self::SyntaxOp::*;
        use TokenStreamElement::*;
        let (file_loc, tse) = match ts.next() {
            Some(tse) => tse?,
            None => break,
        };

        if let SyntaxOp(EndBlock) = tse {
            break
        }

        statements.push(match tse {
            SyntaxOp(Keyword(Let)) => {
                if let (file_loc, Identifier(ident, None)) = ts.next().flatten(file_loc)? {
                    if let (file_loc, SyntaxOp(Equal)) = ts.next().flatten(file_loc)? {
                        Statement::ConstAssign(ident, None, parse_expr(file_loc, &mut None, ts, false)?)
                    } else {
                        return Err(Error::new(file_loc, ErrorKind::ExpectedToken))
                    }
                } else {
                    return Err(Error::new(file_loc, ErrorKind::ExpectedToken))
                }
            }
            SyntaxOp(Keyword(Var)) => {
                if let (file_loc, Identifier(ident, None)) = ts.next().flatten(file_loc)? {
                    if let (file_loc, SyntaxOp(Equal)) = ts.next().flatten(file_loc)? {
                        Statement::VarAssign(ident, None, parse_expr(file_loc, &mut None, ts, false)?)
                    } else {
                        return Err(Error::new(file_loc, ErrorKind::ExpectedToken))
                    }
                } else {
                    return Err(Error::new(file_loc, ErrorKind::ExpectedToken))
                }
            }
            SyntaxOp(Keyword(Fn)) => {
                if let (file_loc, Identifier(ident, None)) = ts.next().flatten(file_loc)? {
                    Statement::ConstAssign(ident, None, parse_expr(file_loc, &mut Some((file_loc, SyntaxOp(Keyword(Fn)))), ts, false)?)
                } else {
                    return Err(Error::new(file_loc, ErrorKind::ExpectedToken))
                }
            }
            // rn we only allow an lvalue to an identifier
            // this should probably just be any expression at all
            Identifier(ident, None) => {
                match ts.peek().flatten(file_loc)? {
                    SyntaxOp(Equal) => {ts.next(); Statement::Reassign(ident, parse_expr(file_loc, &mut None, ts, false)?)},
                    SyntaxOp(End) => Statement::DiscardExpr(Expr::Identifer(file_loc, ident)),
                    _ => Statement::DiscardExpr(parse_expr(file_loc, &mut Some((file_loc, Identifier(ident, None))), ts, false)?),
                }
            }
            SyntaxOp(Return) => Statement::Return(parse_expr(file_loc, &mut None, ts, false)?),
            SyntaxOp(End) => return Err(Error::new(file_loc, ErrorKind::EmptyStatement)),
            pre => Statement::DiscardExpr(parse_expr(file_loc, &mut Some((file_loc, pre)), ts, false)?),
        });

        match ts.next().flatten(file_loc) {
            Ok((_, SyntaxOp(End))) => (),
            Ok((file_loc, _)) => return Err(Error::new(file_loc, ErrorKind::MissingSemicolon)),
            Err(e) => return Err(e),
        }
    }

    Ok(statements)
}

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

fn parse_expr<R: Read>(file_loc: FileLocation, pre: &mut Option<(FileLocation, TokenStreamElement)>, ts: &mut TokenStream<R>, high_precedence: bool) -> Result<Expr, Error> {
    let res = {
        let mut s = State {pre, ts};

        use self::SyntaxOp::*;
        use self::Keyword::*;
        use TokenStreamElement::*;
        let (file_loc, tse) = s.next().flatten_with(file_loc, ErrorKind::ExpectedExpression)?;

        let mut expr = match tse {
            SyntaxOp(End) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
            // variable
            Identifier(ident, None) => {
                match s.peek().flatten(file_loc)? {
                    TokenStreamElement::SyntaxOp(StartParen) => {
                        s.next();
                        let (args, _trailing_comma) = comma_seperated(file_loc, &mut s.pre, &mut s.ts)?;
                        match s.next().flatten(file_loc)? {
                            (_, TokenStreamElement::SyntaxOp(EndParen)) => Ok(Expr::Call(file_loc, ident, args)),
                            (file_loc, _ ) => Err(Error::new(file_loc, ErrorKind::MissingEndParen))
                        }
                    }
                    _ => Ok(Expr::Identifer(file_loc, ident)),
                }
            }
            Identifier(ident, Some((_pred, OperandMode::Prefix))) => Ok(Expr::Call(file_loc, ident, vec![parse_expr(file_loc, &mut s.pre, &mut s.ts, true)?])),
            Identifier(_ident, Some((_, OperandMode::Infix))) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
            NumberLiteral(n) => {
                match (n.parse::<u64>(), n.parse::<i64>(), n.parse::<f64>()) {
                    (Ok(u), Ok(_), _) => Ok(Expr::Constant(file_loc, Primitive::AmbigInt(u))),
                    (Ok(u), Err(_), _) => Ok(Expr::Constant(file_loc, Primitive::Uint(u))),
                    (Err(_), Ok(i), _) => Ok(Expr::Constant(file_loc, Primitive::Int(i))),
                    (_, _, Ok(f)) => Ok(Expr::Constant(file_loc, Primitive::Float(f))),
                    (Err(_), Err(_), Err(_)) => Err(Error::new(file_loc, ErrorKind::MalformedNumber)),
                }
            }
            StringLiteral(s) => Ok(Expr::Constant(file_loc, Primitive::String(s))),
            SyntaxOp(Equal | Member | WithType | Comma | EndParen | EndBlock | EndIndex) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
            SyntaxOp(EndType | StartType) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
            SyntaxOp(StartIndex) => {
                let (elems, _trailing_comma) = comma_seperated(file_loc, &mut s.pre, &mut s.ts)?;
                match s.next().flatten(file_loc)? {
                    (_, TokenStreamElement::SyntaxOp(EndIndex)) => Ok(Expr::Array(file_loc, elems)),
                    (file_loc, _ ) => Err(Error::new(file_loc, ErrorKind::MissingEndParen))
                }
            }
            SyntaxOp(StartParen) => {
                let (args, trailing_comma) = comma_seperated(file_loc, &mut s.pre, &mut s.ts)?;
                match s.next().flatten(file_loc)? {
                    (_, TokenStreamElement::SyntaxOp(EndParen)) => {
                        match (args.len(), trailing_comma) {
                            (0, false) => Ok(Expr::Constant(file_loc, Primitive::Unit)),
                            (1, false) => Ok({
                                let mut args = args;
                                args.pop().unwrap()
                            }),
                            _ => Ok(Expr::Tuple(file_loc, args))
                        }
                    }
                    (file_loc, _ ) => Err(Error::new(file_loc, ErrorKind::MissingEndParen))
                }
            }
            SyntaxOp(StartBlock) => tree_mut(&mut s.ts).map(|stmnts| Expr::Block(file_loc, stmnts)),
            SyntaxOp(Ref) => Ok(Expr::Ref(file_loc, Box::new(parse_expr(file_loc, &mut s.pre, &mut s.ts, true)?))),
            SyntaxOp(MutRef) => Ok(Expr::MutRef(file_loc, Box::new(parse_expr(file_loc, &mut s.pre, &mut s.ts, true)?))),
            SyntaxOp(Deref) => Ok(Expr::Deref(file_loc, Box::new(parse_expr(file_loc, &mut s.pre, &mut s.ts, true)?))),
            SyntaxOp(Return) => Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
            SyntaxOp(Keyword(Fn)) => {
                if let (file_loc, SyntaxOp(StartParen)) = s.next().flatten(file_loc)? {
                    let (args, _trailing_comma) = comma_seperated(file_loc, &mut None, &mut s.ts)?;
                    let args: Vec<_> = args
                        .into_iter()
                        .map(|arg| match arg {
                            Expr::Identifer(_fl, s) => Ok(s),
                            _ => Err(Error::new(file_loc, ErrorKind::UnexpectedToken))
                        })
                        .collect_result()?;
                    if let (file_loc, SyntaxOp(EndParen)) = s.next().flatten(file_loc)? {
                        parse_expr(file_loc, &mut None, &mut s.ts, false).map(|f| Expr::Function(file_loc, args, Box::new(f)))
                    } else {
                        Err(Error::new(file_loc, ErrorKind::UnexpectedToken))
                    }
                } else {
                    Err(Error::new(file_loc, ErrorKind::UnexpectedToken))
                }
            }
            SyntaxOp(Keyword(_)) => todo!(),
        };

        let mut prefix_stack = Vec::new();

        loop { match (high_precedence, s.peek().flatten(file_loc)?) {
            (false, Identifier(_, Some((_, OperandMode::Infix)))) => {
                let (file_loc, next) = s.next().flatten(file_loc)?;
                match next {
                    Identifier(ident, Some((pred, OperandMode::Infix))) => {
                        prefix_stack.push((pred, file_loc, ident, parse_expr(file_loc, &mut s.pre, &mut s.ts, true)?));
                    }
                    _ => unreachable!(),
                }
            }
            (_, SyntaxOp(Member)) => {
                let _ = s.next();
                let (file_loc, next) = s.next().flatten(file_loc)?;
                match next {
                    // TODO Handle member function calling syntax
                    Identifier(s, None) | NumberLiteral(s) => {
                        expr = Ok(Expr::Member(file_loc, Box::new(expr?), s));
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

fn comma_seperated<R: Read>(file_loc: FileLocation, pre: &mut Option<(FileLocation, TokenStreamElement)>, ts: &mut TokenStream<R>) -> Result<(Vec<Expr>, bool), Error> {
    let mut s = State { pre, ts};
    let mut exprs = Vec::new();

    match s.peek().flatten(file_loc)? {
        TokenStreamElement::SyntaxOp(SyntaxOp::Comma) => {
            s.next();
            return Ok((exprs, true))
        }
        TokenStreamElement::SyntaxOp(SyntaxOp::End | SyntaxOp::EndParen
            | SyntaxOp::EndIndex | SyntaxOp::EndBlock | SyntaxOp::EndType) => return Ok((exprs, false)),
        _ => ()
    }

    let trailing_comma = loop {
        // Finding element or end of list
        match s.peek().flatten(file_loc)? {
            TokenStreamElement::SyntaxOp(SyntaxOp::Comma) => {s.next(); return Err(Error::new(file_loc, ErrorKind::UnexpectedToken))},
            TokenStreamElement::SyntaxOp(SyntaxOp::End | SyntaxOp::EndParen
                | SyntaxOp::EndIndex | SyntaxOp::EndBlock | SyntaxOp::EndType) => break false,
            _ => exprs.push(parse_expr(file_loc, &mut s.pre, &mut s.ts, false)?),
        }
        // Checking for comma or end of list
        match s.peek().flatten(file_loc)? {
            TokenStreamElement::SyntaxOp(SyntaxOp::Comma) => {s.next();}
            TokenStreamElement::SyntaxOp(SyntaxOp::End | SyntaxOp::EndParen
                | SyntaxOp::EndIndex | SyntaxOp::EndBlock | SyntaxOp::EndType) => break false,
            _ => return Err(Error::new(file_loc, ErrorKind::UnexpectedToken)),
        }
    };
    Ok((exprs, trailing_comma))
}

fn prefix_stack_unroll(main_expr: Expr, mut prefix_stack: Vec<(u8, FileLocation, String, Expr)>) -> Expr {
    if prefix_stack.is_empty() {
        main_expr
    } else {
        let (split_i, _) = prefix_stack
            .iter()
            .map(|&(n, _, _, _)| n)
            .enumerate()
            .rfold((0, u8::MAX), |(min_i, min_n), (i, n)| if n < min_n { (i, n) } else {(min_i, min_n)});

        let mut second_prefix_stack = prefix_stack.split_off(split_i);
        let (_max_pred, file_loc, func_ident, second_expr) = second_prefix_stack.remove(0);

        Expr::Call(file_loc, func_ident, vec![prefix_stack_unroll(main_expr, prefix_stack), prefix_stack_unroll(second_expr, second_prefix_stack)])
    }
}

pub(crate) fn is_op(s: &str) -> bool {
    SyntaxOp::from_str(s).is_some()
        || BINARY_OPS.contains_key(&s)
        || UNARY_OPS.contains_key(&s)
}
