use super::FileSpan;

use crate::types::{Type as NamedType};

use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Inferred,
    Named(NamedType),
    Tuple(Vec<Self>),
}

impl Type {
    pub fn tuple(mut ts: Vec<Self>) -> Self {
        match ts.len() {
            0 => Type::Named(NamedType::Unit),
            1 => ts.pop().unwrap(),
            _ => Type::Tuple(ts),
        }
    }
    pub fn is_inferred(&self) -> bool {
        match self {
            Type::Inferred => true,
            Type::Named(_) => false,
            Type::Tuple(ts) => ts.iter().all(Type::is_inferred)
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Inferred => "'_".fmt(f),
            Type::Named(t) => t.fmt(f),
            Type::Tuple(ts) => {
                write!(f, "(")?;
                let mut comma_first = false;
                for t in ts {
                    if comma_first {
                        write!(f, ", ")?;
                    }
                    comma_first = true;
                    write!(f, "{t}")?;
                }
                write!(f, ")")
            }
        }
    }
}

pub type Statements = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Expr {
    Identifer(FileSpan, String),
    Constant(FileSpan, Primitive),
    Some(FileSpan, Box<Expr>),
    Array(FileSpan, Vec<Expr>),
    Tuple(FileSpan, Vec<Expr>),
    Call(FileSpan, String, Box<Expr>),
    Ref(FileSpan, Box<Expr>),
    MutRef(FileSpan, Box<Expr>),
    Deref(FileSpan, Box<Expr>),
    Member(FileSpan, Box<Expr>, String),
    Index(FileSpan, Box<Expr>, Box<Expr>),
    Block(FileSpan, Statements),
    If(FileSpan, Box<Expr>, Box<Expr>, Box<Expr>),
    While(FileSpan, Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Expr::*;

        match self {
            Identifer(_, s) => write!(f, "{s}"),
            Constant(_, p) => write!(f, "{p}"),
            Some(_, e) => write!(f, "Some({e})"),
            Array(_, a) => {
                write!(f, "[")?;
                let mut comma_first = false;
                for e in a {
                    if comma_first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{e}")?;
                    comma_first = true;
                }
                write!(f, "]")
            }
            Tuple(_, a) => {
                write!(f, "(")?;
                let mut comma_first = false;
                for e in a {
                    if comma_first {
                        write!(f, ", ")?;
                    }
                    comma_first = true;
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Call(_, s, exp) => {
                if let Tuple(_, _) | Constant(_, Primitive::Unit) = &**exp {
                    write!(f, "{s}{exp}")
                } else {
                    write!(f, "{s}({exp})")
                }
            }
            Ref(_, e) => write!(f, "&({e})"),
            MutRef(_, e) => write!(f, "@({e})"),
            Deref(_, e) => write!(f, "*({e})"),
            Member(_, e, i) => write!(f, "({e}).{i}"),
            Index(_, e, i) => write!(f, "({e}).[{i}]"),
            Block(_, s) => {
                write!(f, "{{\n")?;
                for s in s {
                    write!(f, "\t{s};\n")?;
                }
                write!(f, "}}")
            }
            If(_, cond, if_true, if_false) => write!(f, "if {cond}: {if_true} else {if_false}."),
            While(_, cond, body) => write!(f, "while {cond}: {body}."),
        }
    }
}

impl Expr {
    pub fn file_span(&self) -> FileSpan {
        use self::Expr::*;
        match *self {
            Expr::Identifer(fs, _)
            | Expr::Constant(fs, _)
            | Some(fs, _)
            | Array(fs, _)
            | Tuple(fs, _)
            | Call(fs, _, _)
            | Ref(fs, _)
            | MutRef(fs, _)
            | Deref(fs, _)
            | Member(fs, _, _)
            | Index(fs, _, _)
            | Block(fs, _)
            | If(fs, _, _, _)
            | While(fs, _, _)
             => fs,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ReassignLhs {
    Identifier(FileSpan, String),
    Member(FileSpan, Box<Self>, String),
    Index(FileSpan, Box<Self>, Expr),
    Deref(FileSpan, Box<Self>),
}

impl ReassignLhs {
    pub fn from_expr(lhs: Expr) -> Option<Self> {
        Some(match lhs {
            Expr::Identifer(fs, s) => Self::Identifier(fs, s),
            Expr::Member(fs, e, s) => Self::Member(fs, Box::new(Self::from_expr(*e)?), s),
            Expr::Index(fs, e, index) => Self::Index(fs, Box::new(Self::from_expr(*e)?), *index),
            Expr::Deref(fs, e) => Self::Deref(fs, Box::new(Self::from_expr(*e)?)),
            _ => return None,
        })
    }
    pub fn file_span(&self) -> FileSpan {
        match *self {
            Self::Identifier(fs, _)
            | Self::Deref(fs, _)
            | Self::Member(fs, _, _)
            | Self::Index(fs, _, _)
             => fs,
        }
    }
}

impl Display for ReassignLhs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::ReassignLhs::*;

        match self {
            Identifier(_, s) => write!(f, "{s}"),
            Deref(_, e) => write!(f, "*({e})"),
            Member(_, e, i) => write!(f, "({e}).{i}"),
            Index(_, e, i) => write!(f, "({e}).[{i}]"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Binding {
    Name(String),
    Tuple(Vec<Binding>),
    Unit
}

impl Binding {
    pub fn tuple(mut bs: Vec<Self>) -> Self {
        match bs.len() {
            0 => Self::Unit,
            1 => bs.pop().unwrap(),
            _ => Self::Tuple(bs)
        }
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Binding::Name(n) if f.alternate() => write!(f, "({n})"),
            Binding::Name(n) => write!(f, "{n}"),
            Binding::Unit => write!(f, "()"),
            Binding::Tuple(bs) => {
                write!(f, "(")?;
                let mut comma_first = false;
                for e in bs {
                    if comma_first {
                        write!(f, ", ")?;
                    }
                    comma_first = true;
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    VarAssign(FileSpan, Binding, Type, Expr),
    ConstAssign(FileSpan, Binding, Type, Expr),
    Reassign(FileSpan, ReassignLhs, Expr),
    Function(FileSpan, String, Binding, Type, Expr),
    DiscardExpr(Expr),
    Return(FileSpan, Expr),
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Statement::*;
        match self {
            VarAssign(_, b, Type::Inferred, e) => write!(f, "var {b} = {e}"),
            VarAssign(_, b, nt, e) => write!(f, "var {b}: {nt} = {e}"),
            ConstAssign(_, b, Type::Inferred, e) => write!(f, "let {b} = {e}"),
            ConstAssign(_, b, nt, e) => write!(f, "let {b}: {nt} = {e}"),
            Reassign(_, n, e) => write!(f, "{n} = {e}"),
            Function(_, n, arg_names, arg_type, e) => {
                write!(f, "fn {n}{arg_names:#}: {arg_type} {e}")
            }
            DiscardExpr(e) => write!(f, "{e}"),
            Return(_, e) => write!(f, "-> {e}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Primitive {
    String(String),
    AmbigInt(u64),
    Int(i64),
    Uint(u64),
    Float(f64),
    Bool(bool),
    Unit,
    None
}

impl Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::None => write!(f, "None"),
            Primitive::Unit => write!(f, "()"),
            Primitive::Bool(b) => write!(f, "{b}"),
            Primitive::Float(n) => write!(f, "{n}"),
            Primitive::Uint(i) => write!(f, "{i}u"),
            Primitive::Int(i) => write!(f, "{i}i"),
            Primitive::AmbigInt(i) => write!(f, "{i}"),
            Primitive::String(s) => write!(f, "{s:?}"),
        }
    }
}

impl PartialEq for Primitive {
    fn eq(&self, rhs: &Self) -> bool {
        use self::Primitive::*;
        match (self, rhs) {
            (String(a), String(b)) => a == b,
            (Int(a), Int(b)) => a == b,
            (AmbigInt(a), Int(b)) => (*a) as i64 == *b,
            (Int(a), AmbigInt(b)) => *a == (*b) as i64,
            (Uint(a) | AmbigInt(a), Uint(b) | AmbigInt(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Bool(a), Bool(b)) => a == b,
            (Unit, Unit) => true,
            (None, None) => true,
            _ => false,
        }
    }
}
