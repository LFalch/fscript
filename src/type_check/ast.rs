use std::fmt::{self, Display};

pub use crate::source::ast::Type as TypeHint;
pub use crate::types::Type as NamedType;

use super::check::ReturnType;

pub type TypeVariableName = u64;

pub type Type = NamedType<TypeVariableName>;

pub type Statements = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Expr {
    Identifer(String),
    String(String),
    Int(u64),
    Float(f64),
    Bool(bool),
    Unit,
    None,
    Some(Box<Expr>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Call(ReturnType, String, Box<Expr>),
    Ref(Box<Expr>),
    MutRef(Box<Expr>),
    Deref(Box<Expr>),
    Member(Type, Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Block(ReturnType, Statements),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    VarAssign(String, Type, Expr),
    ConstAssign(String, Type, Expr),
    Reassign(ReassignLhs, Expr),
    Function(String, Vec<(String, Type)>, ReturnType, Expr),
    DiscardExpr(Type, Expr),
    Return(ReturnType, Expr),
}

impl Statement {
    pub fn get_type(&self) -> Type {
        match self {
            Statement::DiscardExpr(t, _) => t.clone(),
            _ => Type::Unit,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ReassignLhs {
    Identifier(String),
    Member(Box<Self>, String),
    Index(Box<Self>, Expr),
    Deref(Box<Self>),
}

impl Display for ReassignLhs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::ReassignLhs::*;

        match self {
            Identifier(s) => write!(f, "{s}"),
            Deref(e) => write!(f, "*({e})"),
            Member(e, i) => write!(f, "({e}).{i}"),
            Index(e, i) => write!(f, "({e}).[{i}]"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Expr::*;

        match self {
            Identifer(s) => write!(f, "{s}"),
            None => write!(f, "None"),
            Unit => write!(f, "()"),
            Bool(b) => write!(f, "{b}"),
            Float(n) => write!(f, "{n}"),
            Int(i) => write!(f, "{i}"),
            String(s) => write!(f, "{s:?}"),
            Some(e) => write!(f, "Some({e})"),
            Array(a) => {
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
            Tuple(a) => {
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
            Call(t, s, exp) => {
                if let Tuple(_) | Unit = &**exp {
                    write!(f, "{s}{exp}")?;
                } else {
                    write!(f, "{s}({exp})")?;
                }
                write!(f, ") : {t}")
            }
            Ref(e) => write!(f, "&({e})"),
            MutRef(e) => write!(f, "@({e})"),
            Deref(e) => write!(f, "*({e})"),
            Member(t, e, i) => write!(f, "(({e}).{i} : {t})"),
            Index(e, i) => write!(f, "({e}).[{i}]"),
            Block(rt, s) => {
                write!(f, "{{\n")?;
                for s in s {
                    write!(f, "\t{s};\n")?;
                }
                write!(f, "}} (-> {rt})")
            }
            If(cond, if_true, if_false) => write!(f, "if {cond}: {if_true} else {if_false}."),
            While(cond, body) => write!(f, "while {cond}: {body}."),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Statement::*;
        match self {
            VarAssign(n, t, e) => write!(f, "var {n}: {t} = {e}"),
            ConstAssign(n, t, e) => write!(f, "let {n}: {t} = {e}"),
            Reassign(n, e) => write!(f, "{n} = {e}"),
            Function(n, args, rt, e) => {
                write!(f, "fn {n}(")?;
                let mut comma_first = false;
                for (n, t) in args {
                    if comma_first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{n}: {t}")?;
                    comma_first = true;
                }
                write!(f, ") -> {rt}: {e}")
            }
            DiscardExpr(t, e) => write!(f, "{e}  (: {t})"),
            Return(rt, e) => write!(f, "-> {e}  : {rt}"),
        }
    }
}
