use super::FileSpan;

use crate::types::Type as ConcreteType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Inferred,
    Concrete(ConcreteType),
}

pub type Statements = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Expr {
    Identifer(FileSpan, String),
    Constant(FileSpan, Primitive),
    Some(FileSpan, Box<Expr>),
    Array(FileSpan, Vec<Expr>),
    Tuple(FileSpan, Vec<Expr>),
    Call(FileSpan, String, Vec<Expr>),
    Ref(FileSpan, Box<Expr>),
    MutRef(FileSpan, Box<Expr>),
    Deref(FileSpan, Box<Expr>),
    Member(FileSpan, Box<Expr>, String),
    Index(FileSpan, Box<Expr>, Box<Expr>),
    Block(FileSpan, Statements),
    If(FileSpan, Box<Expr>, Box<Expr>, Box<Expr>),
    While(FileSpan, Box<Expr>, Box<Expr>),
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
pub enum Statement<T = Type> {
    VarAssign(FileSpan, String, T, Expr),
    ConstAssign(FileSpan, String, T, Expr),
    Reassign(FileSpan, String, Expr),
    Function(FileSpan, String, Vec<(String, T)>, Expr),
    DiscardExpr(Expr),
    Return(FileSpan, Expr),
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