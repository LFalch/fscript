use crate::types::Type;
use super::tokeniser::FileLocation;

pub type Statements = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Expr {
    Identifer(FileLocation, String),
    Constant(FileLocation, Primitive),
    Some(FileLocation, Box<Expr>),
    Array(FileLocation, Vec<Expr>),
    Tuple(FileLocation, Vec<Expr>),
    Call(FileLocation, String, Vec<Expr>),
    Ref(FileLocation, Box<Expr>),
    MutRef(FileLocation, Box<Expr>),
    Deref(FileLocation, Box<Expr>),
    Member(FileLocation, Box<Expr>, String),
    Index(FileLocation, Box<Expr>, Box<Expr>),
    Block(FileLocation, Statements),
    Function(FileLocation, Vec<String>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    VarAssign(String, Option<Type>, Expr),
    ConstAssign(String, Option<Type>, Expr),
    Reassign(String, Expr),
    DiscardExpr(Expr),
    Return(Expr),
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