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
    Int(i64),
    Uint(u64),
    Float(f64),
    Bool(bool),
    Unit,
    None,
    Some(Box<Expr>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Call(Type, String, Vec<Expr>),
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
    Reassign(String, Expr),
    Function(String, Vec<(String, Type)>, Type, Expr),
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
