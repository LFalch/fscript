pub mod ast;
pub mod check;
pub mod state;

use crate::source::ast::Statements as SourceStatements;
use crate::source::ast::Type as TypeHint;
use crate::types::Type;
use self::ast::{Statements as TypedStatements, Statement as TypedStatement, Expr, Type as ProperType};
use self::check::{check_statements, ReturnType, TypeError};
use self::state::{SymbolTable, TypeCollection};

pub fn type_check(stmnts: SourceStatements, function_table: impl IntoIterator<Item=(String, (Type, Type))>) -> Result<(ReturnType, TypedStatements), TypeError> {
    let mut st = SymbolTable::new();

    let mut tv = TypeCollection::new();

    for (n, (arg_type, ret_type)) in function_table {
        let arg = tv.convert(TypeHint::Named(arg_type));
        let rt = tv.convert(TypeHint::Named(ret_type));
        st.add_fn(n, arg, rt);
    }

    check_statements(stmnts, &mut st, &mut tv).map(|r| type_specifier(r, &mut tv))
}

/// Make types as specific as possible
fn type_specifier((mut rt, mut stmnts): (ReturnType, TypedStatements), tv: &mut TypeCollection) -> (ReturnType, TypedStatements) {
    type_specify_type(&mut rt, tv);
    type_specify_statements(&mut stmnts, tv);

    (rt, stmnts)
}

fn type_specify_type(t: &mut ProperType, tv: &mut TypeCollection) {
    // TODO: this function sucks less now
    fn collapse_int_uint(t: &mut ProperType) {
        match t {
            ProperType::TypeVariable(_, ts) if ts == &[ProperType::Int, ProperType::Uint] => *t = ProperType::Int,
            ProperType::Option(t) | ProperType::Reference(t)
            | ProperType::MutReference(t) | ProperType::Array(t) => collapse_int_uint(t),
            ProperType::Tuple(v) => v.into_iter().for_each(|t| collapse_int_uint(t)),
            ProperType::Function(t, rt) => {collapse_int_uint(t); collapse_int_uint(rt);},
            Type::Int | Type::Uint | Type::Unit | Type::Bool | Type::Float | Type::String | Type::TypeVariable(_, _) => (),
        }
    }

    *t = tv.lookup_by_type(&t);
    collapse_int_uint(t);
}

fn type_specify_statements(stmnts: &mut TypedStatements, tv: &mut TypeCollection) {
    use self::TypedStatement::*;

    for stmnt in stmnts {
        match stmnt {
            VarAssign(_, t, e) => {
                type_specify_type(t, tv);
                type_specify_expr(e, tv);
            }
            ConstAssign(_, t, e) => {
                type_specify_type(t, tv);
                type_specify_expr(e, tv);
            }
            Reassign(_, e) => {
                type_specify_expr(e, tv);
            }
            Function(_, _, t, rt, e) => {
                type_specify_type(t, tv);
                type_specify_type(rt, tv);
                type_specify_expr(e, tv);
            }
            DiscardExpr(t, e) => {
                type_specify_type(t, tv);
                type_specify_expr(e, tv);
            }
            Return(rt, e) => {
                type_specify_type(rt, tv);
                type_specify_expr(e, tv);
            }
        }
    }
}

/// Make types as specific as possible
fn type_specify_expr(expr: &mut Expr, tv: &mut TypeCollection) {
    use self::Expr::*;
    match expr {
        Some(e) => type_specify_expr(e, tv),
        Ref(e) => type_specify_expr(e, tv),
        MutRef(e) => type_specify_expr(e, tv),
        Deref(e) => type_specify_expr(e, tv),
        Array(es) => es.iter_mut().for_each(|e| type_specify_expr(e, tv)),
        Tuple(es) => es.iter_mut().for_each(|e| type_specify_expr(e, tv)),
        Call(rt, _, e) => {
            type_specify_type(rt, tv);
            type_specify_expr(e, tv);
        }
        Member(t, e, _) => {
            type_specify_type(t, tv);
            type_specify_expr(e, tv);
        }
        Index(e, e2) => {
            type_specify_expr(e, tv);
            type_specify_expr(e2, tv);
        }
        Block(rt, stmnts) => {
            type_specify_type(rt, tv);
            type_specify_statements(stmnts, tv);
        }
        If(e, e2, e3) => {
            type_specify_expr(e, tv);
            type_specify_expr(e2, tv);
            type_specify_expr(e3, tv);
        }
        While(e, e2) => {
            type_specify_expr(e, tv);
            type_specify_expr(e2, tv);
        }
        Identifer(_) |
        String(_) |
        Int(_) |
        Float(_) |
        Bool(_) |
        Unit |
        None => (),
    }
}
