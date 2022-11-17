use std::fmt::{self, Display};

use super::ast::*;
use super::state::{TypeCollection, SymbolTable};
use crate::source::{
    FileSpan,
    ast::{
        Expr as UntypedExpr,
        Primitive,
        Statement as UntypedStatement,
        Statements as UntypedStatements,
        ReassignLhs as UntypedReassignLhs,
    },
};

pub type ReturnType = Type;


#[derive(Debug, Clone)]
pub enum TypeError {
    CannotBeDereferenced(FileSpan, Type),
    CouldNotUnifyTypes(FileSpan, Type, Type),
    NoSuchVariable(FileSpan, String),
    NotMutable(FileSpan, String),
    NoSuchFunction(FileSpan, String),
    NotAFunction(FileSpan, String, Type),
    IncorrectIndexing(FileSpan, Type, Type),
    LengthOnNonArray(FileSpan, Type),
    ConditionNotBoolean(FileSpan, Type),
}

trait TypeResultExtension {
    fn fs(self, fs: FileSpan) -> Self;
}

impl<T> TypeResultExtension for Result<T, TypeError> {
    fn fs(self, fs: FileSpan) -> Self {
        match self {
            Err(TypeError::CannotBeDereferenced(_, t)) => Err(TypeError::CannotBeDereferenced(fs, t)),
            Err(TypeError::CouldNotUnifyTypes(_, t, t2)) => Err(TypeError::CouldNotUnifyTypes(fs, t, t2)),
            Err(TypeError::NoSuchVariable(_, s)) => Err(TypeError::NoSuchVariable(fs, s)),
            Err(TypeError::NotMutable(_, s)) => Err(TypeError::NotMutable(fs, s)),
            Err(TypeError::NoSuchFunction(_, s)) => Err(TypeError::NoSuchFunction(fs, s)),
            Err(TypeError::NotAFunction(_, s, t)) => Err(TypeError::NotAFunction(fs, s, t)),
            Err(TypeError::IncorrectIndexing(_, t, t2)) => Err(TypeError::IncorrectIndexing(fs, t, t2)),
            Err(TypeError::LengthOnNonArray(_, t)) => Err(TypeError::LengthOnNonArray(fs, t)),
            Err(TypeError::ConditionNotBoolean(_, t)) => Err(TypeError::ConditionNotBoolean(fs, t)),
            a @ Ok(_) => a,
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::CannotBeDereferenced(fp, t) => write!(f, ":{}:{}-{}:{} type {t} is not a reference and therefore cannot be dereferenced", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
            TypeError::CouldNotUnifyTypes(fp, t, t2) => write!(f, ":{}:{}-{}:{} could not unify types {t} and {t2}", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
            TypeError::NoSuchVariable(fp, s) => write!(f, ":{}:{}-{}:{} no variable named {s} was found", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
            TypeError::NotMutable(fp, s) => write!(f, ":{}:{}-{}:{} the variable {s} was not defined as mutable", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
            TypeError::NoSuchFunction(fp, s) => write!(f, ":{}:{}-{}:{} function {s} was not found", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
            TypeError::NotAFunction(fp, s, t) => write!(f, ":{}:{}-{}:{} name {s} was found but was not a function and instead {t}", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
            TypeError::IncorrectIndexing(fp, t, t2) => write!(f, ":{}:{}-{}:{} cannot index type {t} using {t2}, only an array can be indexed and only by a uint", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
            TypeError::LengthOnNonArray(fp, t) => write!(f, ":{}:{}-{}:{} only arrays have lengths, type {t} is not an array", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
            TypeError::ConditionNotBoolean(fp, t) => write!(f, ":{}:{}-{}:{} condition was not a boolean but {t}", fp.start.line, fp.start.col, fp.end.line, fp.end.col),
        }
    }
}

fn returns(stmnt: &Statement) -> Vec<ReturnType> {
    use self::Statement::*;
    match stmnt {
        Return(t, _) => vec![t.clone()],
        DiscardExpr(_, e) => returns_in_expr(e),
        VarAssign(_, _, e) => returns_in_expr(e),
        ConstAssign(_, _, e) => returns_in_expr(e),
        Reassign(_, e) => returns_in_expr(e),
        Function(_, _, _, _, e) => returns_in_expr(e),
    }
}

fn returns_in_expr(expr: &Expr) -> Vec<ReturnType> {
    use self::Expr::*;
    match expr {
        Block(rt, _stmnts) => vec![rt.clone()],
        Some(e) => returns_in_expr(e),
        Array(exprs) => exprs.iter().flat_map(returns_in_expr).collect(),
        Tuple(exprs) => exprs.iter().flat_map(returns_in_expr).collect(),
        // incorrect
        Call(_, _, _, _) => vec![],
        Ref(e) => returns_in_expr(e),
        MutRef(e) => returns_in_expr(e),
        Deref(e) => returns_in_expr(e),
        Member(_, e, _) => returns_in_expr(e),
        Index(e, e2) => {
            let mut v = returns_in_expr(e);
            v.extend(returns_in_expr(e2));
            v
        }
        If(e, e2, e3) => {
            let mut v = returns_in_expr(e);
            v.extend(returns_in_expr(e2));
            v.extend(returns_in_expr(e3));
            v
        }
        While(e, e2) => {
            let mut v = returns_in_expr(e);
            v.extend(returns_in_expr(e2));
            v
        }
        _ => Vec::new(),
    }
}



/// First part of the type is return type
pub fn check_statements(stmnts: UntypedStatements, st: &mut SymbolTable, tv: &mut TypeCollection) -> Result<(ReturnType, Statements), TypeError> {
    let mut typed_stmnts = Vec::with_capacity(stmnts.len());
    for stmnt in stmnts {
        match stmnt {
            UntypedStatement::VarAssign(_, b, t, e) => {
                let t = tv.convert(t);
                let fs = e.file_span();
                let (te, e) = check_exp(e, st, tv)?;
                let t = tv.unify(&t, &te).fs(fs)?;

                st.bind(&b, true, t.clone())?;

                typed_stmnts.push(Statement::VarAssign(b, t, e));
            }
            UntypedStatement::ConstAssign(_, b, t, e) => {
                let t = tv.convert(t);
                let fs = e.file_span();
                let (te, e) = check_exp(e, st, tv)?;
                let t = tv.unify(&t, &te).fs(fs)?;

                st.bind(&b, false, t.clone())?;

                typed_stmnts.push(Statement::ConstAssign(b, t, e));
            }
            UntypedStatement::Reassign(fs, lhs, e) => {
                let (lhs_t, lhs) = check_reassign_lhs(lhs, st, tv)?;
                let (te, e) = check_exp(e, st, tv)?;

                let _t = tv.unify(&lhs_t, &te).fs(fs)?;

                typed_stmnts.push(Statement::Reassign(lhs, e));
            }
            UntypedStatement::Function(fs, name, arg, arg_type, body) => {
                let mut body_st = st.clone();
                let arg_type = tv.convert(arg_type);

                body_st.bind(&arg, true, arg_type.clone())?;

                let (mut body_t, body) = check_exp(body, &mut body_st, tv)?;
                drop(body_st);

                if let Type::Unit = body_t {
                    body_t = tv.next();
                }
                
                for rt in returns_in_expr(&body) {
                    body_t = tv.unify(&body_t, &rt).fs(fs)?;
                }

                st.add(name.clone(), false, Type::Function(Box::new(arg_type.clone()), Box::new(body_t.clone())));

                typed_stmnts.push(Statement::Function(name, arg, arg_type, body_t, body));
            }
            UntypedStatement::DiscardExpr(e) => {
                let (t, e) = check_exp(e, st, tv)?;

                typed_stmnts.push(Statement::DiscardExpr(t, e));
            }
            UntypedStatement::Return(_, e) => {
                let (t, e) = check_exp(e, st, tv)?;
                typed_stmnts.push(Statement::Return(t, e));
            }
        }
    }

    let mut rt = tv.next();
    for rt_cand in typed_stmnts.iter().flat_map(returns) {
        rt = tv.unify(&rt, &rt_cand)?;
    }

    Ok((rt, typed_stmnts))
}

pub fn check_reassign_lhs(lhs: UntypedReassignLhs, st: &mut SymbolTable, tv: &mut TypeCollection) -> Result<(Type, ReassignLhs), TypeError> {
    Ok(match lhs {
        UntypedReassignLhs::Identifier(_, ident) => {
            let t = st.get_var(&ident).map(|(_, t)| t.clone()).unwrap_or_else(|| tv.next());

            (t, ReassignLhs::Identifier(ident))
        }
        UntypedReassignLhs::Deref(fs, lhs) => {
            let (ref_t, lhs) = check_reassign_lhs(*lhs, st, tv)?;

            let (generic_ref_t, inner_generic_t) = tv.next_ref();

            let _t = tv.unify(&ref_t, &generic_ref_t).map_err(|_| TypeError::CannotBeDereferenced(fs, ref_t))?;
            let t = tv.lookup_by_type(&inner_generic_t);

            (t, ReassignLhs::Deref(Box::new(lhs)))
        }
        UntypedReassignLhs::Index(_, array, index) => {
            let array_fs = array.file_span();
            let index_fs = index.file_span();

            let (array_t, array) = check_reassign_lhs(*array, st, tv)?;
            let (index_t, index) = check_exp(index, st, tv)?;

            let generic_at = Type::Array(Box::new(tv.next()));
            let array_t = tv.unify(&array_t, &generic_at).fs(array_fs)?;
            let index_t = tv.unify(&index_t, &Type::Uint).fs(index_fs)?;

            let element_t = match (array_t, index_t) {
                (Type::Array(et), Type::Uint) => et,
                _ => unreachable!(),
            };

            (*element_t, ReassignLhs::Index(Box::new(array), index))
        }
        UntypedReassignLhs::Member(_fs, _lhs, _s) => unimplemented!("no types have mutable properties yet"),
    })
}

pub fn check_exp(expr: UntypedExpr, st: &mut SymbolTable, tv: &mut TypeCollection) -> Result<(Type, Expr), TypeError> {
    Ok(match expr {
        UntypedExpr::Constant(_, Primitive::Bool(b)) => (Type::Bool, Expr::Bool(b)),
        UntypedExpr::Constant(_, Primitive::Float(f)) => (Type::Float, Expr::Float(f)),
        UntypedExpr::Constant(_, Primitive::Int(n)) => (Type::Int, Expr::Int(n as u64)),
        UntypedExpr::Constant(_, Primitive::Uint(n)) => (Type::Int, Expr::Int(n)),
        UntypedExpr::Constant(_, Primitive::AmbigInt(n)) => (tv.next_integral(), Expr::Int(n)),
        UntypedExpr::Constant(_, Primitive::None) => (Type::Option(Box::new(tv.next())), Expr::None),
        UntypedExpr::Constant(_, Primitive::String(s)) => (Type::String, Expr::String(s)),
        UntypedExpr::Constant(_, Primitive::Unit) => (Type::Unit, Expr::Unit),
        UntypedExpr::Identifer(_, ident) => {
            let mut types = Vec::new();
            for (_, t) in st.get(&ident) {
                types.push(t);
            }
            let t = if types.len() == 1 {
                types[0].clone()
            } else {
                tv.next_with(types)
            };

            (t, Expr::Identifer(ident))
        }
        UntypedExpr::Some(_, expr) => {
            let (t, expr) = check_exp(*expr, st, tv)?;
            (Type::Option(Box::new(t)), Expr::Some(Box::new(expr)))
        },
        UntypedExpr::Ref(_, expr) => {
            let (t, expr) = check_exp(*expr, st, tv)?;
            (Type::Reference(Box::new(t)), Expr::Ref(Box::new(expr)))
        },
        UntypedExpr::MutRef(_, expr) => {
            let (t, expr) = check_exp(*expr, st, tv)?;
            (Type::MutReference(Box::new(t)), Expr::MutRef(Box::new(expr)))
        },
        UntypedExpr::Deref(fs, expr) => {
            let (ref_t, expr) = check_exp(*expr, st, tv)?;

            let (generic_ref_t, inner_generic_t) = tv.next_ref();

            let _t = tv.unify(&ref_t, &generic_ref_t).map_err(|_| TypeError::CannotBeDereferenced(fs, ref_t))?;
            let t = tv.lookup_by_type(&inner_generic_t);

            (t, Expr::Deref(Box::new(expr)))
        }
        UntypedExpr::Block(_, stmnts) => {
            let mut st = st.clone();

            let (rt, stmnts) = check_statements(stmnts, &mut st, tv)?;
            drop(st);

            let t = stmnts.last().map_or(Type::Unit, |s| s.get_type());

            (t, Expr::Block(rt, stmnts))
        }
        UntypedExpr::Array(fs, arr) => {
            let mut t = tv.next();
            let mut typed_arr = Vec::with_capacity(arr.len());
            
            for expr in arr {
                let (t2, e) = check_exp(expr, st, tv)?;
                typed_arr.push(e);

                t = tv.unify(&t, &t2).fs(fs)?;
            }

            (Type::Array(Box::new(t)), Expr::Array(typed_arr))
        },
        UntypedExpr::Tuple(_, exprs) => {
            let mut types = Vec::with_capacity(exprs.len());
            let mut elements = Vec::with_capacity(exprs.len());

            for expr in exprs {
                let (t, v) = check_exp(expr, st, tv)?;

                types.push(t);
                elements.push(v);
            }

            (Type::Tuple(types), Expr::Tuple(elements))
        }
        UntypedExpr::Call(fs, name, arg) => {
            let mut types = Vec::new();

            let rt_predicted = tv.next();
            let (at, arg) = check_exp(*arg, st, tv)?;

            for (t, rt) in st.get_fun(&name) {
                let mut tv_ = tv.clone();

                match (tv_.unify(&at, t), tv_.unify(&rt_predicted, rt)) {
                    (Ok(at), Ok(rt)) => {
                        types.push((tv_, at, rt));
                    }
                    (Err(_e), _) | (_, Err(_e)) => (),
                }
            }

            match &*types {
                [] => return Err(TypeError::NoSuchFunction(fs, name)),
                [(tv_, at, rt)] => {
                    *tv = tv_.clone();
                    (rt.clone(), Expr::Call(name, at.clone(), rt.clone(), Box::new(arg)))
                }
                ts => {
                    let mut ret = Vec::new();
                    let mut at = Vec::new();
                    for (_tv, t, rt) in ts {
                        ret.push(rt.clone());
                        at.push(t.clone());
                    }

                    let rt = tv.next_with(ret);
                    let t = tv.next_with(at);

                    (rt.clone(), Expr::Call(name, t, rt, Box::new(arg)))
                }
            }
        }
        UntypedExpr::Member(fs, array, s) if s == "len" => {
            let (array_t, array) = check_exp(*array, st, tv)?;

            let generic_at = Type::Array(Box::new(tv.next()));
            let _array_t = tv.unify(&array_t, &generic_at).map_err(|_| TypeError::LengthOnNonArray(fs, array_t))?;

            (Type::Uint, Expr::Member(Type::Uint, Box::new(array), s))
        }
        UntypedExpr::Member(_, _, _) => todo!(),
        UntypedExpr::Index(_, array, index) => {
            let array_fs = array.file_span();
            let index_fs = index.file_span();

            let (array_t, array) = check_exp(*array, st, tv)?;
            let (index_t, index) = check_exp(*index, st, tv)?;

            let generic_at = Type::Array(Box::new(tv.next()));
            let array_t = tv.unify(&array_t, &generic_at).fs(array_fs)?;
            let index_t = tv.unify(&index_t, &Type::Uint).fs(index_fs)?;

            let element_t = match (array_t, index_t) {
                (Type::Array(et), Type::Uint) => et,
                _ => unreachable!(),
            };

            (*element_t, Expr::Index(Box::new(array), Box::new(index)))
        }
        UntypedExpr::If(fs, cond, if_true, if_false) => {
            let cond_fs = cond.file_span();
            let (bool_t, cond) = check_exp(*cond, st, tv)?;
            let (true_t, if_true) = check_exp(*if_true, st, tv)?;
            let (false_t, if_false) = check_exp(*if_false, st, tv)?;

            tv.unify(&bool_t, &Type::Bool).map_err(|_| TypeError::ConditionNotBoolean(cond_fs, bool_t))?;

            let t = tv.unify(&true_t, &false_t).fs(fs)?;

            (t, Expr::If(Box::new(cond), Box::new(if_true), Box::new(if_false)))
        }
        UntypedExpr::While(_, cond, body) => {
            let cond_fs = cond.file_span();
            let (bool_t, cond) = check_exp(*cond, st, tv)?;
            let (_body_t, body) = check_exp(*body, st, tv)?;

            tv.unify(&bool_t, &Type::Bool).map_err(|_| TypeError::ConditionNotBoolean(cond_fs, bool_t))?;

            (Type::Unit, Expr::While(Box::new(cond), Box::new(body)))
        }
    })
}