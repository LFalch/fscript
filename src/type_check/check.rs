use std::{collections::{BTreeMap, HashMap}, fmt::{self, Display}};

use super::ast::*;
use crate::source::{ast::{Type as TypeHint, Expr as UntypedExpr, Primitive, Statement as UntypedStatement, Statements as UntypedStatements}, FileSpan};

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

pub struct TypeCollection {
    next: TypeVariableName,
    map: BTreeMap<TypeVariableName, Type>,
}

impl TypeCollection {
    pub fn new() -> Self { TypeCollection { next: 0, map: BTreeMap::new() } }
    pub fn next(&mut self) -> Type {
        let t = Type::TypeVariable(self.next);
        self.map.insert(self.next, t.clone());

        self.next += 1;
        t
    }
    pub fn next_integral(&mut self) -> Type {
        let t = Type::IntegralVariable(self.next);
        self.map.insert(self.next, t.clone());

        self.next += 1;
        t
    }
    pub fn convert(&mut self, th: TypeHint) -> Type {
        match th {
            TypeHint::Inferred => self.next(),
            TypeHint::Named(t) => t.into(&mut |_| {
                let n = self.next;
                self.next += 1;
                n
            }),
        }
    }
    pub fn unify(&mut self, lhs: Type, rhs: Type) -> Option<Type> {
        use self::NamedType::*;
        match (lhs, rhs) {
            (a, b) if a == b => Some(a),
            (IntegralVariable(n), IntegralVariable(n2)) | (IntegralVariable(n2), TypeVariable(n)) => {
                let t = self.map[&n].clone();

                *self.map.get_mut(&n2).unwrap() = t.clone();

                Some(t)
            }
            (TypeVariable(n), TypeVariable(n2)) => {
                let t = self.map[&n].clone();

                *self.map.get_mut(&n2).unwrap() = t.clone();

                Some(t)
            }
            (TypeVariable(n), t) | (t, Type::TypeVariable(n)) => {
                *self.map.get_mut(&n).unwrap() = t.clone();

                Some(t)
            },
            (IntegralVariable(n), Int) | (Int, IntegralVariable(n)) => {
                *self.map.get_mut(&n).unwrap() = Int;

                Some(Int)
            }
            (IntegralVariable(n), Uint) | (Uint, IntegralVariable(n)) => {
                *self.map.get_mut(&n).unwrap() = Uint;

                Some(Uint)
            }
            (Array(t), Array(t2)) => self.unify(*t, *t2).map(|t| Array(Box::new(t))),
            (Tuple(ts), Tuple(ts2)) => {
                if ts.len() != ts2.len() {
                    return None;
                }
                let mut v = Vec::with_capacity(ts.len());

                for (a, b) in ts.into_iter().zip(ts2) {
                    v.push(self.unify(a, b)?);
                }

                Some(Tuple(v))
            }
            (Option(t), Option(t2)) => self.unify(*t, *t2).map(|t| Option(Box::new(t))),
            (Reference(t), Reference(t2)) => self.unify(*t, *t2).map(|t| Reference(Box::new(t))),
            (MutReference(t), MutReference(t2)) => self.unify(*t, *t2).map(|t| MutReference(Box::new(t))),
            (Function(ts, t), Function(ts2, t2)) => {
                let t = self.unify(*t, *t2)?;

                if ts.len() != ts2.len() {
                    return None;
                }
                let mut v = Vec::with_capacity(ts.len());

                for (a, b) in ts.into_iter().zip(ts2) {
                    v.push(self.unify(a, b)?);
                }

                Some(Function(v, Box::new(t)))
            }
            (_, _) => None,
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
        Function(_, _, _, e) => returns_in_expr(e),
    }
}

fn returns_in_expr(expr: &Expr) -> Vec<ReturnType> {
    use self::Expr::*;
    match expr {
        Block(rt, _stmnts) => vec![rt.clone()],
        Some(e) => returns_in_expr(e),
        Array(exprs) => exprs.iter().flat_map(returns_in_expr).collect(),
        Tuple(exprs) => exprs.iter().flat_map(returns_in_expr).collect(),
        Call(_, _, exprs) => exprs.iter().flat_map(returns_in_expr).collect(),
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

pub type SymbolTable = HashMap<String, (bool, Type)>;

/// First part of the type is return type
pub fn check_statements(stmnts: UntypedStatements, st: &mut SymbolTable, tv: &mut TypeCollection) -> Result<(ReturnType, Statements), TypeError> {
    let mut typed_stmnts = Vec::with_capacity(stmnts.len());
    for stmnt in stmnts {
        match stmnt {
            UntypedStatement::VarAssign(_, ident, t, e) => {
                let t = tv.convert(t);
                let fs = e.file_span();
                let (te, e) = check_exp(e, st, tv)?;
                let error = TypeError::CouldNotUnifyTypes(fs, t.clone(), te.clone());
                let t = tv.unify(t, te).ok_or(error)?;

                st.insert(ident.clone(), (true, t.clone()));

                typed_stmnts.push(Statement::VarAssign(ident, t, e));
            }
            UntypedStatement::ConstAssign(_, ident, t, e) => {
                let t = tv.convert(t);
                let fs = e.file_span();
                let (te, e) = check_exp(e, st, tv)?;
                let error = TypeError::CouldNotUnifyTypes(fs, t.clone(), te.clone());
                let t = tv.unify(t, te).ok_or(error)?;

                st.insert(ident.clone(), (false, t.clone()));

                typed_stmnts.push(Statement::ConstAssign(ident, t, e));
            }
            UntypedStatement::Reassign(fs, ident, e) => {
                let (te, e) = check_exp(e, st, tv)?;

                let t = match st.get(&ident) {
                    None => return Err(TypeError::NoSuchVariable(fs, ident)),
                    Some((false, _)) => return Err(TypeError::NotMutable(fs, ident)),
                    Some((true, t)) => t.clone(),
                };

                let error = TypeError::CouldNotUnifyTypes(fs, t.clone(), te.clone());
                let _t = tv.unify(t, te).ok_or(error)?;

                typed_stmnts.push(Statement::Reassign(ident, e));
            }
            UntypedStatement::Function(fs, name, args, body) => {
                let mut body_st = st.clone();

                let mut arg_types = Vec::with_capacity(args.len());
                let mut typed_args = Vec::with_capacity(args.len());

                for (arg_name, arg_type) in args {
                    let arg_type = tv.convert(arg_type);
                    arg_types.push(arg_type.clone());
                    typed_args.push((arg_name.clone(), arg_type.clone()));
                    body_st.insert(arg_name, (true, arg_type));
                }

                let (mut body_t, body) = check_exp(body, &mut body_st, tv)?;
                drop(body_st);

                if let Type::Unit = body_t {
                    body_t = tv.next();
                }
                
                for rt in returns_in_expr(&body) {
                    let error = TypeError::CouldNotUnifyTypes(fs, body_t.clone(), rt.clone());
                    body_t = tv.unify(body_t, rt).ok_or(error)?;
                }

                st.insert(name.clone(), (false, Type::Function(arg_types, Box::new(body_t.clone()))));

                typed_stmnts.push(Statement::Function(name, typed_args, body_t, body));
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
        let err = TypeError::CouldNotUnifyTypes(FileSpan::dud(), rt.clone(), rt_cand.clone());

        rt = tv.unify(rt, rt_cand).ok_or(err)?;
    }

    Ok((rt, typed_stmnts))
}

pub fn check_exp(expr: UntypedExpr, st: &mut SymbolTable, tv: &mut TypeCollection) -> Result<(Type, Expr), TypeError> {
    Ok(match expr {
        UntypedExpr::Constant(_, Primitive::Bool(b)) => (Type::Bool, Expr::Bool(b)),
        UntypedExpr::Constant(_, Primitive::Float(f)) => (Type::Float, Expr::Float(f)),
        UntypedExpr::Constant(_, Primitive::Int(n)) => (Type::Int, Expr::Int(n)),
        UntypedExpr::Constant(_, Primitive::Uint(n)) => (Type::Int, Expr::Uint(n)),
        UntypedExpr::Constant(_, Primitive::AmbigInt(n)) => (tv.next_integral(), Expr::Uint(n)),
        UntypedExpr::Constant(_, Primitive::None) => (Type::Option(Box::new(tv.next())), Expr::None),
        UntypedExpr::Constant(_, Primitive::String(s)) => (Type::String, Expr::String(s)),
        UntypedExpr::Constant(_, Primitive::Unit) => (Type::Unit, Expr::Unit),
        UntypedExpr::Identifer(_, ident) => (tv.next(), Expr::Identifer(ident)),
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
            let (t, expr) = check_exp(*expr, st, tv)?;
            let t = match t {
                Type::Reference(t) | Type::MutReference(t) => *t,
                _ => return Err(TypeError::CannotBeDereferenced(fs, t)),
            };
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

                let error = TypeError::CouldNotUnifyTypes(fs, t.clone(), t2.clone());

                t = tv.unify(t, t2).ok_or(error)?;
            }

            (t, Expr::Array(typed_arr))
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
        UntypedExpr::Call(fs, name, args) => {
            let (_, ft) = st.get(&name).ok_or(TypeError::NoSuchFunction(fs, name.clone()))?;
            let ft = ft.clone();

           let (arg_types, rt) = match ft {
                Type::Function(arg_types, rt) => (arg_types, (*rt).clone()),
                ft => return Err(TypeError::NotAFunction(fs, name.clone(), ft)),
            };

            let mut typed_args = Vec::with_capacity(args.len());

            for (arg, at) in args.into_iter().zip(arg_types) {
                let (t, arg) = check_exp(arg, st, tv)?;

                let error = TypeError::CouldNotUnifyTypes(fs, at.clone(), t.clone());
                let _t = tv.unify(at, t).ok_or(error)?;

                typed_args.push(arg);
            }

            (rt.clone(), Expr::Call(rt, name, typed_args))
        }
        UntypedExpr::Member(fs, array, s) if s == "len" => {
            let (array_t, array) = check_exp(*array, st, tv)?;

            let generic_at = Type::Array(Box::new(tv.next()));
            let array_t = tv.unify(array_t.clone(), generic_at).ok_or(TypeError::LengthOnNonArray(fs, array_t))?;

            let element_t = match array_t {
                Type::Array(et) => *et,
                array_t => return Err(TypeError::LengthOnNonArray(fs, array_t)),
            };

            (Type::Uint, Expr::Member(element_t, Box::new(array), s))
        }
        UntypedExpr::Member(_, _, _) => todo!(),
        UntypedExpr::Index(_, array, index) => {
            let array_fs = array.file_span();
            let index_fs = index.file_span();

            let (array_t, array) = check_exp(*array, st, tv)?;
            let (index_t, index) = check_exp(*index, st, tv)?;

            let generic_at = Type::Array(Box::new(tv.next()));
            let array_t = tv.unify(array_t.clone(), generic_at.clone()).ok_or(TypeError::CouldNotUnifyTypes(array_fs, array_t, generic_at))?;
            let index_t = tv.unify(index_t.clone(), Type::Uint).ok_or(TypeError::CouldNotUnifyTypes(index_fs, index_t, Type::Uint))?;

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

            tv.unify(bool_t.clone(), Type::Bool).ok_or(TypeError::ConditionNotBoolean(cond_fs, bool_t))?;

            let error = TypeError::CouldNotUnifyTypes(fs, true_t.clone(), false_t.clone());
            let t = tv.unify(true_t, false_t).ok_or(error)?;

            (t, Expr::If(Box::new(cond), Box::new(if_true), Box::new(if_false)))
        }
        UntypedExpr::While(_, cond, body) => {
            let cond_fs = cond.file_span();
            let (bool_t, cond) = check_exp(*cond, st, tv)?;
            let (_body_t, body) = check_exp(*body, st, tv)?;

            tv.unify(bool_t.clone(), Type::Bool).ok_or(TypeError::ConditionNotBoolean(cond_fs, bool_t))?;

            (Type::Unit, Expr::While(Box::new(cond), Box::new(body)))
        }
    })
}