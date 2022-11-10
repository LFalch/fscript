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

#[derive(Debug)]
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
                self.map.insert(self.next, Type::TypeVariable(n));
                self.next += 1;
                n
            }),
        }
    }
    pub fn unify(&mut self, lhs: Type, rhs: Type) -> Result<Type, TypeError> {
        use self::NamedType::*;

        match (self.lookup_by_type(&lhs), self.lookup_by_type(&rhs)) {
            (a, b) if a == b => Ok(a),
            (IntegralVariable(n2) | TypeVariable(n2), IntegralVariable(n)) | (IntegralVariable(n), TypeVariable(n2)) => {
                let t = self.map[&n].clone();

                *self.map.get_mut(&n2).unwrap() = t.clone();

                Ok(t)
            }
            (TypeVariable(n), TypeVariable(n2)) => {
                let t = self.map[&n].clone();

                *self.map.get_mut(&n2).unwrap() = t.clone();

                Ok(t)
            }
            (TypeVariable(n), t) | (t, TypeVariable(n)) => {
                *self.map.get_mut(&n).unwrap() = t.clone();

                Ok(t)
            },
            (IntegralVariable(n), Int) | (Int, IntegralVariable(n)) => {
                *self.map.get_mut(&n).unwrap() = Int;

                Ok(Int)
            }
            (IntegralVariable(n), Uint) | (Uint, IntegralVariable(n)) => {
                *self.map.get_mut(&n).unwrap() = Uint;

                Ok(Uint)
            }
            (Array(t), Array(t2)) => self.unify(*t, *t2).map(|t| Array(Box::new(t))),
            (Tuple(ts), Tuple(ts2)) if ts.len() == ts2.len() => {
                let mut v = Vec::with_capacity(ts.len());

                for (a, b) in ts.into_iter().zip(ts2) {
                    v.push(self.unify(a, b)?);
                }

                Ok(Tuple(v))
            }
            (Option(t), Option(t2)) => self.unify(*t, *t2).map(|t| Option(Box::new(t))),
            (Reference(t), Reference(t2)) => self.unify(*t, *t2).map(|t| Reference(Box::new(t))),
            (MutReference(t), MutReference(t2)) => self.unify(*t, *t2).map(|t| MutReference(Box::new(t))),
            (Function(ret, arg), Function(ret2, arg2)) => {
                let arg = self.unify(*arg, *arg2)?;
                let ret = self.unify(*ret, *ret2)?;

                Ok(Function(Box::new(ret), Box::new(arg)))
            }
            (a, b) => Err(TypeError::CouldNotUnifyTypes(FileSpan::dud(), a, b)),
        }
    }
    /// Look for the most specific type equivalent to the given type
    fn lookup_by_type(&self, t: &Type) -> Type {
        use self::NamedType::*;
        match t {
            TypeVariable(n) | IntegralVariable(n) => self.lookup_type(*n),
            Array(t) => Array(Box::new(self.lookup_by_type(t))),
            Option(t) => Option(Box::new(self.lookup_by_type(t))),
            Reference(t) => Reference(Box::new(self.lookup_by_type(t))),
            MutReference(t) => MutReference(Box::new(self.lookup_by_type(t))),
            Tuple(ts) => Tuple(ts.iter().map(|t| self.lookup_by_type(t)).collect()),
            Function(ret, arg) => Function(Box::new(self.lookup_by_type(ret)), Box::new(self.lookup_by_type(arg))),
            Unit => Unit,
            Bool => Bool,
            Uint => Uint,
            Int => Int,
            Float => Float,
            String => String,
        }
    }
    /// Look up given type variable name in the inner map for its potential more specific type (either more concrete or just the same as another type variable)
    pub fn lookup_type(&self, name: TypeVariableName) -> Type {
        match &self.map[&name] {
            NamedType::TypeVariable(n) | NamedType::IntegralVariable(n) if *n != name => self.lookup_type(*n),
            t => t.clone(),
        }
    }
    pub fn show(&self) {
        for (i, t) in &self.map {
            eprintln!("'{i} :-> {t} :=: {}", self.lookup_type(*i));
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
        Call(_, _, e) => returns_in_expr(e),
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
                let t = tv.unify(t, te).fs(fs)?;

                st.insert(ident.clone(), (true, t.clone()));

                typed_stmnts.push(Statement::VarAssign(ident, t, e));
            }
            UntypedStatement::ConstAssign(_, ident, t, e) => {
                let t = tv.convert(t);
                let fs = e.file_span();
                let (te, e) = check_exp(e, st, tv)?;
                let t = tv.unify(t, te).fs(fs)?;

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

                let _t = tv.unify(t, te).fs(fs)?;

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
                    body_t = tv.unify(body_t, rt).fs(fs)?;
                }

                let arg_type = match arg_types.len() {
                    0 => Type::Unit,
                    1 => arg_types.pop().unwrap(),
                    _ => Type::Tuple(arg_types),
                };

                st.insert(name.clone(), (false, Type::Function(Box::new(arg_type), Box::new(body_t.clone()))));

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
        rt = tv.unify(rt, rt_cand)?;
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
        UntypedExpr::Identifer(_, ident) => {
            let t = st.get(&ident).map(|(_, t)| t.clone()).unwrap_or_else(|| tv.next());

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

            let t = tv.next();
            let ref_t_immut = Type::Reference(Box::new(t.clone()));
            let ref_t_mut = Type::MutReference(Box::new(t));

            let unified_ref_t = tv.unify(ref_t.clone(), ref_t_immut).or_else(|_| tv.unify(ref_t.clone(), ref_t_mut));

            let t = match unified_ref_t {
                Ok(Type::Reference(t) | Type::MutReference(t)) => *t,
                _ => return Err(TypeError::CannotBeDereferenced(fs, ref_t)),
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

                t = tv.unify(t, t2).fs(fs)?;
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
            let ft = st.get(&name).ok_or(TypeError::NoSuchFunction(fs, name.clone()))?.1.clone();

            let (arg_type, rt) = match ft {
                Type::Function(arg_type, rt) => (*arg_type, *rt),
                ft => return Err(TypeError::NotAFunction(fs, name.clone(), ft)),
            };

            let (at, arg) = check_exp(*arg, st, tv)?;
            let _at = tv.unify(at, arg_type).fs(fs)?;

            (rt.clone(), Expr::Call(rt, name, Box::new(arg)))
        }
        UntypedExpr::Member(fs, array, s) if s == "len" => {
            let (array_t, array) = check_exp(*array, st, tv)?;

            let generic_at = Type::Array(Box::new(tv.next()));
            let _array_t = tv.unify(array_t.clone(), generic_at).map_err(|_| TypeError::LengthOnNonArray(fs, array_t))?;

            (Type::Uint, Expr::Member(Type::Uint, Box::new(array), s))
        }
        UntypedExpr::Member(_, _, _) => todo!(),
        UntypedExpr::Index(_, array, index) => {
            let array_fs = array.file_span();
            let index_fs = index.file_span();

            let (array_t, array) = check_exp(*array, st, tv)?;
            let (index_t, index) = check_exp(*index, st, tv)?;

            let generic_at = Type::Array(Box::new(tv.next()));
            let array_t = tv.unify(array_t.clone(), generic_at.clone()).fs(array_fs)?;
            let index_t = tv.unify(index_t.clone(), Type::Uint).fs(index_fs)?;

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

            tv.unify(bool_t.clone(), Type::Bool).map_err(|_| TypeError::ConditionNotBoolean(cond_fs, bool_t))?;

            let t = tv.unify(true_t, false_t).fs(fs)?;

            (t, Expr::If(Box::new(cond), Box::new(if_true), Box::new(if_false)))
        }
        UntypedExpr::While(_, cond, body) => {
            let cond_fs = cond.file_span();
            let (bool_t, cond) = check_exp(*cond, st, tv)?;
            let (_body_t, body) = check_exp(*body, st, tv)?;

            tv.unify(bool_t.clone(), Type::Bool).map_err(|_| TypeError::ConditionNotBoolean(cond_fs, bool_t))?;

            (Type::Unit, Expr::While(Box::new(cond), Box::new(body)))
        }
    })
}