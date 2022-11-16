use std::collections::{BTreeMap, HashMap, HashSet};

use super::{ast::*, check::ReturnType};
use super::check::TypeError;
use crate::source::{ast::Type as TypeHint, FileSpan};

#[derive(Debug, Clone)]
pub struct TypeCollection {
    next: TypeVariableName,
    map: BTreeMap<TypeVariableName, Type>,
}

impl TypeCollection {
    pub fn new() -> Self { TypeCollection { next: 1, map: BTreeMap::new() } }
    fn next_id(&mut self) -> u64 {
        let i = self.next;
        self.next += 1;
        i
    }
    pub fn next(&mut self) -> Type {
        let next_id = self.next_id();
        let t = Type::type_variable(next_id);
        self.map.insert(next_id, t.clone());

        t
    }
    pub fn next_integral(&mut self) -> Type {
        let next_id = self.next_id();
        let t = Type::int_variable(next_id);
        self.map.insert(next_id, t.clone());

        t
    }
    pub fn next_with(&mut self, candidates: Vec<Type>) -> Type {
        let next_id = self.next_id();
        let t = Type::TypeVariable(next_id, candidates);
        self.map.insert(next_id, t.clone());

        t
    }
    /// -> (ref type, inner type)
    pub fn next_ref(&mut self) -> (Type, Type) {
        let inner_t = Box::new(self.next());
        let next_id = self.next_id();
        let t = Type::TypeVariable(next_id, vec![Type::Reference(inner_t.clone()), Type::MutReference(inner_t.clone())]);
        self.map.insert(next_id, t.clone());

        (t, *inner_t)
    }
    pub fn convert(&mut self, th: TypeHint) -> Type {
        match th {
            TypeHint::Inferred => self.next(),
            TypeHint::Tuple(ts) => Type::Tuple(ts.into_iter().map(|t| self.convert(t)).collect()),
            TypeHint::Named(t) => t.into(&mut |_| {
                let n = self.next_id();
                self.map.insert(n, Type::type_variable(n));
                n
            }),
        }
    }
    // TODO: work on making this less aggressive at times
    pub fn unify(&mut self, lhs: &Type, rhs: &Type) -> Result<Type, TypeError> {
        use self::NamedType::*;

        match (self.lookup_by_type(lhs), self.lookup_by_type(rhs)) {
            (a, b) if a == b => Ok(a),
            (TypeVariable(n, ts), TypeVariable(n2, ts2)) => {
                let intersection = if ts.is_empty() {
                    ts2
                } else if ts2.is_empty() {
                    ts
                } else {
                    let mut v = Vec::with_capacity(ts.len().min(ts2.len()));

                    for t in ts {
                        if ts2.contains(&t) {
                            v.push(t);
                        }
                    }

                    v
                };

                let t = TypeVariable(n, intersection);

                *self.map.get_mut(&n).unwrap() = t.clone();
                *self.map.get_mut(&n2).unwrap() = t.clone();

                Ok(t)
            }
            (TypeVariable(n, ts), t) | (t, TypeVariable(n, ts)) => {
                let ut;
                if ts.is_empty() {
                    ut = t;
                } else {
                    let mut unified_t = None;
    
                    for gt in ts.clone() {
                        match self.unify(&gt, &t) {
                            Ok(t) => {
                                unified_t = Some(t);
                                break
                            }
                            Err(_) => (),
                        }
                    }
    
                    ut = unified_t.ok_or(TypeError::CouldNotUnifyTypes(FileSpan::dud(), TypeVariable(n, ts), t))?;
                }

                *self.map.get_mut(&n).unwrap() = ut.clone();

                Ok(ut)
            },
            (Array(t), Array(t2)) => self.unify(&t, &t2).map(|t| Array(Box::new(t))),
            (Tuple(ts), Tuple(ts2)) if ts.len() == ts2.len() => {
                let mut v = Vec::with_capacity(ts.len());

                for (a, b) in ts.into_iter().zip(ts2) {
                    v.push(self.unify(&a, &b)?);
                }

                Ok(Tuple(v))
            }
            (Option(t), Option(t2)) => self.unify(&t, &t2).map(|t| Option(Box::new(t))),
            (Reference(t), Reference(t2)) => self.unify(&t, &t2).map(|t| Reference(Box::new(t))),
            (MutReference(t), MutReference(t2)) => self.unify(&t, &t2).map(|t| MutReference(Box::new(t))),
            (Function(ret, arg), Function(ret2, arg2)) => {
                let arg = self.unify(&arg, &arg2)?;
                let ret = self.unify(&ret, &ret2)?;

                Ok(Function(Box::new(ret), Box::new(arg)))
            }
            (a, b) => Err(TypeError::CouldNotUnifyTypes(FileSpan::dud(), a, b)),
        }
    }
    /// Look for the most specific type equivalent to the given type
    pub fn lookup_by_type(&self, t: &Type) -> Type {
        use self::NamedType::*;
        match t {
            // TODO: maybe handle the second arg here
            TypeVariable(n, _) => self.lookup_type(*n),
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
    fn lookup_type(&self, name: TypeVariableName) -> Type {
        match &self.map[&name] {
            NamedType::TypeVariable(n, _) if *n != name => self.lookup_type(*n),
            t => t.clone(),
        }
    }
    pub fn show(&self) {
        for (i, t) in &self.map {
            eprintln!("'{i} :-> {t} :=: {}", self.lookup_type(*i));
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    // Non-function variables and mutable function pointers
    variables: HashMap<String, (bool, Type)>,
    // non-mutable functions
    fn_table: HashMap<String, HashSet<(Type, ReturnType)>>
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            variables: HashMap::new(),
            fn_table: HashMap::new(),
        }
    }
    pub fn bind(&mut self, b: &Binding, mutable: bool, t: Type) -> Result<(), TypeError> {
        match (b, t) {
            (Binding::Unit, Type::Unit) => Ok(()),
            (Binding::Unit, t) => panic!("{t} was not a unit"),
            (Binding::Name(s), t) => {
                self.add(s, mutable, t);
                Ok(())
            }
            (Binding::Tuple(bs), Type::Tuple(ts)) => {
                if bs.len() != ts.len() {
                    panic!("non-matching tuple sizes to destructure")
                }
                for (b, t) in bs.into_iter().zip(ts) {
                    self.bind(b, mutable, t)?;
                }
                Ok(())
            }
            (Binding::Tuple(_), v) => panic!("{v:?} was not a tuple"),
        }
    }
    pub fn add<S: ToString>(&mut self, s: S, mutable: bool, t: Type) {
        match (mutable, t) {
            (false, Type::Function(t, rt)) => {
                self.add_fn(s, *t, *rt);
            }
            (_, t) => {
                self.variables.insert(s.to_string(), (mutable, t));
            }
        }
    }
    pub fn add_fn<S: ToString>(&mut self, s: S, arg: Type, rt: Type) {
        let s = s.to_string();
        self.fn_table.entry(s.to_string()).or_insert_with(HashSet::new).insert((arg, rt));
    }
    pub fn get(&self, s: &str) -> impl '_ + Iterator<Item=(bool, Type)> {
        self.variables.get(s).into_iter()
            .map(|x| x.clone())
            .chain(self.fn_table.get(s).map(|t| t.iter().map(|(t, rt)| (false, Type::Function(Box::new(t.clone()), Box::new(rt.clone()))))).into_iter().flatten())
    }
    pub fn get_var(&self, s: &str) -> Option<&(bool, Type)> {
        self.variables.get(s)
    }
    pub fn get_fun(&self, s: &str) -> impl '_ + Iterator<Item=&(Type, ReturnType)> {
        self.fn_table.get(s).map(|set| set.into_iter()).into_iter().flatten()
    }
}
