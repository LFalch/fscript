use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::cmp::PartialEq;

use crate::compile::{Statement, Expr, Literal::{self, String as LString, AmbigInt, Int, Uint, Float, Unit, None as LNone}};

#[derive(Clone)]
pub enum Function {
    Builtin(fn(Vec<Value>, &Enviroment) -> Value),
    Implemented(Vec<String>, Vec<Statement>),
}

impl PartialEq<Self> for Function {
    #[inline(always)]
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin(func) => write!(f, "built-in {:p}", func),
            Function::Implemented(args, _) => {
                write!(f, "fn(")?;
                let mut first = true;
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") {{...}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(Literal),
    Some(Box<Value>),
    Tuple(Vec<Value>),
    Array(Vec<Value>),
    Function(Function),
}

type SVEnv = HashMap<String, Value>;

pub struct Enviroment<'a> {
    parent_const_envs: Vec<&'a SVEnv>,
    const_env: SVEnv,
    var_env: SVEnv,
}

impl Enviroment<'_> {
    #[inline(always)]
    fn new_standard() -> Self {
        Enviroment {
            parent_const_envs: vec![],
            const_env: standard_env(),
            var_env: SVEnv::new(),
        }
    }
    fn get(&self, s: &str) -> Option<&Value> {
        let ret = self.var_env
            .get(s)
            .or_else(|| self.const_env.get(s));

        if let Some(ret) = ret {
            Some(ret)
        } else {
            for env in self.parent_const_envs.iter().rev() {
                if let Some(r) = env.get(s) {
                    return Some(r);
                }
            }
            None
        }
    }
    fn get_mut(&mut self, s: &str) -> Option<&mut Value> {
        self.var_env.get_mut(s)
    }
    fn add_const(&mut self, s: impl ToString, v: Value) {
        let s = s.to_string();
        self.var_env.remove(&s);
        self.const_env.insert(s, v);
    }
    fn add_var(&mut self, s: impl ToString, v: Value) {
        let s = s.to_string();
        self.const_env.remove(&s);
        self.var_env.insert(s, v);
    }
    fn scope<'a>(&'a self, vars: impl IntoIterator<Item=(String, Value)>) -> Enviroment<'a> {
        let mut parent_const_envs = Vec::with_capacity(self.parent_const_envs.len() + 1);
        parent_const_envs.extend(self.parent_const_envs.iter().copied());
        parent_const_envs.push(&self.const_env);
        Enviroment {
            parent_const_envs,
            const_env: SVEnv::new(),
            var_env: vars.into_iter().collect(),
        }
    }
}

mod fns;

fn standard_env() -> SVEnv {
    use crate::run::fns::*;

    let mut env = HashMap::new();

    #[inline]
    fn c(f: fn(Vec<Value>, &Enviroment) -> Value) -> Value {
        Value::Function(Function::Builtin(f))
    }

    env.insert("id".to_owned(), c(id));
    env.insert("add".to_owned(), c(add));
    env.insert("sub".to_owned(), c(sub));
    env.insert("mul".to_owned(), c(mul));
    env.insert("div".to_owned(), c(div));
    env.insert("rem".to_owned(), c(rem));
    env.insert("shl".to_owned(), c(shl));
    env.insert("shr".to_owned(), c(shr));
    env.insert("bitand".to_owned(), c(bitand));
    env.insert("xor".to_owned(), c(xor));
    env.insert("bitor".to_owned(), c(bitor));
    env.insert("eq".to_owned(), c(eq));
    env.insert("neq".to_owned(), c(neq));
    env.insert("gt".to_owned(), c(gt));
    env.insert("gte".to_owned(), c(gte));
    env.insert("lt".to_owned(), c(lt));
    env.insert("lte".to_owned(), c(lte));
    env.insert("neg".to_owned(), c(neg));
    env.insert("not".to_owned(), c(not));
    env.insert("concat".to_owned(), c(concat));
    env.insert("pow".to_owned(), c(pow));
    env.insert("print".to_owned(), c(print));
    env.insert("println".to_owned(), c(println));
    env.insert("show".to_owned(), c(show));

    env
} 

pub fn run(iter: impl IntoIterator<Item=Statement>) -> Value {
    let ref mut env = Enviroment::new_standard();

    run_statements(iter, env)
}

fn run_statements(iter: impl IntoIterator<Item=Statement>, env: &mut Enviroment<'_>) -> Value {
    for statement in iter {
        match statement {
            Statement::DiscardExpr(expr) => match eval(expr, env) {
                Value::Literal(Unit) => (),
                other => eprintln!("warning: unused value {:?}", other),
            }
            Statement::ConstAssign(ident, None, expr) => {
                let val = eval(expr, env);
                env.add_const(ident, val);
            }
            Statement::VarAssign(ident, None, expr) => {
                let val = eval(expr, env);
                env.add_var(ident, val);
            }
            Statement::Reassign(ident, expr) => {
                let val = eval(expr, env);
                match env.get_mut(&ident) {
                    Some(var) => *var = val,
                    None => panic!("no such variable in scope: {}", ident)
                }
            }
            Statement::ConstAssign(_, Some(_), _) | Statement::VarAssign(_, Some(_), _) => unimplemented!(),
            Statement::Return(expr) => return eval(expr, env),
        }
    }

    Value::Literal(Unit)
}

fn eval(expr: Expr, env: &Enviroment<'_>) -> Value {
    match expr {
        Expr::Identifer(ident) => if let Some(val) = env.get(&ident) {
            val.clone()
        } else {
            panic!("no such variable {}", ident);
        }
        Expr::Literal(lit) => Value::Literal(lit),
        Expr::Some(box expr) => Value::Some(Box::new(eval(expr, env))),
        Expr::Array(vec) => Value::Array(vec.into_iter().map(|expr| eval(expr, env)).collect()),
        Expr::Tuple(vec) => Value::Tuple(vec.into_iter().map(|expr| eval(expr, env)).collect()),
        Expr::Call(func, arg_exprs) => {
            let f = match env.get(&func) {
                Some(Value::Function(f)) => f.clone(),
                Some(_) => panic!("{} is not a function", func),
                None => panic!("no such function {}", func),
            };

            let exprs = arg_exprs.into_iter().map(|expr| eval(expr, env));

            match f {
                Function::Implemented(arg_names, body) => {
                    let ref mut env = env.scope(arg_names
                        .iter()
                        .map(|s| s.clone())
                        .zip(exprs));

                    run_statements(body.clone(), env)
                }
                Function::Builtin(f) => f(exprs.collect(), &*env),
            }
        }
        Expr::Ref(_) => todo!(),
        Expr::Deref(_) => todo!(),
        Expr::Member(box expr, ident) => {
            match (eval(expr, env), &*ident) {
                (Value::Array(v), "len") => Value::Literal(Uint(v.len() as u64)),
                (Value::Literal(LString(s)), "len") => Value::Literal(Uint(s.len() as u64)),
                (Value::Tuple(v), s) => if let Ok(n) = s.parse() {
                        if let Some(elem) = v.get::<usize>(n) {
                            elem.clone()
                        } else {
                            panic!("tuple index out of bounds")
                        }
                } else {
                    panic!("can only use numbers as members on a tuple")
                }
                _ => panic!("No such member {} on that type", ident),
            }
        }
        Expr::Index(box indexable, box index) => {
            let index = match eval(index, env) {
                Value::Literal(Uint(i) | AmbigInt(i)) => Ok(i as usize),
                Value::Tuple(mut tu) => {
                    if tu.len() == 2 {
                        let a = tu.swap_remove(0);
                        let b = tu.swap_remove(0);
                        match (a, b) {
                            (Value::Literal(Uint(i) | AmbigInt(i)), Value::Literal(Uint(j) | AmbigInt(j))) => {
                                Err((i as usize, j as usize))
                            }
                            _ => panic!("can only interpret two uints as a range")
                        }
                    } else {
                        panic!("can only interpret a tuple of two elements as a range")
                    }
                }
                _ => panic!("only a uint or tuple can be an index"),
            };
            match (eval(indexable, env), index) {
                (Value::Array(mut v), Ok(i)) => v.swap_remove(i),
                (Value::Array(mut v), Err((i, j))) => {
                    v.drain(j..);
                    v.drain(..i);
                    Value::Array(v)
                }
                (Value::Literal(LString(s)), Ok(i)) => Value::Literal(LString(s.chars().nth(i).expect("character at that index").to_string())),
                (Value::Literal(LString(s)), Err((i, j))) => Value::Literal(LString(
                    s.chars().skip(i).take(j-i).collect()
                )),
                _ => panic!("Invalid index"),
            }
        }
        // TODO should be able to access outer scope
        Expr::Block(statements) => run_statements(statements, &mut env.scope(None)),
        Expr::Function(arg_names, box body) => {
            Value::Function(Function::Implemented(arg_names, match body {
                Expr::Block(statements) => statements,
                expr => vec![Statement::Return(expr)],
            }))
        }
    }
}