//! An interpreter that runs the source abstract syntax tree directly
#![warn(missing_docs)]

use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::cmp::PartialEq;

use collect_result::CollectResult;

use crate::source::ast::{
    Statement,
    Expr,
    Primitive::{self, String as LString, AmbigInt, Uint, Unit, None as LNone}
};
use crate::source::tokeniser::FileLocation;

#[derive(Clone)]
/// A function value
pub enum Function {
    /// A function whose body is not fscript.
    /// Used for built-in functions.
    Builtin(fn(Vec<Value>, &Enviroment) -> Value),
    /// A function that is defined in fscript
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
/// A value of some type
pub enum Value {
    /// A basic type that would be defined with a literal constant
    Primitive(Primitive),
    /// An optional value that is `Some` and not `None`
    Some(Box<Value>),
    /// A tuple of values
    Tuple(Vec<Value>),
    /// An array of values
    Array(Vec<Value>),
    /// A function
    Function(Function),
    /// An immutable reference with some address
    Ref(usize),
    /// A mutable refrence wiht som address
    MutRef(usize),
}

type SVEnv = HashMap<String, usize>;

/// The environment of variables declared in some scope
pub struct Enviroment<'a> {
    stack: &'a mut Vec<Value>,
    parent_length: usize,
    parent_const_envs: Box<[&'a SVEnv]>,
    const_env: SVEnv,
    var_env: SVEnv,
}

impl Drop for Enviroment<'_> {
    #[inline]
    fn drop(&mut self) {
        self.stack.drain(self.parent_length..);
    }
}

mod fns;

impl<'s> Enviroment<'s> {
    #[inline(always)]
    /// Stack will be cleared
    fn new_standard(stack: &'s mut Vec<Value>) -> Self {
        use crate::interpreter::fns::*;
        stack.clear();

        let mut env = Enviroment {
            stack: stack,
            parent_length: 0,
            parent_const_envs: Box::new([]),
            const_env: SVEnv::new(),
            var_env: SVEnv::new(),
        };
        #[inline]
        fn c(f: fn(Vec<Value>, &Enviroment) -> Value) -> Value {
            Value::Function(Function::Builtin(f))
        }

        env.add_const("id", c(id));
        env.add_const("add", c(add));
        env.add_const("sub", c(sub));
        env.add_const("mul", c(mul));
        env.add_const("div", c(div));
        env.add_const("rem", c(rem));
        env.add_const("shl", c(shl));
        env.add_const("shr", c(shr));
        env.add_const("bitand", c(bitand));
        env.add_const("xor", c(xor));
        env.add_const("bitor", c(bitor));
        env.add_const("eq", c(eq));
        env.add_const("neq", c(neq));
        env.add_const("gt", c(gt));
        env.add_const("gte", c(gte));
        env.add_const("lt", c(lt));
        env.add_const("lte", c(lte));
        env.add_const("neg", c(neg));
        env.add_const("not", c(not));
        env.add_const("concat", c(concat));
        env.add_const("pow", c(pow));
        env.add_const("print", c(print));
        env.add_const("println", c(println));
        env.add_const("show", c(show));

        env
    }
    fn index(&self, i: usize) -> &Value {
        &self.stack[i]
    }
    fn index_mut(&mut self, i: usize) -> &mut Value {
        &mut self.stack[i]
    }
    fn get_index(&self, s: &str) -> Option<usize> {
        let ret = self.var_env
            .get(s)
            .or_else(|| self.const_env.get(s));

        if let Some(ret) = ret {
            Some(*ret)
        } else {
            let mut ret = None;
            for env in self.parent_const_envs.iter().rev() {
                if let Some(r) = env.get(s) {
                    ret = Some(*r);
                    break;
                }
            }
            ret
        }
    }
    #[inline]
    fn get(&self, s: &str) -> Option<&Value> {
        if let Some(index) = self.get_index(s) {
            Some(&self.stack[index])
        } else {
            None
        }
    }
    #[inline]
    fn get_mut_index(&self, s: &str) -> Option<usize> {
        self.var_env.get(s).copied()
    }
    fn get_mut(&mut self, s: &str) -> Option<&mut Value> {
        if let Some(i) = self.get_mut_index(s) {
            Some(self.index_mut(i))
        } else {
            None
        }
    }
    fn add(&mut self, v: Value) -> usize {
        let i = self.stack.len();
        match &v {
            Value::Ref(r) | Value::MutRef(r) if r >= &i => panic!("Dangling reference"),
            _ => ()
        }
        self.stack.push(v);
        i
    }
    fn add_const(&mut self, s: impl ToString, v: Value) {
        let s = s.to_string();
        self.var_env.remove(&s);
        let i = self.add(v);
        self.const_env.insert(s, i);
    }
    fn add_var(&mut self, s: impl ToString, v: Value) {
        let s = s.to_string();
        self.const_env.remove(&s);
        let i = self.add(v);
        self.var_env.insert(s, i);
    }
    fn scope<'a>(&'a mut self, vars: impl IntoIterator<Item=(String, Value)>) -> Enviroment<'a> {
        let parent_const_envs = self.parent_const_envs.iter().copied().chain(Some(&self.const_env)).collect();

        Enviroment {
            parent_length: self.stack.len(),
            var_env: {
                let vars = vars.into_iter(); 
                let mut var_env = SVEnv::with_capacity(vars.size_hint().0);
                for (var, val) in vars {
                    var_env.insert(var, self.stack.len());
                    self.stack.push(val);
                }
                var_env
            },
            stack: self.stack,
            parent_const_envs,
            const_env: SVEnv::new(),
        }
    }
}

#[derive(Debug, Clone)]
/// Represents an error that occured whislt interpreting the program
pub struct RuntimeError {
    file_loc: FileLocation,
    error: String,
}

impl RuntimeError {
    fn new<S: ToString>(fl: FileLocation, error: S) -> Self {
        RuntimeError { file_loc: fl, error: error.to_string() }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{} {}", self.file_loc.line, self.file_loc.column, self.error)
    }
}

macro_rules! rte {
    ($fl:expr, $s:expr) => {
        return Err(RuntimeError::new($fl, format!($s)))
    };
    ($fl:expr, $s:expr, $($a:expr),+) => {
        return Err(RuntimeError::new($fl, format!($s, $($a),+)))
    };
}

/// Runs an iterator of statements and returns the value returned by those statements
pub fn run(iter: impl IntoIterator<Item=Statement>) -> Result<Value, RuntimeError> {
    let mut stack = Vec::new();
    let mut env = Enviroment::new_standard(&mut stack);

    #[cfg(feature = "debug_print_statements")]
    let iter = iter.into_iter().inspect(|statement| println!("{:?}", statement));

    run_statements(iter, &mut env)
}

fn run_statements(iter: impl IntoIterator<Item=Statement>, env: &mut Enviroment<'_>) -> Result<Value, RuntimeError> {
    for statement in iter {
        match statement {
            Statement::DiscardExpr(expr) => match eval(expr, env)? {
                Value::Primitive(Unit) => (),
                other => eprintln!("warning: unused value {:?}", other),
            }
            Statement::ConstAssign(ident, None, expr) => {
                let val = eval(expr, env)?;
                env.add_const(ident, val);
            }
            Statement::VarAssign(ident, None, expr) => {
                let val = eval(expr, env)?;
                env.add_var(ident, val);
            }
            Statement::Reassign(ident, expr) => {
                let val = eval(expr, env)?;
                match env.get_mut(&ident) {
                    Some(var) => *var = val,
                    None => panic!("no such variable in scope: {}", ident)
                }
            }
            Statement::ConstAssign(_, Some(_), _) | Statement::VarAssign(_, Some(_), _) => unimplemented!(),
            Statement::Return(expr) => return eval(expr, env),
        }
    }

    Ok(Value::Primitive(Unit))
}

fn eval(expr: Expr, env: &mut Enviroment<'_>) -> Result<Value, RuntimeError> {
    Ok(match expr {
        Expr::Identifer(fl, ident) => if let Some(val) = env.get(&ident) {
            val.clone()
        } else {
            return Err(RuntimeError::new(fl, format!("no such variable {}", ident)))
        }
        Expr::Constant(_fl, lit) => Value::Primitive(lit),
        Expr::Some(_fl, expr) => {
            let expr = *expr;
            Value::Some(Box::new(eval(expr, env)?))
        }
        Expr::Array(_fl, vec) => Value::Array(vec.into_iter().map(|expr| eval(expr, env)).collect_result()?),
        Expr::Tuple(_fl, vec) => Value::Tuple(vec.into_iter().map(|expr| eval(expr, env)).collect_result()?),
        Expr::Call(fl, func, arg_exprs) => {
            let f = match env.get(&func) {
                Some(Value::Function(f)) => f.clone(),
                Some(_) => rte!(fl, "{} is not a function", func),
                None => rte!(fl, "no such function {}", func),
            };

            let exprs: Vec<_> = arg_exprs.into_iter().map(|expr| eval(expr, env)).collect_result()?;

            match f {
                Function::Implemented(arg_names, body) => {
                    let ref mut env = env.scope(arg_names
                        .iter()
                        .map(|s| s.clone())
                        .zip(exprs));

                    run_statements(body.clone(), env)?
                }
                Function::Builtin(f) => f(exprs, &*env),
            }
        }
        Expr::Ref(_fl, expr) => match *expr {
            Expr::Identifer(fl, s) => Value::Ref(env.get_index(&s).ok_or_else(|| RuntimeError::new(fl, "no value bound to name"))?),
            expr => {
                let val = eval(expr, env)?;
                Value::Ref(env.add(val))
            }
        },
        Expr::MutRef(_fl, expr) => match *expr {
            Expr::Identifer(fl, s) => Value::MutRef(env.get_mut_index(&s).ok_or_else(|| RuntimeError::new(fl, "no value bound to name"))?),
            expr => {
                let val = eval(expr, env)?;
                Value::MutRef(env.add(val))
            }
        },
        Expr::Deref(fl, expr) => {
            match eval(*expr, env)? {
                Value::Some(val) => *val,
                Value::Primitive(LNone) => panic!("Value was None :("),
                // TODO handle n
                Value::Ref(_n) | Value::MutRef(_n) => env.index(0).clone(),
                v => rte!(fl, "Cannot deref value: {:?}", v)
            }
        }
        Expr::Member(fl, expr, ident) => {
            match (eval(*expr, env)?, &*ident) {
                (Value::Array(v), "len") => Value::Primitive(Uint(v.len() as u64)),
                (Value::Primitive(LString(s)), "len") => Value::Primitive(Uint(s.len() as u64)),
                (Value::Tuple(v), s) => if let Ok(n) = s.parse() {
                        if let Some(elem) = v.get::<usize>(n) {
                            elem.clone()
                        } else {
                            rte!(fl, "tuple index out of bounds")
                        }
                } else {
                    rte!(fl, "can only use numbers as members on a tuple")
                }
                _ => rte!(fl, "No such member {} on that type", ident),
            }
        }
        Expr::Index(fl, indexable, index) => {
            let index = match eval(*index, env)? {
                Value::Primitive(Uint(i) | AmbigInt(i)) => Ok(i as usize),
                Value::Tuple(mut tu) => {
                    if tu.len() == 2 {
                        let a = tu.swap_remove(0);
                        let b = tu.swap_remove(0);
                        match (a, b) {
                            (Value::Primitive(Uint(i) | AmbigInt(i)), Value::Primitive(Uint(j) | AmbigInt(j))) => {
                                Err((i as usize, j as usize))
                            }
                            _ => rte!(fl, "can only interpret two uints as a range")
                        }
                    } else {
                        rte!(fl, "can only interpret a tuple of two elements as a range")
                    }
                }
                _ => rte!(fl, "only a uint or tuple can be an index"),
            };
            match (eval(*indexable, env)?, index) {
                (Value::Array(mut v), Ok(i)) => v.swap_remove(i),
                (Value::Array(mut v), Err((i, j))) => {
                    v.drain(j..);
                    v.drain(..i);
                    Value::Array(v)
                }
                (Value::Primitive(LString(s)), Ok(i)) => Value::Primitive(LString(s.chars().nth(i).ok_or_else(|| RuntimeError::new(fl,"character at that index"))?.to_string())),
                (Value::Primitive(LString(s)), Err((i, j))) => Value::Primitive(LString(
                    s.chars().skip(i).take(j-i).collect()
                )),
                _ => rte!(fl, "Invalid index"),
            }
        }
        // TODO should be able to access outer scope
        Expr::Block(_fl, statements) => run_statements(statements, &mut env.scope(None))?,
        Expr::Function(_fl, arg_names, body) => {
            Value::Function(Function::Implemented(arg_names, match *body {
                Expr::Block(_fl, statements) => statements,
                expr => vec![Statement::Return(expr)],
            }))
        }
    })
}