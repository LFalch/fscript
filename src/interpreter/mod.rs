//! An interpreter that runs the source abstract syntax tree directly
#![warn(missing_docs)]

use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::cmp::PartialEq;
use std::iter;
use std::mem::replace;

use collect_result::CollectResult;
// use lazy_static::__Deref;

use crate::source::ast::ReassignLhs;
use crate::source::ast::{
    Binding,
    Statement,
    Statements,
    Expr,
    Primitive::{self, String as LString, AmbigInt, Uint, Unit, None as LNone},
};

use crate::source::FileSpan;

#[derive(Clone)]
/// A function value
pub enum Function {
    /// A function whose body is not fscript.
    /// Used for built-in functions.
    Builtin(fn(Vec<Value>, &Enviroment) -> Value),
    /// A function that is defined in fscript
    Implemented(Binding, SymbolTable, Statements),
}

impl Function {
    fn call(self, arg: Value, env: &mut Enviroment) -> Result<Value, RuntimeError> {
        match self {
            Function::Implemented(bindings, fn_sym_tab, body) => {
                let Enviroment { stack, .. } = env;
                let parent_stack_length = stack.len();

                let ref mut env = Enviroment { stack: *stack, parent_stack_length, table: SymbolTable::new(), parent_tables: Box::new([&fn_sym_tab]) };

                env.bind(bindings, true, arg);

                run_statements(body.clone(), env).map(StatementsOk::get_return)
            }
            Function::Builtin(f) => {
                let args = match arg {
                    Value::Tuple(v) => v,
                    Value::Primitive(Unit) => Vec::new(),
                    v => vec![v],
                };
                Ok(f(args, &*env))
            }
        }
    }
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
            Function::Implemented(args, _, _) => write!(f, "fn{args:#} {{...}}")
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

type SymbolTable = HashMap<String, (bool, usize)>;

/// The environment of variables declared in some scope
#[derive(Debug)]
pub struct Enviroment<'a> {
    stack: &'a mut Vec<Value>,
    table: SymbolTable,
    parent_stack_length: usize,
    parent_tables: Box<[&'a SymbolTable]>,
}

impl Drop for Enviroment<'_> {
    #[inline]
    fn drop(&mut self) {
        // Drop everything upto the parent stack length
        self.stack.drain(self.parent_stack_length..);
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
            parent_stack_length: 0,
            parent_tables: Box::new([]),
            table: SymbolTable::new(),
        };
        #[inline]
        fn c(f: fn(Vec<Value>, &Enviroment) -> Value) -> Function {
            Function::Builtin(f)
        }

        env.add_fun("id", c(id));
        env.add_fun("add", c(add));
        env.add_fun("sub", c(sub));
        env.add_fun("mul", c(mul));
        env.add_fun("div", c(div));
        env.add_fun("rem", c(rem));
        env.add_fun("shl", c(shl));
        env.add_fun("shr", c(shr));
        env.add_fun("bitand", c(bitand));
        env.add_fun("xor", c(xor));
        env.add_fun("bitor", c(bitor));
        env.add_fun("eq", c(eq));
        env.add_fun("neq", c(neq));
        env.add_fun("gt", c(gt));
        env.add_fun("gte", c(gte));
        env.add_fun("lt", c(lt));
        env.add_fun("lte", c(lte));
        env.add_fun("neg", c(neg));
        env.add_fun("not", c(not));
        env.add_fun("concat", c(concat));
        env.add_fun("pow", c(pow));
        env.add_fun("print", c(print));
        env.add_fun("println", c(println));
        env.add_fun("show", c(show));
        env.add_fun("read", c(read));
        env.add_fun("int", c(int));

        env
    }
    fn index(&self, i: usize) -> &Value {
        &self.stack[i]
    }
    fn index_mut(&mut self, i: usize) -> &mut Value {
        &mut self.stack[i]
    }
    fn get_index(&self, s: &str) -> Option<(bool, usize)> {
        let ret = self.table.get(s);

        if let Some(ret) = ret {
            Some(*ret)
        } else {
            for table in self.parent_tables.iter().rev() {
                if let Some(r) = table.get(s) {
                    return Some(*r);
                }
            }

            None
        }
    }
    #[inline]
    fn get(&self, s: &str) -> Option<&Value> {
        if let Some((_, index)) = self.get_index(s) {
            Some(&self.stack[index])
        } else {
            None
        }
    }
    fn get_mut(&mut self, s: &str) -> Option<&mut Value> {
        if let Some((true, i)) = self.get_index(s) {
            Some(self.index_mut(i))
        } else {
            None
        }
    }
    fn get_next_index(&self) -> usize {
        self.stack.len()
    }

    fn add(&mut self, v: Value) -> usize {
        let i = self.get_next_index();
        match &v {
            Value::Ref(r) | Value::MutRef(r) if r >= &i => panic!("Dangling reference"),
            _ => ()
        }
        self.stack.push(v);
        i
    }
    fn bind(&mut self, b: Binding, mutable: bool, v: Value) {
        match (b, v) {
            (Binding::Unit, Value::Primitive(Unit)) => (),
            (Binding::Unit, v) => panic!("{v:?} was not a unit"),
            (Binding::Name(s), v) => {
                let i = self.add(v);

                self.table.insert(s, (mutable, i));
            }
            (Binding::Tuple(names), Value::Tuple(values)) => {
                if names.len() != values.len() {
                    panic!("non-matching tuple sizes to destructure")
                }
                for (name, value) in names.into_iter().zip(values) {
                    self.bind(name, mutable, value);
                }
            }
            (Binding::Tuple(_), v) => panic!("{v:?} was not a tuple"),
        }
    }
    fn add_fun(&mut self, s: impl ToString, f: Function) {
        let i = self.add(Value::Function(f));

        self.table.insert(s.to_string(), (false, i));
    }
    fn scope<'a>(&'a mut self) -> Enviroment<'a> {
        Enviroment {
            parent_stack_length: self.stack.len(),
            stack: self.stack,
            table: SymbolTable::new(),
            parent_tables: self.parent_tables.iter().copied().chain(iter::once(&self.table)).collect(),
        }
    }
    fn collapse_symbol_table(&self) -> SymbolTable {
        let mut st = SymbolTable::new();

        for pt in self.parent_tables.iter().chain(iter::once(&&self.table)) {
            for (k, v) in *pt {
                st.insert(k.clone(), *v);
            }
        }

        st
    }
}

#[derive(Debug, Clone)]
/// Represents an error that occured whilst interpreting the program
pub struct RuntimeError {
    /// Location in the source file where error occured
    file_loc: FileSpan,
    /// Description of the error
    error: String,
}

impl RuntimeError {
    fn new<S: ToString>(fl: FileSpan, error: S) -> Self {
        RuntimeError { file_loc: fl, error: error.to_string() }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{} - {}:{} {}", self.file_loc.start.line, self.file_loc.start.col, self.file_loc.end.line, self.file_loc.end.col, self.error)
    }
}

macro_rules! rte {
    ($fl:expr, $s:expr) => {
        Err(RuntimeError::new($fl, format!($s)))?
    };
    ($fl:expr, $s:expr, $($a:expr),+) => {
        Err(RuntimeError::new($fl, format!($s, $($a),+)))?
    };
}

/// Runs an iterator of statements and returns the value returned by those statements
pub fn run(iter: impl IntoIterator<Item=Statement>) -> Result<Value, RuntimeError> {
    let mut stack = Vec::new();
    let mut env = Enviroment::new_standard(&mut stack);

    #[cfg(feature = "debug_print_statements")]
    let iter = iter.into_iter().inspect(|statement| println!("{:?}", statement));

    run_statements(iter, &mut env).map(StatementsOk::get_return)
}

#[derive(Debug, Clone, PartialEq)]
enum StatementsOk {
    Return(Value),
    LastVal(Value),
}

impl StatementsOk {
    fn get_return(self) -> Value {
        match self {
            StatementsOk::Return(v) | StatementsOk::LastVal(v) => v
        }
    }
}

macro_rules! reassign {
    ($e:expr, $env:expr) => {
        match reassign($e, $env) {
            Ok(v) => v,
            Err(NoValue::RuntimeError(e)) => return Err(e),
            Err(NoValue::Return(r)) => return Ok(StatementsOk::Return(r)),
        }
    };
}

macro_rules! eval {
    ($e:expr, $env:expr) => {
        match eval($e, $env) {
            Ok(v) => v,
            Err(NoValue::RuntimeError(e)) => return Err(e),
            Err(NoValue::Return(r)) => return Ok(StatementsOk::Return(r)),
        }
    };
}

fn run_statements(iter: impl IntoIterator<Item=Statement>, env: &mut Enviroment<'_>) -> Result<StatementsOk, RuntimeError> {
    let mut last_val = Value::Primitive(Unit);
    
    for statement in iter {
        let mut eval = Value::Primitive(Unit);

        match statement {
            Statement::DiscardExpr(expr) => eval = eval!(expr, env),
            Statement::ConstAssign(_, b, t, expr) => {
                let val = eval!(expr, env);
                env.bind(b, false, val);
                if !t.is_inferred() {
                    eprintln!("warning: type annotations are ignored in interpreter mode")
                }
            }
            Statement::VarAssign(_, b, t, expr) => {
                let val = eval!(expr, env);
                env.bind(b, true, val);
                if !t.is_inferred() {
                    eprintln!("warning: type annotations are ignored in interpreter mode")
                }
            }
            Statement::Reassign(_, lhs, expr) => {
                let val = eval!(expr, env);
                *reassign!(lhs, env) = val;
            }
            Statement::Function(_fl, func_name, arg_names, _arg_type, body) => {
                let i = env.get_next_index();
                env.add_fun(func_name, Function::Builtin(fns::dummy));

                env.stack[i] = Value::Function(Function::Implemented(arg_names, env.collapse_symbol_table(), match body {
                    Expr::Block(_fl, statements) => statements,
                    expr => vec![Statement::Return(expr.file_span(), expr)],
                }));
            }
            Statement::Return(_, expr) => return Ok(StatementsOk::Return(eval!(expr, env))),
        }

        match replace(&mut last_val, eval) {
            Value::Primitive(Unit) => (),
            other => eprintln!("warning: unused value {:?}", other),
        }
    }

    Ok(StatementsOk::LastVal(last_val))
}

fn reassign<'env: 'a, 'a>(lhs: ReassignLhs, env: &'a mut Enviroment<'env>) -> Result<&'a mut Value, NoValue> {
    Ok(match lhs {
        ReassignLhs::Identifier(_, ident) => {
            match env.get_mut(&ident) {
                Some(var) => var,
                // TODO: Make this a runtime error and use file span
                None => panic!("no such variable in scope: {ident}")
            }
        }
        ReassignLhs::Deref(_, lhs) => {
            let addr = match reassign(*lhs, env)? {
                Value::MutRef(n) => *n,
                _ => panic!("not a mutable reference"),
            };
            env.index_mut(addr)
        }
        ReassignLhs::Member(_, _, _) => unimplemented!("no mutable members exist yet"),
        ReassignLhs::Index(_, array, index) => {
            let index = eval(index, env)?;
            match (reassign(*array, env)?, index) {
                (Value::Array(v), Value::Primitive(AmbigInt(i) | Uint(i))) => &mut v[i as usize],
                _ => panic!("not an array"),
            }
        }
    })
}

#[derive(Debug, Clone)]
/// No value was evaluated
enum NoValue {
    /// Return
    Return(Value),
    /// Represents an error that occured whislt interpreting the program
    RuntimeError(RuntimeError) 
}

impl From<RuntimeError> for NoValue {
    fn from(re: RuntimeError) -> Self {
        NoValue::RuntimeError(re)
    }
}

fn eval(expr: Expr, env: &mut Enviroment<'_>) -> Result<Value, NoValue> {
    Ok(match expr {
        Expr::Identifer(fl, ident) => if let Some(val) = env.get(&ident) {
            val.clone()
        } else {
            rte!(fl, "no such variable {}\n{env:#?}", ident)
        }
        Expr::Constant(_fl, lit) => Value::Primitive(lit),
        Expr::Some(_fl, expr) => {
            let expr = *expr;
            Value::Some(Box::new(eval(expr, env)?))
        }
        Expr::Array(_fl, vec) => Value::Array(vec.into_iter().map(|expr| eval(expr, env)).collect_result()?),
        Expr::Tuple(_fl, vec) => Value::Tuple(vec.into_iter().map(|expr| eval(expr, env)).collect_result()?),
        Expr::Call(fl, func, arg_expr) => {
            let f = match env.get(&func) {
                Some(Value::Function(f)) => f,
                Some(_) => rte!(fl, "{} is not a function", func),
                None => rte!(fl, "no such function {}", func),
            }.clone();

            f.call(eval(*arg_expr, env)?, env)?
        }
        Expr::Ref(_fl, expr) => match *expr {
            Expr::Identifer(fl, s) => Value::Ref(env.get_index(&s).ok_or_else(|| RuntimeError::new(fl, "no value bound to name"))?.1),
            expr => {
                let val = eval(expr, env)?;
                Value::Ref(env.add(val))
            }
        },
        Expr::MutRef(_fl, expr) => match *expr {
            Expr::Identifer(fl, s) => Value::MutRef(
                env
                    .get_index(&s)
                    .and_then(|(b, i)| if b { Some(i) } else { None })
                    .ok_or_else(|| RuntimeError::new(fl, "no mutable value bound to name"))?
            ),
            expr => {
                let val = eval(expr, env)?;
                Value::MutRef(env.add(val))
            }
        },
        Expr::Deref(fl, expr) => {
            match eval(*expr, env)? {
                Value::Some(val) => *val,
                Value::Primitive(LNone) => panic!("Value was None :("),
                Value::Ref(n) | Value::MutRef(n) => env.stack[n].clone(),
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
        Expr::If(_fl, cond, if_true, if_false) => {
            let cond_fl = cond.file_span();
            match eval(*cond, env)? {
                Value::Primitive(Primitive::Bool(true)) => eval(*if_true, env)?,
                Value::Primitive(Primitive::Bool(false)) => eval(*if_false, env)?,
                _ => rte!(cond_fl, "Condition was not a boolean"),
            }
        }
        Expr::While(fl, cond, body) => {
            let cond = &*cond;
            let body = &*body;
            loop {
                match eval(cond.clone(), env)? {
                    Value::Primitive(Primitive::Bool(true)) => {
                        eval(body.clone(), env)?;
                    }
                    Value::Primitive(Primitive::Bool(false)) => break,
                    _ => rte!(fl, "Condition was not a boolean"),
                }
            }
            Value::Primitive(Unit)
        }
        Expr::Block(_fl, statements) => {
            match run_statements(statements, &mut env.scope())? {
                StatementsOk::Return(v) => return Err(NoValue::Return(v)),
                StatementsOk::LastVal(v) => v,
            }
        }
    })
}
