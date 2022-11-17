use std::{fmt::{self, Debug, Display}, collections::HashMap, mem::replace};

use crate::type_check::ast::*;

type ReturnType = Type;

#[derive(Copy, Clone)]
pub union Value {
    int: i64,
    uint: u64,
    boolean: bool,
    float: f64,
    c: char,
    unit: (),
    pointer: usize,
    f_pointer: usize,
    fat_pointer: (u64, usize),
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            write!(f, "{:8X}{:8X}", self.fat_pointer.0, self.fat_pointer.1)
        }
    }
}

#[derive(Debug)]
pub struct Return {
    pub value: Value,
    pub t: Type,
    pub display: String,
}

pub fn run(stmnts: impl IntoIterator<Item=Statement>) -> Option<Return> {
    let mut stack = Vec::new();
    let mut heap = Vec::new();
    let mut text = Vec::new();

    let mut env = Environment::new_standard(&mut stack, &mut heap, &mut text);

    #[cfg(feature = "debug_print_statements")]
    let stmnts = stmnts.into_iter().inspect(|statement| println!("{}", statement));

    run_statements(stmnts, &mut env)
        .map(|(value, t)| Return {
            display: format!("{}", env.display_value(&value, &t)),
            value,
            t
        })
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    var_map: HashMap<String, (Type, usize)>,
    fn_map: HashMap<String, HashMap<(Type, ReturnType), usize>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable { var_map: HashMap::new(), fn_map: HashMap::new() }
    }
}

/// The environment of variables declared in some scope
#[derive(Debug)]
pub struct Environment<'a> {
    stack: &'a mut Vec<Value>,
    heap: &'a mut Vec<Value>,
    text: &'a mut Vec<Function>,
    table: SymbolTable,
    parent_stack_length: usize,
}

impl Drop for Environment<'_> {
    #[inline]
    fn drop(&mut self) {
        // Drop everything upto the parent stack length
        self.stack.drain(self.parent_stack_length..);
    }
}
mod fns;

impl<'s> Environment<'s> {
    #[inline(always)]
    /// Inputs will be cleared
    fn new_standard(stack: &'s mut Vec<Value>, heap: &'s mut Vec<Value>, text: &'s mut Vec<Function>) -> Self {
        use crate::tinterpreter::fns::*;

        stack.clear();
        text.clear();
        heap.clear();

        let mut env = Environment {
            parent_stack_length: 0,
            table: SymbolTable::new(),
            stack,
            heap,
            text,
        };

        env.add_fn("add", Type::Int, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(addi));
        env.add_fn("add", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(addi));
        env.add_fn("add", Type::Float, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(addf));
        env.add_fn("sub", Type::Int, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(subi));
        env.add_fn("sub", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(subi));
        env.add_fn("sub", Type::Float, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(subf));
        env.add_fn("mul", Type::Int, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(muli));
        env.add_fn("mul", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(muli));
        env.add_fn("mul", Type::Float, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(mulf));
        env.add_fn("div", Type::Int, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(divi));
        env.add_fn("div", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(divi));
        env.add_fn("div", Type::Float, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(divf));
        env.add_fn("rem", Type::Int, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(remi));
        env.add_fn("rem", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(remi));
        env.add_fn("rem", Type::Float, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(remf));
        env.add_fn("shl", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(shl));
        env.add_fn("shr", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(shr));
        env.add_fn("bitand", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(bitand));
        env.add_fn("bitand", Type::Bool, Type::Tuple(vec![Type::Bool, Type::Bool]), Function::Builtin(booland));
        env.add_fn("bitor", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(bitor));
        env.add_fn("bitor", Type::Bool, Type::Tuple(vec![Type::Bool, Type::Bool]), Function::Builtin(boolor));
        env.add_fn("bitxor", Type::Uint, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(bitxor));
        env.add_fn("bitxor", Type::Bool, Type::Tuple(vec![Type::Bool, Type::Bool]), Function::Builtin(boolxor));
        env.add_fn("eq", Type::Bool, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(eqi));
        env.add_fn("eq", Type::Bool, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(eqi));
        env.add_fn("eq", Type::Bool, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(eqf));
        env.add_fn("lt", Type::Bool, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(lti));
        env.add_fn("lt", Type::Bool, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(lti));
        env.add_fn("lt", Type::Bool, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(ltf));
        env.add_fn("lte", Type::Bool, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(ltei));
        env.add_fn("lte", Type::Bool, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(ltei));
        env.add_fn("lte", Type::Bool, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(ltef));
        env.add_fn("gt", Type::Bool, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(gti));
        env.add_fn("gt", Type::Bool, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(gti));
        env.add_fn("gt", Type::Bool, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(gtf));
        env.add_fn("gte", Type::Bool, Type::Tuple(vec![Type::Int, Type::Int]), Function::Builtin(gtei));
        env.add_fn("gte", Type::Bool, Type::Tuple(vec![Type::Uint, Type::Uint]), Function::Builtin(gtei));
        env.add_fn("gte", Type::Bool, Type::Tuple(vec![Type::Float, Type::Float]), Function::Builtin(gtef));
        env.add_fn("not", Type::Bool, Type::Bool, Function::Builtin(notb));
        env.add_fn("not", Type::Uint, Type::Uint, Function::Builtin(noti));
        env.add_fn("neg", Type::Int, Type::Int, Function::Builtin(negi));
        env.add_fn("neg", Type::Float, Type::Float, Function::Builtin(negf));
        // env.add_fn("pow", ...);
        env.add_fn("concat", Type::String, Type::Tuple(vec![Type::String, Type::String]), Function::Builtin(concats));
        env.add_fn("concat", Type::Array(Box::new(Type::Int)), Type::Tuple(vec![Type::Array(Box::new(Type::Int)), Type::Array(Box::new(Type::Int))]), Function::Builtin(concata));
        env.add_fn("concat", Type::Array(Box::new(Type::Uint)), Type::Tuple(vec![Type::Array(Box::new(Type::Uint)), Type::Array(Box::new(Type::Uint))]), Function::Builtin(concata));
        env.add_fn("show", Type::String, Type::Reference(Box::new(Type::Int)), Function::Builtin(showi));
        env.add_fn("show", Type::String, Type::Reference(Box::new(Type::Uint)), Function::Builtin(showu));
        env.add_fn("show", Type::String, Type::Reference(Box::new(Type::Float)), Function::Builtin(showf));
        env.add_fn("show", Type::String, Type::Reference(Box::new(Type::Bool)), Function::Builtin(showb));
        env.add_fn("print", Type::Unit, Type::String, Function::Builtin(print));
        env.add_fn("println", Type::Unit, Type::String, Function::Builtin(println));
        env.add_fn("read", Type::String, Type::Unit, Function::Builtin(read));
        env.add_fn("int", Type::Int, Type::String, Function::Builtin(int));
        env.add_fn("vardump", Type::Unit, Type::Unit, Function::Builtin(vardump));

        env
    }
    fn index(&self, i: usize) -> &Value {
        self.stack.get(i).or_else(|| self.heap.get(i - 0x8000_0000_0000_0000)).expect("Out of bounds")
    }
    fn index_mut(&mut self, i: usize) -> &mut Value {
        self.stack.get_mut(i).or_else(|| self.heap.get_mut(i - 0x8000_0000_0000_0000)).expect("Out of bounds")
    }
    fn get_index(&self, s: &str) -> Option<(Type, usize)> {
        let ret = self.table.var_map.get(s);

        if let Some(ret) = ret {
            Some(ret.clone())
        } else {
            None
        }
    }
    fn get_fn(&self, s: &str) -> impl Iterator<Item=(&Type, &ReturnType, &Function)> {
        self.table.fn_map.get(s).into_iter().flatten().map(|((t, rt), &i)| (t, rt, &self.text[i]))
    }
    #[inline]
    fn get(&self, s: &str) -> Option<&Value> {
        if let Some((_, i)) = self.get_index(s) {
            Some(self.index(i))
        } else {
            None
        }
    }
    fn get_mut(&mut self, s: &str) -> Option<&mut Value> {
        if let Some((_t, i)) = self.get_index(s) {
            Some(self.index_mut(i))
        } else {
            None
        }
    }
    fn get_next_index(&self) -> usize {
        self.stack.len()
    }
    fn get_next_heap_index(&self) -> usize {
        0x8000_0000_0000_0000 + self.heap.len()
    }

    fn add(&mut self, v: Value) -> usize {
        let i = self.get_next_index();
        self.stack.push(v);
        i
    }
    fn add_heap(&mut self, v: Value) -> usize {
        let i = self.get_next_heap_index();
        self.heap.push(v);
        i
    }
    fn add_fn(&mut self, s: impl ToString, rt: ReturnType, t: Type, f: Function) -> usize {
        let i = self.text.len();
        self.text.push(f);

        self.table.fn_map.entry(s.to_string())
            .or_insert_with(|| HashMap::new())
            .insert((t, rt), i);

        i
    }
    fn add_var(&mut self, s: impl ToString, t: Type, v: Value) {
        let i = self.add(v);

        self.table.var_map.insert(s.to_string(), (t, i));
    }
    fn bind(&mut self, b: Binding, t: Type, v: Value) {
        match b {
            Binding::Unit => (),
            Binding::Name(s) => {
                self.add_var(s, t, v);
            }
            Binding::Tuple(bs) => {
                let ts = match t {
                    Type::Tuple(ts) => ts,
                    _ => unreachable!(),
                };

                let mut p = unsafe { v.pointer };

                for (b, t) in bs.into_iter().zip(ts) {
                    let v = *self.index(p);
                    p += 1;
                    self.bind(b, t, v);
                }
            }
        }
    }
    fn scope<'a>(&'a mut self) -> Environment<'a> {
        Environment {
            parent_stack_length: self.stack.len(),
            stack: self.stack,
            heap: self.heap,
            text: self.text,
            table: self.table.clone(),
        }
    }
    fn new_env<'a>(&'a mut self, table: SymbolTable) -> Environment<'a> {
        Environment {
            table,
            parent_stack_length: self.stack.len(),
            text: self.text,
            heap: self.heap,
            stack: self.stack,
        }
    }
    fn new_array(&mut self, values: &[Value]) -> Value {
        let p = self.get_next_heap_index();
        for value in values {
            self.add_heap(*value);
        }
        Value { fat_pointer: (values.len() as u64, p) }
    }
    fn new_tuple(&mut self, values: &[Value]) -> Value {
        let p = self.get_next_heap_index();
        for value in values {
            self.add_heap(*value);
        }
        Value { pointer: p }
    }
    fn new_string(&mut self, s: &str) -> Value {
        let p = self.get_next_heap_index();
        let mut len = 0;
        for c in s.chars() {
            self.add_heap(Value { c });
            len += 1;
        }
        Value { fat_pointer: (len, p) }
    }
    fn display_value<'a>(&'a self, value: &'a Value, t: &'a Type) -> DisplayValue<'s, 'a> {
        DisplayValue(self, value, t)
    }
}

pub struct DisplayValue<'e: 'a, 'a>(&'a Environment<'e>, &'a Value, &'a Type);

impl Display for DisplayValue<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &DisplayValue(env, val, t) = self;

        unsafe { match t {
            NamedType::Unit => write!(f, "{:?}", val.unit),
            NamedType::Bool => write!(f, "{}", val.boolean),
            NamedType::Uint => write!(f, "{}", val.uint),
            NamedType::Int => write!(f, "{}", val.int),
            NamedType::Float => write!(f, "{}", val.float),
            NamedType::Reference(t) => {
                let p = val.pointer;
                let val = env.index(p);
                write!(f, "&{}", DisplayValue(env, val, t))
            }
            NamedType::MutReference(t) => {
                let p = val.pointer;
                let val = env.index(p);
                write!(f, "@{}", DisplayValue(env, val, t))
            }
            NamedType::Option(t) => {
                let p = val.pointer;
                if p == 0 {
                    write!(f, "None")
                } else {
                    let val = env.index(p);
                    write!(f, "Some({})", DisplayValue(env, val, t))
                }
            }
            NamedType::Array(t) => {
                let (len, p) = val.fat_pointer;
                write!(f, "[")?;
                for i in 0..len {
                    let val = env.index(p+i as usize);
                    write!(f, "{},", DisplayValue(env, val, t))?;
                }
                write!(f, "]")
            }
            NamedType::String => {
                let (len, p) = val.fat_pointer;
                write!(f, "\"")?;
                for i in 0..len {
                    match env.index(p+i as usize).c {
                        '\n' => write!(f, "\\n"),
                        '\r' => write!(f, "\\r"),
                        '\t' => write!(f, "\\t"),
                        c => write!(f, "{c}"),
                    }?;
                }
                write!(f, "\"")
            }
            NamedType::Tuple(ts) => {
                let p = val.pointer;
                write!(f, "(")?;
                for (i, t) in ts.iter().enumerate() {
                    let val = env.index(p + i);
                    write!(f, "{},", DisplayValue(env, val, t))?;
                }
                write!(f, ")")
            }
            NamedType::Function(arg_t, rt) => {
                let p = val.f_pointer;
                write!(f, "{arg_t} -> {rt}: {:?}", &env.text[p])
            }
            _ => write!(f, "???"),
        } }
    }
}

#[derive(Clone)]
pub enum Function {
    /// A function whose body is not fscript.
    /// Used for built-in functions.
    Builtin(fn(Value, &mut Environment) -> Value),
    /// A function that is defined in fscript
    Implemented(Binding, Type, SymbolTable, Statements),
}

impl Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin(func) => write!(f, "built-in {:p}", func),
            Function::Implemented(arg, _, _, _) => write!(f, "fn{arg:#}) {{...}}"),
        }
    }
}

impl Function {
    fn call(self, arg: Value, env: &mut Environment) -> Option<Value> {
        match self {
            Function::Implemented(b, t, fn_sym_tab, body) => {
                let ref mut env = env.new_env(fn_sym_tab);

                env.bind(b, t, arg);

                run_statements(body.clone(), env).map(|(v, _)| v)
            }
            Function::Builtin(f) => Some(f(arg, env)),
        }
    }
}

fn run_statements(iter: impl IntoIterator<Item=Statement>, env: &mut Environment<'_>) -> Option<(Value, Type)> {
    let mut last_val = (Value{unit: ()}, Type::Unit);
    
    for statement in iter {
        let mut eval = (Value{unit: ()}, Type::Unit);

        match statement {
            Statement::DiscardExpr(t, expr) => eval = (eval_expr(expr, env)?, t),
            Statement::ConstAssign(b, t, expr)
            | Statement::VarAssign(b, t, expr) => {
                let v = eval_expr(expr, env)?;
                env.bind(b, t, v);
            }
            Statement::Reassign(lhs, expr) => {
                let val = eval_expr(expr, env)?;
                *reassign(lhs, env)? = val;
            }
            Statement::Function(func_name, arg, t, rt, body) => {
                let i = env.add_fn(func_name, rt.clone(), t.clone(), Function::Builtin(fns::not_yet_implemented));

                env.text[i] = Function::Implemented(arg, t, env.table.clone(), match body {
                    Expr::Block(_, statements) => statements,
                    expr => vec![Statement::Return(rt, expr)],
                });
            }
            Statement::Return(rt, expr) => return Some((eval_expr(expr, env)?, rt)),
        }

        match replace(&mut last_val, eval) {
            (_, Type::Unit) => (),
            (_, other) => eprintln!("warning: unused value of type {other}"),
        }
    }

    Some(last_val)
}

fn reassign<'env: 'a, 'a>(lhs: ReassignLhs, env: &'a mut Environment<'env>) -> Option<&'a mut Value> {
    Some(match lhs {
        ReassignLhs::Identifier(ident) => {
            match env.get_mut(&ident) {
                Some(var) => var,
                None => panic!("no such variable in scope: {ident}")
            }
        }
        ReassignLhs::Deref(lhs) => {
            let addr = unsafe { reassign(*lhs, env)?.pointer };
            env.index_mut(addr)
        }
        ReassignLhs::Member(_, _) => unimplemented!("no mutable members exist yet"),
        ReassignLhs::Index(array, index) => unsafe {
            let index = eval_expr(index, env)?.uint;
            let (array_size , array_pointer) = reassign(*array, env)?.fat_pointer;

            if index < array_size {
                env.index_mut(array_pointer + index as usize)
            } else {
                panic!("Index out of bounds {index} >= {array_size}");
            }
        }
    })
}

fn eval_expr(expr: Expr, env: &mut Environment<'_>) -> Option<Value> {
    Some(match expr {
        Expr::Identifer(ident) => if let Some(val) = env.get(&ident) {
            *val
        } else {
            panic!("no such variable {}", ident);
        }
        Expr::Int(i) => Value { uint: i },
        Expr::Bool(b) => Value { boolean: b },
        Expr::Float(f) => Value { float: f },
        Expr::Unit => Value { unit: () },
        Expr::None => Value { pointer: 0 },
        Expr::Some(expr) => {
            let val = eval_expr(*expr, env)?;
            let pointer = env.add(val);
            Value { pointer }
        }
        Expr::String(s) => env.new_string(&s),
        Expr::Array(vec) => {
            let mut values = Vec::with_capacity(vec.len());
            for e in vec {
                values.push(eval_expr(e, env)?);
            }
            env.new_array(&values)
        }
        Expr::Tuple(vec) => {
            let mut values = Vec::with_capacity(vec.len());
            for e in vec {
                values.push(eval_expr(e, env)?);
            }
            env.new_tuple(&values)
        }
        Expr::Call(func, t, rt, arg_expr) => {
            let mut f: Option<Function> = None;
            for (lt, lrt, lf) in env.get_fn(&func) {
                if &rt == lrt && &t == lt {
                    f = Some(lf.clone());
                    break;
                }
            }
            let val = eval_expr(*arg_expr, env)?;
            f.expect(&format!("no such function {func}")).call(val, env)?
        }
        Expr::Ref(expr) | Expr::MutRef(expr) => match *expr {
            Expr::Identifer(s) => Value { pointer: env.get_index(&s).expect("should've been caught by type checker").1 },
            expr => {
                let val = eval_expr(expr, env)?;
                let pointer = env.add(val);
                Value { pointer }
            }
        }
        Expr::Deref(expr) => {
            let pointer = unsafe { eval_expr(*expr, env)?.pointer };
            *env.index(pointer)
        }
        Expr::Member(t, expr, ident) => {
            match (&*ident, t) {
                ("len", Type::Unit) => {
                    let (len, _pointer) = unsafe { eval_expr(*expr, env)?.fat_pointer };

                    Value { uint: len }
                }
                _ => unimplemented!(),
            }
        }
        Expr::Index(indexable, index) => {
            let index = unsafe { eval_expr(*index, env)?.uint };
            let (len, array_pointer) = unsafe { eval_expr(*indexable, env)?.fat_pointer };

            if index < len {
                *env.index(array_pointer + index as usize)
            } else {
                panic!("Index out of bounds {index} >= {len}");
            }
        }
        Expr::If(cond, if_true, if_false) => {
            if unsafe { eval_expr(*cond, env)?.boolean } {
                eval_expr(*if_true, env)?
            } else {
                eval_expr(*if_false, env)?
            }
        }
        Expr::While(cond, body) => {
            let cond = &*cond;
            let body = &*body;
            while unsafe { eval_expr(cond.clone(), env)?.boolean } {
                eval_expr(body.clone(), env)?;
            }
            Value { unit: () }
        }
        Expr::Block(_rt, statements) => {
            // TODO: handle returns
            let (val, _t) = run_statements(statements, &mut env.scope())?;

            val
        }
    })
}
