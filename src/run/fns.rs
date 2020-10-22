use std::ops::{Add, Mul, Div, Rem, Shl, Shr, BitAnd, BitOr, BitXor};

/*
    "+" => "add"
    "-" => "sub"
    "*" => "mul"
    "/" => "div"
    "%" => "rem"
    "++" => "concat"
    "**" => "pow"
    "==" => "eq"
    "!=" => "neq"
    ">" => "gt"
    ">=" => "gte"
    "<" => "lt"
    "<=" => "lte"
    "&&" => "and"
    "||" => "or"
    "<<" => "shl"
    ">>" => "shr"
    "&" => "bitand"
    "^" => "xor"
    "|" => "bitor"
    "-" => "neg"
    "!" => "not"
*/

use super::{Value, Enviroment};
use crate::compile::Literal::{String as LString, AmbigInt, Bool, Int, Uint, Float, Unit, None as LNone};

macro_rules! args {
    ($fn:ident, $args:expr; $($arg:ident),*) => {
        args!(@ARGS $args; $($arg),*);
        assert!($args.is_empty(), "too many args");
    };
    (@ARGS $args:expr;) => {};
    (@ARGS $args:expr; $arg:ident $(, $rest:ident)*) => {
        args!(@ARGS $args; $($rest),*);
        let $arg = $args.pop().expect(concat!("arg ", stringify!($arg)));
    };
}

pub fn id(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(id, args; arg);
    arg
}

macro_rules! bin_op {
    ($name:ident, $op:expr $(,$ext:ident)*) => {
        pub fn $name(mut args: Vec<Value>, _env: &Enviroment) -> Value {
            args!($name, args; a, b);
            match (a, b) {
                (Value::Literal(Int(a)), Value::Literal(Int(b))) => Value::Literal(Int($op(a, b))),
                (Value::Literal(AmbigInt(a)), Value::Literal(Int(b))) => Value::Literal(Int($op(a as i64, b))),
                (Value::Literal(Int(a)), Value::Literal(AmbigInt(b))) => Value::Literal(Int($op(a, b as i64))),
                (Value::Literal(Uint(a) | AmbigInt(a)), Value::Literal(Uint(b) | AmbigInt(b))) => Value::Literal(Uint($op(a, b))),
                $((Value::Literal($ext(a)), Value::Literal($ext(b))) => Value::Literal($ext($op(a, b))),)*
                _ => panic!("mismatched types"),
            }
        }
    };
}
pub fn sub(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(sub, args; a, b);
    match (a, b) {
        (Value::Literal(AmbigInt(a)), Value::Literal(AmbigInt(b))) => {
            Value::Literal(match (a.checked_sub(b), (a as i64).checked_sub(b as i64)) {
                (Some(n), Some(_) | None) => AmbigInt(n),
                (None, Some(n)) => Int(n),
                (None, None) => panic!("sub overflowed")
            })
        }
        (Value::Literal(Int(a)), Value::Literal(Int(b))) => Value::Literal(Int(a - b)),
        (Value::Literal(AmbigInt(a)), Value::Literal(Int(b))) => Value::Literal(Int(a as i64 - b)),
        (Value::Literal(Int(a)), Value::Literal(AmbigInt(b))) => Value::Literal(Int(a - b as i64)),
        (Value::Literal(Uint(a) | AmbigInt(a)), Value::Literal(Uint(b) | AmbigInt(b))) => Value::Literal(Uint(a - b)),
        (Value::Literal(Float(a)), Value::Literal(Float(b))) => Value::Literal(Float(a - b)),
        _ => panic!("mismatched types"),
    }
}

bin_op!(add, Add::add, Float);
bin_op!(mul, Mul::mul, Float);
bin_op!(div, Div::div, Float);
bin_op!(rem, Rem::rem, Float);
bin_op!(shl, Shl::shl);
bin_op!(shr, Shr::shr);
bin_op!(bitand, BitAnd::bitand, Bool);
bin_op!(xor, BitXor::bitxor, Bool);
bin_op!(bitor, BitOr::bitor, Bool);

macro_rules! comp_eq {
    ($name:ident, $op:expr) => {
        pub fn $name(mut args: Vec<Value>, _env: &Enviroment) -> Value {
            args!($name, args; a, b);
            Value::Literal(Bool($op(&a, &b)))
        }
    };
}
macro_rules! comp_cmp {
    ($name:ident, $op:expr) => {
        pub fn $name(mut args: Vec<Value>, _env: &Enviroment) -> Value {
            args!($name, args; a, b);
            match (a, b) {
                (Value::Literal(Int(a)), Value::Literal(Int(b))) => Value::Literal(Bool($op(&a, &b))),
                (Value::Literal(AmbigInt(a)), Value::Literal(Int(b))) => Value::Literal(Bool($op(&(a as i64), &b))),
                (Value::Literal(Int(a)), Value::Literal(AmbigInt(b))) => Value::Literal(Bool($op(&a, &(b as i64)))),
                (Value::Literal(Uint(a) | AmbigInt(a)), Value::Literal(Uint(b) | AmbigInt(b))) => Value::Literal(Bool($op(&a, &b))),
                (Value::Literal(Bool(a)), Value::Literal(Bool(b))) => Value::Literal(Bool($op(&a, &b))),
                (Value::Literal(Float(a)), Value::Literal(Float(b))) => Value::Literal(Bool($op(&a, &b))),
                (Value::Literal(LString(a)), Value::Literal(LString(b))) => Value::Literal(Bool($op(&a, &b))),
                _ => panic!("mismatched types"),
            }
        }
    };
}

use std::cmp::{PartialEq, PartialOrd};

comp_eq!(eq, PartialEq::eq);
comp_eq!(neq, PartialEq::ne);
comp_cmp!(gt, PartialOrd::gt);
comp_cmp!(gte, PartialOrd::ge);
comp_cmp!(lt, PartialOrd::lt);
comp_cmp!(lte, PartialOrd::le);

pub fn neg(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(neg, args; arg);
    match arg {
        Value::Literal(Int(a)) => Value::Literal(Int(-a)),
        Value::Literal(Float(a)) => Value::Literal(Float(-a)),
        _ => panic!("mismatched types"),
    }
}

pub fn not(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(not, args; arg);
    match arg {
        Value::Literal(Int(a)) => Value::Literal(Int(!a)),
        Value::Literal(AmbigInt(a)) => Value::Literal(AmbigInt(!a)),
        Value::Literal(Uint(a)) => Value::Literal(Uint(!a)),
        Value::Literal(Bool(a)) => Value::Literal(Bool(!a)),
        _ => panic!("mismatched types"),
    }
}

pub fn concat(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(concat, args; a, b);
    match (a, b) {
        (Value::Array(mut a), Value::Array(mut b)) => Value::Array({a.append(&mut b); a}),
        (Value::Literal(LString(a)), Value::Literal(LString(b))) => Value::Literal(LString(a + &b)),
        _ => panic!("mismatched types"),
    }
}

pub fn pow(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(pow, args; a, b);
    match (a, b) {
        (Value::Literal(Int(a)), Value::Literal(Uint(b) | AmbigInt(b))) => Value::Literal(Int(a.pow(b as u32))),
        (Value::Literal(AmbigInt(a)), Value::Literal(Uint(b) | AmbigInt(b))) => Value::Literal(AmbigInt(a.pow(b as u32))),
        (Value::Literal(Uint(a)), Value::Literal(Uint(b) | AmbigInt(b))) => Value::Literal(Uint(a.pow(b as u32))),
        (Value::Literal(Float(a)), Value::Literal(Int(b))) => Value::Literal(Float(a.powi(b as i32))),
        (Value::Literal(Float(a)), Value::Literal(AmbigInt(b))) => Value::Literal(Float(a.powi(b as i32))),
        (Value::Literal(Float(a)), Value::Literal(Float(b))) => Value::Literal(Float(a.powf(b))),
        _ => panic!("mismatched types"),
    }
}

pub fn print(args: Vec<Value>, env: &Enviroment) -> Value {
    write(args, env);
    println!();
    Value::Literal(Unit)
}
pub fn write(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(write, args; arg);

    match arg {
        Value::Literal(LString(s)) => print!("{}", s),
        _ => panic!("can only print string"),
    }

    Value::Literal(Unit)
}
fn show_inner(arg: Value) -> String {
    match arg {
        Value::Literal(LString(s)) => s,
        Value::Literal(Int(i)) => format!("{}", i),
        Value::Literal(Uint(i)) => format!("{}", i),
        Value::Literal(AmbigInt(i)) => format!("{}", i),
        Value::Literal(Float(f)) => format!("{}", f),
        Value::Literal(Bool(b)) => format!("{}", b),
        Value::Literal(Unit) => "()".to_owned(),
        Value::Literal(LNone) => "None".to_owned(),
        Value::Some(box val) => format!("Some({})", show_inner(val)),
        Value::Tuple(vs) => format!("{:?}", vs),
        Value::Array(vs) => format!("{:?}", vs),
        Value::Function(f) => format!("{:?}", f),
    }
}
pub fn show(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(show, args; arg);
    Value::Literal(LString(show_inner(arg)))
}