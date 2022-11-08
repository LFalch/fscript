use std::ops::{Add, Mul, Div, Rem, Shl, Shr, BitAnd, BitOr, BitXor};

use super::{Value, Enviroment};
use crate::source::ast::Primitive::{String as LString, AmbigInt, Bool, Int, Uint, Float, Unit, None as LNone};

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

pub(super) fn id(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(id, args; arg);
    arg
}

macro_rules! bin_op {
    ($name:ident, $op:expr $(,$ext:ident)*) => {
        pub(super) fn $name(mut args: Vec<Value>, _env: &Enviroment) -> Value {
            args!($name, args; a, b);
            match (a, b) {
                (Value::Primitive(Int(a)), Value::Primitive(Int(b))) => Value::Primitive(Int($op(a, b))),
                (Value::Primitive(AmbigInt(a)), Value::Primitive(AmbigInt(b))) => Value::Primitive(AmbigInt($op(a, b))),
                (Value::Primitive(AmbigInt(a)), Value::Primitive(Int(b))) => Value::Primitive(Int($op(a as i64, b))),
                (Value::Primitive(Int(a)), Value::Primitive(AmbigInt(b))) => Value::Primitive(Int($op(a, b as i64))),
                (Value::Primitive(Uint(a) | AmbigInt(a)), Value::Primitive(Uint(b) | AmbigInt(b))) => Value::Primitive(Uint($op(a, b))),
                $((Value::Primitive($ext(a)), Value::Primitive($ext(b))) => Value::Primitive($ext($op(a, b))),)*
                _ => panic!("mismatched types"),
            }
        }
    };
}
pub(super) fn sub(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(sub, args; a, b);
    match (a, b) {
        (Value::Primitive(AmbigInt(a)), Value::Primitive(AmbigInt(b))) => {
            Value::Primitive(match (a.checked_sub(b), (a as i64).checked_sub(b as i64)) {
                (Some(n), Some(_) | None) => AmbigInt(n),
                (None, Some(n)) => Int(n),
                (None, None) => panic!("sub overflowed")
            })
        }
        (Value::Primitive(Int(a)), Value::Primitive(Int(b))) => Value::Primitive(Int(a - b)),
        (Value::Primitive(AmbigInt(a)), Value::Primitive(Int(b))) => Value::Primitive(Int(a as i64 - b)),
        (Value::Primitive(Int(a)), Value::Primitive(AmbigInt(b))) => Value::Primitive(Int(a - b as i64)),
        (Value::Primitive(Uint(a) | AmbigInt(a)), Value::Primitive(Uint(b) | AmbigInt(b))) => Value::Primitive(Uint(a - b)),
        (Value::Primitive(Float(a)), Value::Primitive(Float(b))) => Value::Primitive(Float(a - b)),
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
        pub(super) fn $name(mut args: Vec<Value>, _env: &Enviroment) -> Value {
            args!($name, args; a, b);
            Value::Primitive(Bool($op(&a, &b)))
        }
    };
}
macro_rules! comp_cmp {
    ($name:ident, $op:expr) => {
        pub(super) fn $name(mut args: Vec<Value>, _env: &Enviroment) -> Value {
            args!($name, args; a, b);
            match (a, b) {
                (Value::Primitive(Int(a)), Value::Primitive(Int(b))) => Value::Primitive(Bool($op(&a, &b))),
                (Value::Primitive(AmbigInt(a)), Value::Primitive(Int(b))) => Value::Primitive(Bool($op(&(a as i64), &b))),
                (Value::Primitive(Int(a)), Value::Primitive(AmbigInt(b))) => Value::Primitive(Bool($op(&a, &(b as i64)))),
                (Value::Primitive(Uint(a) | AmbigInt(a)), Value::Primitive(Uint(b) | AmbigInt(b))) => Value::Primitive(Bool($op(&a, &b))),
                (Value::Primitive(Bool(a)), Value::Primitive(Bool(b))) => Value::Primitive(Bool($op(&a, &b))),
                (Value::Primitive(Float(a)), Value::Primitive(Float(b))) => Value::Primitive(Bool($op(&a, &b))),
                (Value::Primitive(LString(a)), Value::Primitive(LString(b))) => Value::Primitive(Bool($op(&a, &b))),
                (a, b) => panic!("mismatched types {:?} {:?}", a, b),
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

pub(super) fn neg(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(neg, args; arg);
    match arg {
        Value::Primitive(Int(a)) => Value::Primitive(Int(-a)),
        Value::Primitive(AmbigInt(a)) => Value::Primitive(Int(-(a as i64))),
        Value::Primitive(Float(a)) => Value::Primitive(Float(-a)),
        _ => panic!("mismatched types {:?}", arg),
    }
}

pub(super) fn not(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(not, args; arg);
    match arg {
        Value::Primitive(Int(a)) => Value::Primitive(Int(!a)),
        Value::Primitive(AmbigInt(a)) => Value::Primitive(AmbigInt(!a)),
        Value::Primitive(Uint(a)) => Value::Primitive(Uint(!a)),
        Value::Primitive(Bool(a)) => Value::Primitive(Bool(!a)),
        _ => panic!("mismatched types"),
    }
}

pub(super) fn concat(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(concat, args; a, b);
    match (a, b) {
        (Value::Array(mut a), Value::Array(mut b)) => Value::Array({a.append(&mut b); a}),
        (Value::Primitive(LString(a)), Value::Primitive(LString(b))) => Value::Primitive(LString(a + &b)),
        _ => panic!("mismatched types"),
    }
}

pub(super) fn pow(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(pow, args; a, b);
    match (a, b) {
        (Value::Primitive(Int(a)), Value::Primitive(Uint(b) | AmbigInt(b))) => Value::Primitive(Int(a.pow(b as u32))),
        (Value::Primitive(AmbigInt(a)), Value::Primitive(Uint(b) | AmbigInt(b))) => Value::Primitive(AmbigInt(a.pow(b as u32))),
        (Value::Primitive(Uint(a)), Value::Primitive(Uint(b) | AmbigInt(b))) => Value::Primitive(Uint(a.pow(b as u32))),
        (Value::Primitive(Float(a)), Value::Primitive(Int(b))) => Value::Primitive(Float(a.powi(b as i32))),
        (Value::Primitive(Float(a)), Value::Primitive(AmbigInt(b))) => Value::Primitive(Float(a.powi(b as i32))),
        (Value::Primitive(Float(a)), Value::Primitive(Float(b))) => Value::Primitive(Float(a.powf(b))),
        _ => panic!("mismatched types"),
    }
}

pub(super) fn println(args: Vec<Value>, env: &Enviroment) -> Value {
    print(args, env);
    println!();
    Value::Primitive(Unit)
}
pub(super) fn print(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(write, args; arg);

    match arg {
        Value::Primitive(LString(s)) => print!("{}", s),
        _ => panic!("can only print string"),
    }

    Value::Primitive(Unit)
}
fn show_inner(arg: &Value, env: &Enviroment) -> String {
    match arg {
        Value::Primitive(LString(s)) => s.clone(),
        Value::Primitive(Int(i)) => format!("{}", i),
        Value::Primitive(Uint(i)) => format!("{}", i),
        Value::Primitive(AmbigInt(i)) => format!("{}", i),
        Value::Primitive(Float(f)) => format!("{}", f),
        Value::Primitive(Bool(b)) => format!("{}", b),
        Value::Primitive(Unit) => "()".to_owned(),
        Value::Primitive(LNone) => "None".to_owned(),
        Value::Some(val) => format!("Some({})", show_inner(&*val, env)),
        Value::Tuple(vs) => format!("{:?}", vs),
        Value::Array(vs) => format!("{:?}", vs),
        Value::Function(f) => format!("{:?}", f),
        Value::Ref(n) => format!("&#{}", n),
        Value::MutRef(n) => format!("@#{}", n),
    }
}
pub(super) fn show(mut args: Vec<Value>, env: &Enviroment) -> Value {
    args!(show, args; arg);
    Value::Primitive(LString(match arg {
        Value::Ref(n) => show_inner(env.index(n), env),
        _ => panic!("arg needs to be a reference"),
    }))
}

use std::io::stdin;

pub(super) fn read(args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(read, args;);
    let mut s = String::new();
    stdin().read_line(&mut s).unwrap();
    Value::Primitive(LString(s))
}

pub(super) fn int(mut args: Vec<Value>, _env: &Enviroment) -> Value {
    args!(read, args; arg);
    match arg {
        Value::Primitive(LString(s)) => match s.trim().parse() {
            Ok(i) => Value::Primitive(Int(i)),
            _ => Value::Primitive(LNone),
        }
        _ => panic!("wrong type"),
    }
}
