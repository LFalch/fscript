use std::ops::{Add, Sub, Mul, Div, Rem, Shl, Shr, BitAnd, BitOr, BitXor, Not, Neg};
use std::cmp::{PartialEq, PartialOrd};

use super::{Value, Environment};

macro_rules! binop {
    (
        $($name:ident, $value_type:ident, $op:expr;)*
    ) => {
        $(
        pub fn $name(v: Value, env: &mut Environment) -> Value {
            unsafe {
                let p = v.pointer;
    
                Value { $value_type: $op(env.index(p).$value_type, env.index(p+1).$value_type) }
            }
        }
        )*
    };
}

binop!{
    addi, uint, Add::add;
    addf, float, Add::add;
    subi, uint, Sub::sub;
    subf, float, Sub::sub;
    muli, uint, Mul::mul;
    mulf, float, Mul::mul;
    divi, uint, Div::div;
    divf, float, Div::div;
    remi, uint, Rem::rem;
    remf, float, Rem::rem;
    shl, uint, Shl::shl;
    shr, uint, Shr::shr;
    bitand, uint, BitAnd::bitand;
    booland, boolean, BitAnd::bitand;
    bitor, uint, BitOr::bitor;
    boolor, boolean, BitOr::bitor;
    bitxor, uint, BitXor::bitxor;
    boolxor, boolean, BitXor::bitxor;
}

macro_rules! cmp {
    (
        $($name:ident, $value_type:ident, $op:expr;)*
    ) => {
        $(
        pub fn $name(v: Value, env: &mut Environment) -> Value {
            unsafe {
                let p = v.pointer;

                Value { boolean: $op(&env.index(p).$value_type, &env.index(p+1).$value_type) }
            }
        }
        )*
    };
}

fn gte<I: PartialEq + PartialOrd>(a: I, b: I) -> bool {
    a.gt(&b) || a.eq(&b)
}

fn lte<I: PartialEq + PartialOrd>(a: I, b: I) -> bool {
    a.lt(&b) || a.eq(&b)
}

cmp!{
    eqi, int, PartialEq::eq;
    eqf , float, PartialEq::eq;
    gti, int, PartialOrd::gt;
    gtf, float, PartialOrd::gt;
    gtei, int, gte;
    gtef, float, gte;
    lti, int, PartialOrd::lt;
    ltf, float, PartialOrd::lt;
    ltei, int, lte;
    ltef, float, lte;
}

macro_rules! unop {
    (
        $($name:ident, $value_type:ident, $op:expr;)*
    ) => {
        $(
        pub fn $name(v: Value, _env: &mut Environment) -> Value {
            unsafe { Value { $value_type: $op(v.$value_type) } }
        }
        )*
    };
}

unop!{
    notb, boolean, Not::not;
    noti, uint, Not::not;
    negi, int, Neg::neg;
    negf, float, Neg::neg;
}

use std::io::stdin;

pub(super) fn read(_v: Value, env: &mut Environment) -> Value {
    let mut s = String::new();
    stdin().read_line(&mut s).unwrap();
    env.new_string(&s)
}

pub(super) fn int(v: Value, env: &mut Environment) -> Value {
    let (len, p) = unsafe { v.fat_pointer };
    let mut s = String::with_capacity(len as usize);

    for i in 0..len {
        s.push(unsafe { env.index(p+i as usize).c });
    }

    Value { int: s.parse().unwrap() }
}

pub(super) fn print(v: Value, env: &mut Environment) -> Value {
    let (len, p) = unsafe { v.fat_pointer };
    let mut s = String::with_capacity(len as usize);

    for i in 0..len {
        s.push(unsafe { env.index(p+i as usize).c });
    }

    print!("{s}");

    Value { unit: () }
}

pub(super) fn println(v: Value, env: &mut Environment) -> Value {
    let v = print(v, env);
    println!();
    v
}

pub(super) fn not_yet_implemented(_arg: Value, _env: &mut Environment) -> Value {
   panic!("aaaaarh");
}

macro_rules! show {
    ($($name:ident, $t:ident;)*) => {
        $(pub(super) fn $name(arg: Value, env: &mut Environment) -> Value {
            let p = unsafe { arg.pointer };
            let i = unsafe { env.index(p).$t };

            env.new_string(&format!("{i}"))
        })*
    };
}

show!{
    showi, int;
    showu, uint;
    showf, float;
    showb, boolean;
}

pub(super) fn concats(v: Value, env: &mut Environment) -> Value {
    let p = unsafe { v.pointer };
    let s1 = unsafe {env.index(p).fat_pointer };
    let s2 = unsafe {env.index(p+1).fat_pointer };
    let mut s = String::with_capacity((s1.0 + s2.0) as usize);

    for i in 0..s1.0 {
        s.push(unsafe { env.index(s1.1+i as usize).c });
    }
    for i in 0..s2.0 {
        s.push(unsafe { env.index(s2.1+i as usize).c });
    }

    env.new_string(&s)
}

pub(super) fn concata(v: Value, env: &mut Environment) -> Value {
    let p = unsafe { v.pointer };
    let a1 = unsafe {env.index(p).fat_pointer };
    let a2 = unsafe {env.index(p+1).fat_pointer };
    let mut vec = Vec::with_capacity((a1.0 + a2.0) as usize);

    for i in 0..a1.0 {
        vec.push(*env.index(a1.1+i as usize));
    }
    for i in 0..a2.0 {
        vec.push(*env.index(a2.1+i as usize));
    }

    env.new_array(&vec)
}
