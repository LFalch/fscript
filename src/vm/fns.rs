use std::ops::{Add, Sub, Mul, Div, Rem, Shl, Shr, BitAnd, BitOr, BitXor};

use crate::types::{Bool, Float, Int, Uint};
use super::rt::{Runtime, Error};

pub fn id(_rt: &mut Runtime) -> Result<(), Error> { Ok(()) }

macro_rules! args {
    ($rt:expr; $var:ident : $t:ty $(, $args:ident : $ts:ty)*) => {
        args!($rt; $($args : $ts),*);
        let $var: $t = $rt.pop_stack()?;
    };
    ($rt:expr;) => {};
}

pub fn print_int(rt: &mut Runtime) -> Result<(), Error> {
    args!(rt; i: Int);
    println!("{i}");

    Ok(())
}

macro_rules! bin_op {
    ($name:ident, $t:ty, $op:expr) => {
        pub fn $name(rt: &mut Runtime) -> Result<(), Error> {
            args!(rt; lhs: $t, rhs: $t);
            let result = $op(lhs, rhs);
            rt.push_stack::<$t>(result);
            Ok(())
        }
    };
}
bin_op!(addf, Float, Add::add);
bin_op!(subf, Float, Sub::sub);
bin_op!(mulf, Float, Mul::mul);
bin_op!(divf, Float, Div::div);
bin_op!(remf, Float, Rem::rem);
bin_op!(addi, Int, Add::add);
bin_op!(subi, Int, Sub::sub);
bin_op!(muli, Int, Mul::mul);
bin_op!(divi, Int, Div::div);
bin_op!(remi, Int, Rem::rem);
bin_op!(addu, Uint, Add::add);
bin_op!(subu, Uint, Sub::sub);
bin_op!(mulu, Uint, Mul::mul);
bin_op!(divu, Uint, Div::div);
bin_op!(remu, Uint, Rem::rem);
bin_op!(shli, Int, Shl::shl);
bin_op!(shri, Int, Shr::shr);
bin_op!(shl, Uint, Shl::shl);
bin_op!(shr, Uint, Shr::shr);
bin_op!(and, Bool, BitAnd::bitand);
bin_op!(xor, Bool, BitXor::bitxor);
bin_op!(or, Bool, BitOr::bitor);
bin_op!(andu, Uint, BitAnd::bitand);
bin_op!(xoru, Uint, BitXor::bitxor);
bin_op!(oru, Uint, BitOr::bitor);
bin_op!(andi, Int, BitAnd::bitand);
bin_op!(xori, Int, BitXor::bitxor);
bin_op!(ori, Int, BitOr::bitor);

use std::io::stdin;

pub fn read(rt: &mut Runtime) -> Result<(), Error> {
    let mut s = String::new();
    stdin().read_line(&mut s).map_err(|e| Error::IoError(e))?;
    let p = rt.dyn_alloc(s.len());
    rt.push_stack(p.0.get() as u64);
    Ok(())
}
