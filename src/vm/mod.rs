//! The virtual machine

use crate::types::Type;

/// Instruction set
pub mod ins;
pub mod memory_area;
pub mod rt;
pub mod fns;

/// A runtime builder with standard functions added
pub fn std() -> rt::RuntimeBuilder {
    rt::RuntimeBuilder::new()
        .add_native("printint", Type::Int, Type::Unit, fns::print_int)
        .add_native("add", Type::Tuple(vec![Type::Float, Type::Float]), Type::Float, fns::addf)
        .add_native("sub", Type::Tuple(vec![Type::Float, Type::Float]), Type::Float, fns::subf)
        .add_native("mul", Type::Tuple(vec![Type::Float, Type::Float]), Type::Float, fns::mulf)
        .add_native("div", Type::Tuple(vec![Type::Float, Type::Float]), Type::Float, fns::divf)
        .add_native("rem", Type::Tuple(vec![Type::Float, Type::Float]), Type::Float, fns::remf)
        .add_native("add", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::addi)
        .add_native("sub", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::subi)
        .add_native("mul", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::muli)
        .add_native("div", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::divi)
        .add_native("rem", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::remi)
        .add_native("add", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::addu)
        .add_native("sub", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::subu)
        .add_native("mul", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::mulu)
        .add_native("div", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::divu)
        .add_native("rem", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::remu)
        .add_native("shl", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::shli)
        .add_native("shr", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::shri)
        .add_native("shl", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::shl)
        .add_native("shr", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::shr)
        .add_native("and", Type::Tuple(vec![Type::Bool, Type::Bool]), Type::Bool, fns::and)
        .add_native("xor", Type::Tuple(vec![Type::Bool, Type::Bool]), Type::Bool, fns::xor)
        .add_native("or", Type::Tuple(vec![Type::Bool, Type::Bool]), Type::Bool, fns::or)
        .add_native("and", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::andu)
        .add_native("xor", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::xoru)
        .add_native("or", Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint, fns::oru)
        .add_native("and", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::andi)
        .add_native("xor", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::xori)
        .add_native("or", Type::Tuple(vec![Type::Int, Type::Int]), Type::Int, fns::ori)
        .add_native("read", Type::Unit, Type::String, fns::read)
}
