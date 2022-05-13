//! The virtual machine

use crate::types::Type;

/// Instruction set
pub mod ins;
pub mod memory_area;
pub mod rt;
pub mod fns;

pub fn runtime_builder() -> rt::RuntimeBuilder {
    rt::RuntimeBuilder::new()
        .add_native("printint", (vec![Type::Int], Type::Unit), fns::print_int)
        .add_native("add", (vec![Type::Float, Type::Float], Type::Float), fns::addf)
        .add_native("sub", (vec![Type::Float, Type::Float], Type::Float), fns::subf)
        .add_native("mul", (vec![Type::Float, Type::Float], Type::Float), fns::mulf)
        .add_native("div", (vec![Type::Float, Type::Float], Type::Float), fns::divf)
        .add_native("rem", (vec![Type::Float, Type::Float], Type::Float), fns::remf)
        .add_native("add", (vec![Type::Int, Type::Int], Type::Int), fns::addi)
        .add_native("sub", (vec![Type::Int, Type::Int], Type::Int), fns::subi)
        .add_native("mul", (vec![Type::Int, Type::Int], Type::Int), fns::muli)
        .add_native("div", (vec![Type::Int, Type::Int], Type::Int), fns::divi)
        .add_native("rem", (vec![Type::Int, Type::Int], Type::Int), fns::remi)
        .add_native("add", (vec![Type::Uint, Type::Uint], Type::Uint), fns::addu)
        .add_native("sub", (vec![Type::Uint, Type::Uint], Type::Uint), fns::subu)
        .add_native("mul", (vec![Type::Uint, Type::Uint], Type::Uint), fns::mulu)
        .add_native("div", (vec![Type::Uint, Type::Uint], Type::Uint), fns::divu)
        .add_native("rem", (vec![Type::Uint, Type::Uint], Type::Uint), fns::remu)
        .add_native("shl", (vec![Type::Int, Type::Int], Type::Int), fns::shli)
        .add_native("shr", (vec![Type::Int, Type::Int], Type::Int), fns::shri)
        .add_native("shl", (vec![Type::Uint, Type::Uint], Type::Uint), fns::shl)
        .add_native("shr", (vec![Type::Uint, Type::Uint], Type::Uint), fns::shr)
        .add_native("and", (vec![Type::Bool, Type::Bool], Type::Bool), fns::and)
        .add_native("xor", (vec![Type::Bool, Type::Bool], Type::Bool), fns::xor)
        .add_native("or", (vec![Type::Bool, Type::Bool], Type::Bool), fns::or)
        .add_native("and", (vec![Type::Uint, Type::Uint], Type::Uint), fns::andu)
        .add_native("xor", (vec![Type::Uint, Type::Uint], Type::Uint), fns::xoru)
        .add_native("or", (vec![Type::Uint, Type::Uint], Type::Uint), fns::oru)
        .add_native("and", (vec![Type::Int, Type::Int], Type::Int), fns::andi)
        .add_native("xor", (vec![Type::Int, Type::Int], Type::Int), fns::xori)
        .add_native("or", (vec![Type::Int, Type::Int], Type::Int), fns::ori)
        .add_native("read", (vec![], Type::String), fns::read)
}
