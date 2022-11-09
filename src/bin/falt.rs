use std::{fs::File, collections::HashMap};

use fscript::{
    source::parse_source,
    type_check::{type_check, ast::Type},
};

fn main() {
    let mut functions = HashMap::new();

    functions.insert("printint".to_owned(), (vec![Type::Int], Type::Unit));
    functions.insert("add".to_owned(), (vec![Type::Float, Type::Float], Type::Float));
    functions.insert("sub".to_owned(), (vec![Type::Float, Type::Float], Type::Float));
    functions.insert("mul".to_owned(), (vec![Type::Float, Type::Float], Type::Float));
    functions.insert("div".to_owned(), (vec![Type::Float, Type::Float], Type::Float));
    functions.insert("rem".to_owned(), (vec![Type::Float, Type::Float], Type::Float));
    functions.insert("add".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("sub".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("mul".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("div".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("rem".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("add".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("sub".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("mul".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("div".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("rem".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("shl".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("shr".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("shl".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("shr".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("and".to_owned(), (vec![Type::Bool, Type::Bool], Type::Bool));
    functions.insert("xor".to_owned(), (vec![Type::Bool, Type::Bool], Type::Bool));
    functions.insert("or".to_owned(), (vec![Type::Bool, Type::Bool], Type::Bool));
    functions.insert("and".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("xor".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("or".to_owned(), (vec![Type::Uint, Type::Uint], Type::Uint));
    functions.insert("and".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("xor".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("or".to_owned(), (vec![Type::Int, Type::Int], Type::Int));
    functions.insert("concat".to_owned(), (vec![Type::String, Type::String], Type::String));
    functions.insert("read".to_owned(), (vec![], Type::String));
    functions.insert("show".to_owned(), (vec![Type::TypeVariable(0)], Type::String));
    functions.insert("print".to_owned(), (vec![Type::String], Type::Unit));
    functions.insert("println".to_owned(), (vec![Type::String], Type::Unit));

    for arg in std::env::args().skip(1) {
        match &*arg {
            f => {
                match parse_source(File::open(f).unwrap()) {
                    Err(err) => {
                        eprintln!("{f}:");
                        eprintln!("Parse errors:");
                        eprintln!("{err}\n");
                    }
                    Ok(code) => {
                        println!("{f}:");

                        match type_check(code, functions.clone().into_iter()) {
                            Ok((rt, stmnts)) => {
                                println!("Return type: {:?}", rt);
                                println!("Typed code:");
                                for stmnt in stmnts {
                                    println!("{stmnt}");
                                }
                            }
                            Err(e) => eprintln!("Type error {f}{e}"),
                        }
                        println!();
                    }
                }
            }
        }
    }
}
