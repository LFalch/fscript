use std::fs::File;

use fscript::{
    source::parse_source,
    type_check::type_check,
    tinterpreter::{run, Return},
    types::Type,
};

fn main() {
    let functions = [
        ("add".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Int)),
        ("add".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("add".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Float)),
        ("sub".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Int)),
        ("sub".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("sub".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Float)),
        ("mul".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Int)),
        ("mul".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("mul".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Float)),
        ("div".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Int)),
        ("div".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("div".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Float)),
        ("rem".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Int)),
        ("rem".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("rem".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Float)),
        ("shl".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("shr".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("bitand".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("bitand".to_owned(), (Type::Tuple(vec![Type::Bool, Type::Bool]), Type::Bool)),
        ("bitor".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("bitor".to_owned(), (Type::Tuple(vec![Type::Bool, Type::Bool]), Type::Bool)),
        ("bitxor".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Uint)),
        ("bitxor".to_owned(), (Type::Tuple(vec![Type::Bool, Type::Bool]), Type::Bool)),
        ("eq".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Bool)),
        ("eq".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Bool)),
        ("eq".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Bool)),
        ("lt".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Bool)),
        ("lt".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Bool)),
        ("lt".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Bool)),
        ("lte".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Bool)),
        ("lte".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Bool)),
        ("lte".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Bool)),
        ("gt".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Bool)),
        ("gt".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Bool)),
        ("gt".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Bool)),
        ("gte".to_owned(), (Type::Tuple(vec![Type::Int, Type::Int]), Type::Bool)),
        ("gte".to_owned(), (Type::Tuple(vec![Type::Uint, Type::Uint]), Type::Bool)),
        ("gte".to_owned(), (Type::Tuple(vec![Type::Float, Type::Float]), Type::Bool)),
        ("not".to_owned(), (Type::Bool, Type::Bool)),
        ("not".to_owned(), (Type::Uint, Type::Uint)),
        ("neg".to_owned(), (Type::Int, Type::Int)),
        ("neg".to_owned(), (Type::Float, Type::Float)),
        ("concat".to_owned(), (Type::Tuple(vec![Type::String, Type::String]), Type::String)),
        ("concat".to_owned(), (Type::Array(Box::new(Type::Int)), Type::Tuple(vec![Type::Array(Box::new(Type::Int)), Type::Array(Box::new(Type::Int))]))),
        ("concat".to_owned(), (Type::Array(Box::new(Type::Uint)), Type::Tuple(vec![Type::Array(Box::new(Type::Uint)), Type::Array(Box::new(Type::Uint))]))),
        ("show".to_owned(), (Type::Reference(Box::new(Type::Int)), Type::String)),
        ("show".to_owned(), (Type::Reference(Box::new(Type::Uint)), Type::String)),
        ("show".to_owned(), (Type::Reference(Box::new(Type::Float)), Type::String)),
        ("show".to_owned(), (Type::Reference(Box::new(Type::Bool)), Type::String)),
        ("print".to_owned(), (Type::String, Type::Unit)),
        ("println".to_owned(), (Type::String, Type::Unit)),
        ("read".to_owned(), (Type::Unit, Type::String)),
        ("int".to_owned(), (Type::String, Type::Int)),
        ("vardump".to_owned(), (Type::Unit, Type::Unit)),
    ];

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
                            Ok((_rt, stmnts)) => {
                                let Return { value: _, t: _, display } = run(stmnts).unwrap();
                                println!("\nReturned: {display}");
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
