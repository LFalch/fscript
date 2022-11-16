use std::fs::File;

use fscript::{
    source::parse_source,
    type_check::type_check,
    types::Type,
};

fn main() {
    use self::Type::*;

    let functions = [
        ("add".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("add".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("add".to_owned(), (Tuple(vec![Float, Float]), Float)),
        ("sub".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("sub".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("sub".to_owned(), (Tuple(vec![Float, Float]), Float)),
        ("mul".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("mul".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("mul".to_owned(), (Tuple(vec![Float, Float]), Float)),
        ("div".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("div".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("div".to_owned(), (Tuple(vec![Float, Float]), Float)),
        ("rem".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("rem".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("rem".to_owned(), (Tuple(vec![Float, Float]), Float)),
        ("shl".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("shl".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("shr".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("shr".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("and".to_owned(), (Tuple(vec![Bool, Bool]), Bool)),
        ("and".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("and".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("xor".to_owned(), (Tuple(vec![Bool, Bool]), Bool)),
        ("xor".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("xor".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("or".to_owned(), (Tuple(vec![Bool, Bool]), Bool)),
        ("or".to_owned(), (Tuple(vec![Uint, Uint]), Uint)),
        ("or".to_owned(), (Tuple(vec![Int, Int]), Int)),
        ("eq".to_owned(), (Tuple(vec![Int, Int]), Bool)),
        ("eq".to_owned(), (Tuple(vec![Uint, Uint]), Bool)),
        ("eq".to_owned(), (Tuple(vec![Float, Float]), Bool)),
        ("eq".to_owned(), (Tuple(vec![String, String]), Bool)),
        ("concat".to_owned(), (Tuple(vec![String, String]), String)),
        // Wait for proper type variables
        ("concat".to_owned(), (Tuple(vec![Array(Box::new(Int)), Array(Box::new(Int))]), String)),
        ("concat".to_owned(), (Tuple(vec![Array(Box::new(Uint)), Array(Box::new(Uint))]), String)),
        ("concat".to_owned(), (Tuple(vec![Array(Box::new(Float)), Array(Box::new(Float))]), String)),
        ("concat".to_owned(), (Tuple(vec![Array(Box::new(Bool)), Array(Box::new(Bool))]), String)),
        ("read".to_owned(), (Unit, String)),
        ("show".to_owned(), (Reference(Box::new(Int)), String)),
        ("show".to_owned(), (Reference(Box::new(Uint)), String)),
        ("show".to_owned(), (Reference(Box::new(Bool)), String)),
        ("show".to_owned(), (Reference(Box::new(Float)), String)),
        ("print".to_owned(), (String, Unit)),
        ("println".to_owned(), (String, Unit)),
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
                            Ok((rt, stmnts)) => {
                                println!("Return type: {}", rt);
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
