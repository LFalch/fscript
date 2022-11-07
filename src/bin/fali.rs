use std::fs::File;

use fscript::{
    source::parse_source,
    interpreter::run,
};

fn main() {
    let mut show_tree = false;
    let mut show_return = true;

    for arg in std::env::args().skip(1) {
        match &*arg {
            "-t" => show_tree = true,
            "-T" => show_tree = false,
            "-r" => show_return = true,
            "-R" => show_return = false,
            f => {
                match parse_source(File::open(f).unwrap()) {
                    Err(e) => eprintln!("Parser error {f}:{e:?}"),
                    Ok(code) => {
                        println!("{f}:");

                        if show_tree {
                            println!("AST Program:");
                            code.iter().for_each(|statement| println!("{:?}", statement));
                        }
                        match run(code) {
                            Ok(val) => {
                                if show_return {
                                    println!("{:?}", val);
                                }
                            }
                            Err(e) => eprintln!("Runtime error {f}:{e}"),
                        }
                        println!();
                    }
                }
            }
        }
    }
}
