use std::io::{Write, stdout, stdin};

use fscript::Type;

fn main() {
    let mut s = String::with_capacity(32);
    println!("{:064b}", fscript::types::STACK_OFFSET);
    loop {
        print!("Give me a type: ");
        stdout().flush().unwrap();
        stdin().read_line(&mut s).unwrap();

        if s.trim().is_empty() {
            break
        }

        let typ: String = s
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        match typ.parse::<Type>() {
            Ok(typ) => println!("sizeof( {} ) = {}", typ, typ.size()),
            Err(e) => println!("Error: {:?}", e),
        }
        s.clear();
    }
}
