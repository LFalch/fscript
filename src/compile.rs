use std::{
    collections::HashMap,
    io::Read,
};

use crate::{
    Type,
    tokeniser::{Tokeniser, Class},
    chars::{CharsExt, CharsError},
};

type OpFuncTable = HashMap<&'static str, &'static str>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum SyntaxOp {
    Equal,
    Ref,
    Deref,
    Member,
    End,
    StartParen,
    EndParen,
    StartIndex,
    EndIndex,
    StartBlock,
    EndBlock,
}

impl SyntaxOp {
    #[inline]
    fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "=" => SyntaxOp::Equal,
            "&" => SyntaxOp::Ref,
            "*" => SyntaxOp::Deref,
            "." => SyntaxOp::Member,
            ";" => SyntaxOp::End,
            "(" => SyntaxOp::StartParen,
            ")" => SyntaxOp::EndParen,
            "[" => SyntaxOp::StartIndex,
            "]" => SyntaxOp::EndIndex,
            "{" => SyntaxOp::StartBlock,
            "}" => SyntaxOp::EndBlock,
            _ => return None,
        })
    }
}

struct OpTables {
    binary_ops: OpFuncTable,
    unary_ops: OpFuncTable,
}

fn op_func_table() -> OpTables {
    let mut binary_ops = HashMap::new();
    let mut unary_ops = HashMap::new();
    binary_ops.insert("+", "add");
    binary_ops.insert("-", "sub");
    binary_ops.insert("*", "mul");
    binary_ops.insert("/", "div");
    binary_ops.insert("%", "rem");
    binary_ops.insert("++", "concat");
    binary_ops.insert("**", "pow");
    binary_ops.insert("==", "eq");
    binary_ops.insert("!=", "neq");
    binary_ops.insert(">", "gt");
    binary_ops.insert(">=", "gte");
    binary_ops.insert("<", "lt");
    binary_ops.insert("<=", "lte");
    binary_ops.insert("&&", "and");
    binary_ops.insert("||", "or");
    binary_ops.insert("&", "bitand");
    binary_ops.insert("|", "bitor");
    binary_ops.insert("^", "xor");

    unary_ops.insert("!", "not");
    unary_ops.insert("-", "neg");
    
    OpTables {
        binary_ops,
        unary_ops,
    }
}

pub fn compile<R: Read>(read: R) -> Result<(), CharsError> {
    let op_tables = op_func_table();

    for token in Tokeniser::from_char_iter(read.chars_iterator(), |s| SyntaxOp::from_str(s).is_some() || op_tables.binary_ops.contains_key(s) || op_tables.unary_ops.contains_key(s)) {
        let (s, class) = token?;

        if !class.should_ignore() {
            match class {
                Class::Identifier => print!("$"),
                Class::Number => print!("#"),
                Class::Operator if matches!(&*s, ";") => {
                    println!(";");
                    continue
                }
                Class::UnrecognisedOperator => print!("👎"),
                Class::String => {
                    print!("{:?} ", s);
                    continue
                }
                _ => (),
            }
            print!("{} ", s);
        }
    }

    Ok(())
}