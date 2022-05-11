//! Everything that has to do with parsing the source into an AST representation

use std::io::Read;

pub mod chars;
pub mod tokeniser;
pub mod parser;
pub mod ast;

use self::chars::CharsExt;
use self::tokeniser::Tokeniser;
use self::parser::*;

/// Parses the content of a reader into an abstract syntax tree representation of a program
pub fn parse_source<R: Read>(read: R) -> Result<ast::Program, Error> {
    let tokeniser = Tokeniser::from_char_iter(read.chars_iterator());
    let token_stream = TokenStream::new(tokeniser);

    tree(token_stream)
}