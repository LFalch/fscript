//! Everything that has to do with parsing the source into an AST representation

use std::io::Read;

use std::io::read_to_string;

mod parser;
pub mod ast;

use lrlex::DefaultLexeme;
use lrpar::{Lexeme, NonStreamingLexer, Span};

use self::parser::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FileLocation {
    pub line: u16,
    pub col: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FileSpan {
    pub start: FileLocation,
    pub end: FileLocation,
}

impl FileSpan {
    pub const fn dud() -> Self {
        FileSpan {
            start: FileLocation {
                line: 0,
                col: 0,
            },
            end: FileLocation {
                line: 0,
                col: 0,
            },
        }
    }
    pub fn new(lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>, span: Span) -> Self {
        let ((sl, sc), (el, ec)) = lexer.line_col(span);
        FileSpan {
            start: FileLocation {
                line: sl as u16,
                col: sc as u16,
            },
            end: FileLocation {
                line: el as u16,
                col: ec as u16,
            },
        }
    }
    pub fn from_lexeme(lexer: &dyn NonStreamingLexer<DefaultLexeme, u32>, lexeme: Result<DefaultLexeme, DefaultLexeme>) -> Self {
        let ((sl, sc), (el, ec)) = lexer.line_col(lexeme.unwrap_or_else(|e| e).span());
        FileSpan {
            start: FileLocation {
                line: sl as u16,
                col: sc as u16,
            },
            end: FileLocation {
                line: el as u16,
                col: ec as u16,
            },
        }
    }
    pub fn ended_by(self, end: FileSpan) -> Self {
        FileSpan {
            end: end.end,
            ..self
        }
    }
}

/// Parses the content of a reader into an abstract syntax tree representation of a program
pub fn parse_source<R: Read>(read: R) -> Result<ast::Statements, ParseError> {
    let s = read_to_string(read).unwrap();

    parse(&s)
}
