use crate::chars::CharsError;

use super::{LastTokenKind, SyntaxOp};

#[derive(Debug)]
pub enum CompileError {
    ExpectedToken,
    UnexpectedToken,
    MissingSemicolon,
    EmptyStatement,
    MalformedNumber,
    UnrecognisedOperator(String),
    UnexpectedOperator(LastTokenKind, Option<SyntaxOp>, Option<(&'static str, u8)>, Option<&'static str>),
    Chars(CharsError),
}

pub trait FlattenToResult<T, E> {
    fn flatten(self) -> Result<T, E>;
}

impl<T, E: Into<CompileError>> FlattenToResult<T, CompileError> for Option<Result<T, E>> {
    fn flatten(self) -> Result<T, CompileError> {
        match self {
            Some(Ok(t)) => Ok(t),
            Some(Err(e)) => Err(e.into()),
            None => Err(CompileError::ExpectedToken),
        }
    }
}

impl From<CharsError> for CompileError {
    fn from(c: CharsError) -> Self {
        CompileError::Chars(c)
    }
}

impl From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> Self {
        CompileError::Chars(CharsError::Other(e))
    }
}