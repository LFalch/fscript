use std::fmt::{self, Display};

use super::super::chars::CharsError;
use super::super::tokeniser::FileLocation;

use super::{LastTokenKind, SyntaxOp};

#[derive(Debug)]
pub struct Error {
    file_loc: FileLocation,
    kind: ErrorKind,
}

impl Error {
    #[inline(always)]
    pub(crate) fn new(file_loc: FileLocation, kind: ErrorKind) -> Error {
        Error {
            file_loc,
            kind,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{} {}", self.file_loc.line, self.file_loc.column, self.kind)
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    ExpectedExpression,
    ExpectedToken,
    UnexpectedToken,
    MissingSemicolon,
    MissingEndParen,
    EmptyStatement,
    MalformedNumber,
    UnrecognisedOperator(String),
    UnexpectedOperator(LastTokenKind, Option<SyntaxOp>, Option<(&'static str, u8)>, Option<&'static str>),
    Chars(CharsError),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::ExpectedExpression => write!(f, "expected expression"),
            ErrorKind::ExpectedToken => write!(f, "expected token"),
            ErrorKind::UnexpectedToken => write!(f, "unexpected oken"),
            ErrorKind::MissingSemicolon => write!(f, "missing semicolon"),
            ErrorKind::MissingEndParen => write!(f, "missing end parenthesis"),
            ErrorKind::EmptyStatement => write!(f, "empty statement"),
            ErrorKind::MalformedNumber => write!(f, "malformed number"),
            ErrorKind::UnrecognisedOperator(op) => write!(f, "unrecognised operator {}", op),
            ErrorKind::UnexpectedOperator(ltk, _, _, _) => write!(f, "unexpected operator, last token was {:?}", ltk),
            ErrorKind::Chars(ce) => ce.fmt(f),
        }
    }
}

pub trait FlattenToResult<T, E>: Sized {
    #[inline(always)]
    fn flatten(self, file_loc: FileLocation) -> Result<T, E> {
        self.flatten_with(file_loc, ErrorKind::ExpectedToken)
    }
    fn flatten_with(self, file_loc: FileLocation, error: ErrorKind) -> Result<T, E>;
}

impl<T, E: Into<Error>> FlattenToResult<T, Error> for Option<Result<T, E>> {
    fn flatten_with(self, file_loc: FileLocation, default_error: ErrorKind) -> Result<T, Error> {
        match self {
            Some(Ok(t)) => Ok(t),
            Some(Err(e)) => Err(e.into()),
            None => Err(Error {
                file_loc,
                kind: default_error,
            }),
        }
    }
}

impl From<(FileLocation, CharsError)> for Error {
    fn from((file_loc, c): (FileLocation, CharsError)) -> Self {
        Error {
            file_loc,
            kind: ErrorKind::Chars(c),
        }
    }
}

impl From<(FileLocation, std::io::Error)> for Error {
    fn from((file_loc, e): (FileLocation, std::io::Error)) -> Self {
        Error {
            file_loc,
            kind: ErrorKind::Chars(CharsError::Other(e)),
        }
    }
}