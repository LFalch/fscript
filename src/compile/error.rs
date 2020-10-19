use crate::chars::CharsError;
use crate::tokeniser::FileLocation;

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

#[derive(Debug)]
pub enum ErrorKind {
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

impl<T, E: Into<Error>> FlattenToResult<T, Error> for Option<Result<T, E>> {
    fn flatten(self) -> Result<T, Error> {
        match self {
            Some(Ok(t)) => Ok(t),
            Some(Err(e)) => Err(e.into()),
            None => Err(Error {
                file_loc: FileLocation::default(),
                kind: ErrorKind::ExpectedToken
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