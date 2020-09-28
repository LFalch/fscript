use std::{
    // collections::HashMap,
    mem::size_of,
    str::FromStr,
    fmt::{self, Display},
};

use super::types as t;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    Uint,
    Int,
    Float,
    Array(Box<Type>, usize),
    Tuple(Vec<Type>),
    // Struct(HashMap<String, Type>),
    Option(Box<Type>),
    Reference(Box<Type>),
    Function(Vec<Type>, Box<Type>),
}

impl Type {
    #[inline]
    pub fn size(&self) -> usize {
        use Type::*;
        match *self {
            Unit => size_of::<t::Unit>(),
            Bool => size_of::<t::Bool>(),
            Uint => size_of::<t::Uint>(),
            Int => size_of::<t::Int>(),
            Float => size_of::<t::Float>(),
            Array(ref t, n) => t.size() * n,
            Tuple(ref ts) => ts.iter().map(Type::size).sum(),
            // Pointer optimisation
            Option(ref t) if t.is_reference() => t.size(),
            Option(ref t) => size_of::<t::Bool>() + t.size(),
            Reference(_) => size_of::<t::Pointer>(),
            Function(_, _) => size_of::<t::Pointer>(),
        }
    }
    #[inline(always)]
    pub fn is_reference(&self) -> bool {
        match *self {
            Type::Reference(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeParserError {
    ArrayNoEndBracket,
    TupleNoEndBracket,
    MissingSemicolonInArray,
    InvalidFunctionArgs,
    UnrecognisedType,
    UnparseableSize(<usize as FromStr>::Err)
}

impl FromStr for Type {
    type Err = TypeParserError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "()" => Type::Unit,
            "int" => Type::Int,
            "uint" => Type::Uint,
            "bool" => Type::Bool,
            "float" => Type::Float,
            _ if s.starts_with('?') => Type::Option(Box::new(Self::from_str(&s[1..])?)),
            _ if s.starts_with('&') => Type::Reference(Box::new(Self::from_str(&s[1..])?)),
            _ if s.starts_with('[') => {
                if !s.ends_with(']') {
                    return Err(TypeParserError::ArrayNoEndBracket)
                }
                let sep = s.find(';').ok_or(TypeParserError::MissingSemicolonInArray)?;
                Type::Array(Box::new(Self::from_str(&s[1..sep])?), s[sep+1..s.len()-1].parse().map_err(|e| TypeParserError::UnparseableSize(e))?)
            }
            _ if s.starts_with('(') => {
                if !s.ends_with(')') {
                    return Err(TypeParserError::TupleNoEndBracket)
                }
                let s = &s[1..s.len()-1];

                if s == "," {
                    return Ok(Type::Tuple(vec![]))
                }

                let mut split = s.split(',');
                let mut ts = Vec::with_capacity(split.size_hint().0);

                let back = split.next_back().and_then(|b| if b.is_empty() {
                    None
                } else { Some(b) });

                for t in split.chain(back) {
                    ts.push(Self::from_str(t)?);
                }
                Type::Tuple(ts)
            }
            _ if s.starts_with("fn") => {
                let s = &s[2..];
                let (ret, end_i) =  if let Some(ret_i) = s.find("->") {
                    (Self::from_str(&s[ret_i+2..])?, ret_i)
                } else {
                    (Type::Unit, s.len())
                };
                match Self::from_str(&s[..end_i])? {
                    Type::Unit => Type::Function(vec![], Box::new(ret)),
                    Type::Tuple(ts) => Type::Function(ts, Box::new(ret)),
                    _ => return Err(TypeParserError::InvalidFunctionArgs),
                }
            }
            _ => return Err(TypeParserError::UnrecognisedType),
        })
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;

        struct TupleType<'a>(&'a [Type]);

        impl Display for TupleType<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "(")?;

                {
                    let mut iter = self.0.iter();
                    let last = iter.next_back();
                    for t in iter {
                        write!(f, "{},", t)?;
                    }
                    if let Some(t) = last {
                        write!(f, "{}", t)?;
                    }
                }

                if self.0.len() < 2 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
        }

        match *self {
            Unit => "()".fmt(f),
            Bool => "bool".fmt(f),
            Uint => "uint".fmt(f),
            Int => "int".fmt(f),
            Float => "float".fmt(f),
            Array(ref t, n) => write!(f, "[{},{}]", t, n),
            Tuple(ref ts) => write!(f, "{}", TupleType(ts)),
            Option(ref t) => write!(f, "?{}", t),
            Reference(ref t) => write!(f, "&{}", t),
            Function(ref args, ref ret) => {
                write!(f, "fn{}->{}", TupleType(args), ret)
            }
        }
    }
}