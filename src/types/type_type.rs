//! Type system

use std::{
    // collections::HashMap,
    mem::size_of,
    str::FromStr,
    fmt::{self, Display},
};
use byteorder::{ByteOrder, LittleEndian};

use crate::types as t;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    Uint,
    Int,
    Float,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    // Struct(HashMap<String, Type>),
    Option(Box<Type>),
    Reference(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    String,
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
            Tuple(ref ts) => ts.iter().map(Type::size).sum(),
            // Pointer optimisation
            Option(ref t) if t.is_reference() => t.size(),
            Option(ref t) => size_of::<t::Bool>() + t.size(),
            Reference(_) => size_of::<t::Pointer>(),
            Function(_, _) => size_of::<t::Pointer>(),
            Array(_) => size_of::<t::Pointer>(),
            String => size_of::<t::Pointer>(),
        }
    }
    #[inline(always)]
    pub fn is_reference(&self) -> bool {
        matches!(*self, Type::Reference(_) | Type::Function(_, _) | Type::Array(_) | Type::String)
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
                Type::Array(Box::new(Self::from_str(&s[1..s.len()-1])?))
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
            Array(ref t) => write!(f, "[{}]", t),
            Tuple(ref ts) => write!(f, "{}", TupleType(ts)),
            Option(ref t) => write!(f, "?{}", t),
            Reference(ref t) => write!(f, "&{}", t),
            Function(ref args, ref ret) => {
                write!(f, "fn{}->{}", TupleType(args), ret)
            }
            String => "str".fmt(f),
        }
    }
}

pub trait FType {
    fn get_type() -> Type;
    #[inline(always)]
    fn size() -> usize {
        Self::get_type().size()
    }
    fn as_bytes(&self) -> Vec<u8>;
    fn from_bytes(buf: &[u8]) -> Self;
}

impl FType for t::Unit {
    #[inline(always)]
    fn get_type() -> Type {
        Type::Unit
    }
    #[inline]
    fn as_bytes(&self) -> Vec<u8> {
        Vec::new()
    }
    fn from_bytes(_buf: &[u8]) -> Self {
        ()
    }
}

impl FType for t::Bool {
    #[inline(always)]
    fn get_type() -> Type {
        Type::Bool
    }
    #[inline]
    fn as_bytes(&self) -> Vec<u8> {
        vec![*self as u8]
    }
    fn from_bytes(buf: &[u8]) -> Self {
        buf[0] != 0
    }
}

impl FType for t::Uint {
    #[inline(always)]
    fn get_type() -> Type {
        Type::Uint
    }
    #[inline]
    fn as_bytes(&self) -> Vec<u8> {
        let mut v = vec![0; size_of::<t::Uint>()];
        LittleEndian::write_u64(&mut v, *self);
        v
    }
    #[inline]
    fn from_bytes(buf: &[u8]) -> Self {
        LittleEndian::read_u64(buf)
    }
}

impl FType for t::Int {
    #[inline(always)]
    fn get_type() -> Type {
        Type::Int
    }
    #[inline]
    fn as_bytes(&self) -> Vec<u8> {
        let mut v = vec![0; size_of::<t::Int>()];
        LittleEndian::write_i64(&mut v, *self);
        v
    }
    #[inline]
    fn from_bytes(buf: &[u8]) -> Self {
        LittleEndian::read_i64(buf)
    }
}

impl FType for t::Float {
    #[inline(always)]
    fn get_type() -> Type {
        Type::Float
    }
    #[inline]
    fn as_bytes(&self) -> Vec<u8> {
        let mut v = vec![0; size_of::<t::Float>()];
        LittleEndian::write_f64(&mut v, *self);
        v
    }
    #[inline]
    fn from_bytes(buf: &[u8]) -> Self {
        LittleEndian::read_f64(buf)
    }
}

impl<T: FType> FType for Vec<T> {
    #[inline]
    fn get_type() -> Type {
        Type::Array(Box::new(T::get_type()))
    }
    #[inline]
    fn as_bytes(&self) -> Vec<u8> {
        panic!("arrays are points")
    }
    #[inline]
    fn from_bytes(_buf: &[u8]) -> Self {
        panic!("arrays are points")
    }
}

macro_rules! impl_for_tuple {
    ($($t1:ident, $($tn:ident),*;)+) => {
        $(
        impl<$t1: FType $(, $tn: FType)*> FType for ($t1, $($tn),*) {
            #[inline]
            fn get_type() -> Type {
                Type::Tuple(vec![$t1::get_type() $(, $tn::get_type())*])
            }
            fn as_bytes(&self) -> Vec<u8> {
                #[allow(non_snake_case)]
                let ($t1, $($tn),*) = self;
                $t1.as_bytes().into_iter()
                    $( .chain($tn.as_bytes().into_iter()) )*
                    .collect()
            }
            #[inline]
            fn from_bytes(_buf: &[u8]) -> Self {
                todo!()
            }
        }
        )*
    };
}

impl_for_tuple!{
    T1,;
    T1, T2;
    T1, T2, T3;
    T1, T2, T3, T4;
    T1, T2, T3, T4, T5;
    T1, T2, T3, T4, T5, T6;
    T1, T2, T3, T4, T5, T6, T7;
}

impl FType for t::Pointer {
    /// Panicks
    fn get_type() -> Type {
        panic!("raw pointer has no runtime type");
    }
    fn size() -> usize { 8 }
    fn as_bytes(&self) -> Vec<u8> {
        let p = t::Uint::from(*self);
        p.as_bytes()
    }
    #[inline]
    fn from_bytes(buf: &[u8]) -> Self {
        let p = LittleEndian::read_u64(&buf);
        p.into()
    }
}

impl<T: FType> FType for Option<T> {
    fn get_type() -> Type {
        Type::Option(Box::new(T::get_type()))
    }
    fn as_bytes(&self) -> Vec<u8> {
        if T::get_type().is_reference() {
            match self {
                None => vec![false as u8; T::get_type().size()],
                Some(i) => i.as_bytes(),
            }
        } else {
            match self {
                None => vec![false as u8; size_of::<t::Bool>() + T::get_type().size()],
                Some(i) => {
                    let mut v = vec![true as u8];
                    v.append(&mut i.as_bytes());
                    v
                }
            }
        }
    }
    #[inline]
    fn from_bytes(buf: &[u8]) -> Self {
        if T::get_type().is_reference() {
            if buf.iter().all(|v| *v == 0)  {
                None
            } else {
                Some(T::from_bytes(buf))
            }
        } else {
            if buf[0] == 0 {
                None
            } else {
                Some(T::from_bytes(&buf[1..]))
            }
        }
    }
}
