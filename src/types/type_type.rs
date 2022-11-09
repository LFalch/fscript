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
pub struct NoTypeVariable;

impl Display for NoTypeVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type<V=NoTypeVariable> {
    TypeVariable(V),
    IntegralVariable(V),
    Unit,
    Bool,
    Uint,
    Int,
    Float,
    Array(Box<Type<V>>),
    Tuple(Vec<Type<V>>),
    // Struct(HashMap<String, Type>),
    Option(Box<Type<V>>),
    Reference(Box<Type<V>>),
    MutReference(Box<Type<V>>),
    /// (args, return type)
    Function(Box<Type<V>>, Box<Type<V>>),
    String,
}

impl<V> Type<V> {
    #[inline]
    pub fn size(&self) -> usize {
        use Type::*;
        match *self {
            TypeVariable(_) | IntegralVariable(_) => panic!("not concrete"),
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
            MutReference(_) => size_of::<t::Pointer>(),
            Function(_, _) => size_of::<t::Pointer>(),
            Array(_) => size_of::<t::Pointer>(),
            String => size_of::<t::Pointer>(),
        }
    }
    #[inline(always)]
    pub fn is_reference(&self) -> bool {
        matches!(*self, Type::Reference(_) | Type::Function(_, _) | Type::Array(_) | Type::String)
    }
    pub fn into<V2, F: FnMut(V) -> V2>(self, f: &mut F) -> Type<V2> {
        match self {
            Type::TypeVariable(v) => Type::TypeVariable(f(v)),
            Type::IntegralVariable(v) => Type::IntegralVariable(f(v)),
            Type::Unit => Type::Unit,
            Type::Bool => Type::Bool,
            Type::Uint => Type::Uint,
            Type::Int => Type::Int,
            Type::Float => Type::Float,
            Type::Array(t) => Type::Array(Box::new((*t).into(f))),
            Type::Tuple(ts) => Type::Tuple(ts.into_iter().map(|t| t.into(f)).collect()),
            Type::Option(t) => Type::Option(Box::new((*t).into(f))),
            Type::Reference(t) => Type::Reference(Box::new((*t).into(f))),
            Type::MutReference(t) => Type::MutReference(Box::new((*t).into(f))),
            Type::Function(a, r) => Type::Function(Box::new((*a).into(f)), Box::new((*r).into(f))),
            Type::String => Type::String,
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

impl<V: Display> Display for Type<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;

        struct TupleType<'a, V>(&'a [Type<V>]);

        impl<V: Display> Display for TupleType<'_, V> {
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
            MutReference(ref t) => write!(f, "@{}", t),
            Function(ref args, ref ret) => {
                match &**args {
                    Tuple(_) | Unit => write!(f, "fn{}->{}", args, ret),
                    _ => write!(f, "fn({})->{}", args, ret),
                }
            }
            String => "str".fmt(f),
            TypeVariable(ref c) => write!(f, "'{c}"),
            IntegralVariable(ref c) => write!(f, "int('{c})"),
        }
    }
}

pub trait FType<V=NoTypeVariable> {
    fn get_type() -> Type<V>;
    #[inline(always)]
    fn size() -> usize {
        Self::get_type().size()
    }
    fn as_bytes(&self) -> Vec<u8>;
    fn from_bytes(buf: &[u8]) -> Self;
}

impl<V> FType<V> for t::Unit {
    #[inline(always)]
    fn get_type() -> Type<V> {
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

impl<V> FType<V> for t::Bool {
    #[inline(always)]
    fn get_type() -> Type<V> {
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

impl<V> FType<V> for t::Uint {
    #[inline(always)]
    fn get_type() -> Type<V> {
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

impl<V> FType<V> for t::Int {
    #[inline(always)]
    fn get_type() -> Type<V> {
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

impl<V> FType<V> for t::Float {
    #[inline(always)]
    fn get_type() -> Type<V> {
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

impl<V, T: FType<V>> FType<V> for Vec<T> {
    #[inline]
    fn get_type() -> Type<V> {
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
        impl<V, $t1: FType<V> $(, $tn: FType<V>)*> FType<V> for ($t1, $($tn),*) {
            #[inline]
            fn get_type() -> Type<V> {
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

impl<V> FType<V> for t::Pointer {
    /// Panicks
    fn get_type() -> Type<V> {
        panic!("raw pointer has no runtime type");
    }
    fn size() -> usize { 8 }
    fn as_bytes(&self) -> Vec<u8> {
        let p = t::Uint::from(*self);
        <u64 as FType>::as_bytes(&p)
    }
    #[inline]
    fn from_bytes(buf: &[u8]) -> Self {
        let p = LittleEndian::read_u64(&buf);
        p.into()
    }
}

impl<V, T: FType<V>> FType<V> for Option<T> {
    fn get_type() -> Type<V> {
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
