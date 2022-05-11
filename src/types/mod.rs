//! Type system

use std::num::NonZeroUsize;

mod type_type;

pub use type_type::*;

pub type Unit = ();
pub type Bool = bool;
pub type Uint = u64;
pub type Int = i64;
pub type Float = f64;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pointer(pub(super) NonZeroUsize);
pub type Offset = usize;
pub const         MASK: usize = 0b1100_0000 << (64 - 8);
pub const SECTION_SIZE: usize = 0b0100_0000 << (64 - 8);
pub const    TEXT_MASK: usize = 0b0000_0000 << (64 - 8);
pub const  HEAP_OFFSET: usize = 0b0100_0000 << (64 - 8);
pub const STACK_OFFSET: usize = 0b1000_0000 << (64 - 8);
pub const   STACK_MASK: usize = 0b1100_0000 << (64 - 8);
impl Pointer {
    #[inline(always)]
    pub fn text(i: usize) -> Self {
        debug_assert_ne!(i, 0);
        Pointer(unsafe{NonZeroUsize::new_unchecked(i | TEXT_MASK)})
    }
    #[inline(always)]
    pub const fn heap(i: usize) -> Self {
        Pointer(unsafe{NonZeroUsize::new_unchecked(i | HEAP_OFFSET)})
    }
    #[inline(always)]
    pub const fn stack(i: usize) -> Self {
        Pointer(unsafe{NonZeroUsize::new_unchecked(i | STACK_OFFSET)})
    }
}
impl From<Pointer> for u64 {
    #[inline]
    fn from(p: Pointer) -> Self {
        p.0.get() as u64
    }
}
impl From<u64> for Pointer {
    #[inline]
    fn from(p: u64) -> Self {
        debug_assert_ne!(p, 0);
        Pointer(unsafe{NonZeroUsize::new_unchecked(p as usize)})
    }
}

