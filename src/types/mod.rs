//! Type system

use std::num::NonZeroUsize;

mod type_type;

pub use type_type::*;

pub type Unit = ();
pub type Bool = bool;
pub type Uint = u64;
pub type Int = i64;
pub type Float = f64;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer(pub(super) NonZeroUsize);
pub type Offset = usize;
impl Pointer {
    #[inline(always)]
    pub fn top(i: usize) -> Self {
        debug_assert_ne!(i, 0);
        Pointer(unsafe{NonZeroUsize::new_unchecked(i)})
    }
    #[inline(always)]
    pub fn bottom(i: usize) -> Self {
        debug_assert_ne!(i, usize::MAX);
        Pointer(unsafe{NonZeroUsize::new_unchecked(usize::MAX - i)})
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
