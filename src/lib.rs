use std::num::NonZeroUsize;
use std::io::{Read, Error as IoError};

#[cfg(not(target_pointer_width = "64"))]
compile_error!("Does not support anything other than 64-bit");

pub mod types {
    use std::num::NonZeroUsize;
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
}
mod type_type;
pub use type_type::*;
use types::Pointer;
pub mod ins;
pub mod tokeniser;
pub mod chars;
pub mod compile;
pub mod stack_table;
pub mod run;

use std::{
    alloc::{alloc, dealloc, realloc, Layout},
    mem::replace,
    collections::HashMap,
    ptr,
};

#[derive(Debug)]
struct MemoryArea {
    buf: *mut u8,
    size: usize,
}
impl Default for MemoryArea {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}
impl MemoryArea {
    #[inline(always)]
    pub const fn new() -> Self {
        MemoryArea {
            buf: ptr::null_mut(),
            size: 0,
        }
    }
    #[inline(always)]
    pub fn with_size(size: usize) -> Self {
        let buf = unsafe { alloc(Layout::array::<u8>(size).unwrap()) };
        MemoryArea {
            buf,
            size,
        }
    }
    fn set_size(&mut self, size: usize) {
        if self.buf.is_null() {
            let _ = replace(self, Self::with_size(size));
        } else {
            self.buf = unsafe { realloc(self.buf, Layout::array::<u8>(self.size).unwrap(), size) };
            self.size = size;
        }
    }
    pub fn grow(&mut self, new_size: usize) {
        assert!(self.size < new_size);
        self.set_size(new_size);
    }
    pub fn deref(&mut self, index: usize, size: usize) -> Option<*mut u8> {
        if index + size > self.size {
            None
        } else {
            unsafe { Some(self.buf.add(index)) }
        }
    }
}
impl Drop for MemoryArea {
    fn drop(&mut self) {
        if !self.buf.is_null() {
            unsafe {
                ptr::slice_from_raw_parts_mut(self.buf, self.size).drop_in_place();
                dealloc(self.buf, Layout::array::<u8>(self.size).unwrap());
                self.buf = ptr::null_mut();
            }
        }
    }
}
impl Clone for MemoryArea {
    fn clone(&self) -> Self {
        let clone = MemoryArea::with_size(self.size);
        unsafe {
            ptr::copy_nonoverlapping(self.buf, clone.buf, clone.size);
        }
        clone
    }
}
#[derive(Debug, Clone)]
struct RuntimeEnvironment {
    symbol_table: HashMap<(String, Type), Pointer>,
    stack_pointer: Pointer,
    instruction_pointer: Pointer,
    text: MemoryArea,
    heap: MemoryArea,
    stack: MemoryArea,
}
// TODO make this able to run code
impl RuntimeEnvironment {
    pub fn next_ins(&mut self) -> ins::Instruction {
        ins::InstructionCoder(self).read_ins().unwrap()
    }
    pub fn deref(&mut self, p: Pointer, size: usize) -> Option<*mut u8> {
        let i = p.0.get();
        let masked = i & types::MASK;

        match masked.count_ones() {
            0 => self.text.deref(i, size),
            1 => self.heap.deref(i - types::HEAP_OFFSET, size),
            2 => self.stack.deref(i & (!types::MASK), size),
            _ => unreachable!(),
        }
    }
}

impl Read for RuntimeEnvironment {
    #[inline(always)]
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, IoError> {
        self.read_exact(buf).map(|_| buf.len())
    }
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), IoError> {
        let len = buf.len();
        match self.deref(self.instruction_pointer, len) {
            Some(p) => unsafe {
                self.instruction_pointer.0 = NonZeroUsize::new_unchecked(self.instruction_pointer.0.get() + len);
                ptr::copy_nonoverlapping(p, buf.as_mut_ptr(), len);
                Ok(())
            }
            None => Err(IoError::from(std::io::ErrorKind::NotFound))
        }
    }
}
