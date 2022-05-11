//! The virtual machine

/// Instruction set
pub mod ins;

use std::{
    alloc::{alloc, dealloc, realloc, Layout},
    mem::replace,
    collections::HashMap,
    io::{Read, Error as IoError},
    ptr, num::NonZeroUsize,
};

use crate::{types::{self, Type, Pointer}};

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
