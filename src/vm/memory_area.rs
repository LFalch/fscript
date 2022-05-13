use std::{
    alloc::{alloc, dealloc, realloc, Layout},
    mem::replace,
    io::{Write, Result as IoResult, Error as IoError, ErrorKind},
    ptr,
};

use super::ins::StackOffset;

#[derive(Debug)]
pub struct MemoryArea {
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
    pub const fn size(&self) -> usize { self.size }
    fn set_size(&mut self, size: usize) {
        if self.buf.is_null() {
            let _ = replace(self, Self::with_size(size));
        } else {
            self.buf = unsafe { realloc(self.buf, Layout::array::<u8>(self.size).unwrap(), size) };
            self.size = size;
        }
    }
    pub fn grow_to(&mut self, new_size: usize) {
        assert!(self.size < new_size);
        self.set_size(new_size);
    }
    pub fn grow_by(&mut self, additional: usize) {
        self.set_size(self.size + additional);
    }
    pub fn deref(&mut self, index: usize, size: usize) -> Option<&mut [u8]> {
        // Saturating such that overflow will lead to a value that could never be larger than any usize
        // (if self.size is max usize, then they could at most be equal)
        if index.saturating_add(size) > self.size {
            None
        } else {
            unsafe { Some( std::slice::from_raw_parts_mut(
                    self.buf.add(index),
                    size
            ) ) }
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
                self.size = 0;
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

pub struct MemoryAreaCursor<'a>(pub usize, pub &'a mut MemoryArea);

impl<'a> MemoryAreaCursor<'a> {
    pub fn new(at: usize, ma: &'a mut MemoryArea) -> Self {
        MemoryAreaCursor(at, ma)
    }
}

impl Write for MemoryAreaCursor<'_> {
    fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        let size = buf.len();

        if self.0 + size > self.1.size() {
            self.1.grow_by(size);
        }

        self.1.deref(self.0, size)
            .ok_or(IoError::from(ErrorKind::OutOfMemory))?
            .iter_mut()
            .zip(buf.iter())
            .for_each(|(p, &v)| *p = v);

        self.0 += size;
        Ok(size)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Stack {
    inner: Vec<u8>,
}

impl Stack {
    pub const fn new() -> Self { Stack { inner: Vec::new() } }
    fn translate_offset(&self, so: StackOffset) -> usize {
        let so = so as usize;
        self.inner.len() - so
    }
    pub fn push<I: Iterator<Item=u8>>(&mut self, v: I) {
        self.inner.extend(v);
    }
    pub fn pop<'a>(&'a mut self, n: StackOffset) -> impl 'a + Iterator<Item=u8> {
        self.inner.drain(self.translate_offset(n)..)
    }
    pub fn read(&self, index: StackOffset, size: usize) -> Option<&[u8]> {
        let index = self.translate_offset(index);
        self.inner.get(index .. index + size)
    }
    pub fn read_mut(&mut self, index: StackOffset, size: usize) -> Option<&mut [u8]> {
        let index = self.translate_offset(index);
        self.inner.get_mut(index .. index + size)
    }
    pub fn deref(&mut self, index: usize, size: usize) -> Option<&mut [u8]> {
        self.inner.get_mut(index .. index + size)
    }
}
