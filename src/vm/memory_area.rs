use std::{
    io::{Write, Result as IoResult, Error as IoError, ErrorKind},
};

use crate::types::Pointer;

use super::ins::StackOffset;

#[derive(Debug, Default, Clone)]
pub struct MemoryArea {
    buf: Vec<u8>,
}
impl MemoryArea {
    #[inline(always)]
    pub const fn new() -> Self {
        MemoryArea { buf: Vec::new() }
    }
    #[inline(always)]
    pub fn with_size(size: usize) -> Self {
        MemoryArea { buf: vec![0; size] }
    }
    pub fn size(&self) -> usize { self.buf.len() }
    fn set_size(&mut self, size: usize) {
        self.buf.resize(size, 0);
    }
    pub fn grow_to(&mut self, new_size: usize) {
        assert!(self.buf.len() < new_size);
        self.set_size(new_size);
    }
    pub fn grow_by(&mut self, additional: usize) {
        self.set_size(self.buf.len() + additional);
    }
    pub fn deref_mut(&mut self, index: usize, size: usize) -> Option<&mut [u8]> {
        self.buf.get_mut(index..index+size)
    }
    pub fn deref(&self, index: usize, size: usize) -> Option<&[u8]> {
        self.buf.get(index..index+size)
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

        self.1.deref_mut(self.0, size)
            .ok_or(IoError::from(ErrorKind::OutOfMemory))?
            .iter_mut()
            .zip(buf.iter())
            .for_each(|(p, &v)| *p = v);

        self.0 += size;
        Ok(size)
    }
    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> IoResult<()> {
        self.write(buf).map(|_| ())
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
    pub fn offset_to_pointer(&self, so: StackOffset) -> Pointer {
        let index = self.translate_offset(so);
        Pointer::bottom(index)
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
    pub fn deref_mut(&mut self, index: usize, size: usize) -> Option<&mut [u8]> {
        self.inner.get_mut(index .. index + size)
    }
    pub fn deref(&self, index: usize, size: usize) -> Option<&[u8]> {
        self.inner.get(index .. index + size)
    }
}
