use std::{
    collections::HashMap,
    io::{copy, Read, Error as IoError},
    fmt::{self, Debug},
    ptr, num::NonZeroUsize,
};

use byteorder::{LittleEndian, ByteOrder};

use crate::{types::{Type, Pointer, FType}};

use super::ins::{self, Error as InsError};
use super::memory_area::{MemoryAreaCursor, MemoryArea, Stack};

pub struct RuntimeBuilder {
    symbol_table: HashMap<(String, Type), Pointer>,
    native_functions: HashMap<Pointer, NativeFunction>,
    top: MemoryArea,
}

impl Debug for RuntimeBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RuntimeEnvironment")
            .field("symbol_table", &self.symbol_table)
            .field("native_functions", &DebugKeyHashMap(&self.native_functions))
            .field("top", &self.top)
            .finish()
    }
}

impl RuntimeBuilder {
    pub fn new() -> Self {
        let mut top = MemoryArea::with_size(8);
        top.deref_mut(0, 8)
            .unwrap()
            .iter_mut()
            .for_each(|p| *p = 0);

        RuntimeBuilder {
            symbol_table: HashMap::new(),
            native_functions: HashMap::new(),
            top
        }
    }
    pub fn lookup(&self, name: &(String, Type)) -> Pointer {
        self.symbol_table[name]
    }
    pub fn add_native<S: ToString>(mut self, name: S, arg_type: Type, ret_type: Type, func: NativeFunction) -> Self {
        let p = self.write([255].as_slice());
        self.symbol_table.insert((name.to_string(), Type::Function(Box::new(arg_type), Box::new(ret_type))), p);
        self.native_functions.insert(p, func);

        self
    }
    pub fn add_func<S: ToString>(mut self, name: S, arg_type: Type, ret_type: Type, inss: &[ins::Instruction]) -> Self {
        let p = self.write_inss(inss);
        self.symbol_table.insert((name.to_string(), Type::Function(Box::new(arg_type), Box::new(ret_type))), p);

        self
    }
    pub fn add<S: ToString, R: Read>(mut self, name: S, sym_type: Type, reader: R) -> Self {
        let p = self.write(reader);
        self.symbol_table.insert((name.to_string(), sym_type), p);

        self
    }

    pub fn finish_from<R: Read>(mut self, reader: R) -> Runtime {
        let instruction_pointer = self.write(reader);

        Runtime::new(self, instruction_pointer)
    }
    pub fn finish(mut self, inss: &[ins::Instruction]) -> Runtime {
        let instruction_pointer = self.write_inss(inss);

        Runtime::new(self, instruction_pointer)
    }

    fn write<R: Read>(&mut self, mut reader: R) -> Pointer {
        let at = self.top.size();
        let mut cursor = MemoryAreaCursor::new(at, &mut self.top);
        copy(&mut reader, &mut cursor).unwrap();

        Pointer::top(at)
    }
    fn write_inss(&mut self, inss: &[ins::Instruction]) -> Pointer {
        let at = self.top.size();
        let inss_size = inss.iter().map(|i| i.encoded_len()).sum();
        self.top.grow_by(inss_size);

        let mut cursor = MemoryAreaCursor::new(at, &mut self.top);

        for ins in inss {
            ins::write_ins(&mut cursor, ins).unwrap();
        }

        Pointer::top(at)
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidRead,
    InvalidWrite,
    InsError(InsError),
    IoError(IoError),
}

pub type NativeFunction = fn(&mut Runtime) -> Result<(), Error>;

#[derive(Clone)]
pub struct Runtime {
    native_functions: HashMap<Pointer, NativeFunction>,
    call_stack: Vec<Pointer>,
    instruction_pointer: Pointer,
    top: MemoryArea,
    heap_start: usize,
    stack: Stack,
}

impl Runtime {
    fn new(rb: RuntimeBuilder, instruction_pointer: Pointer) -> Self {
        Runtime {
            native_functions: rb.native_functions,
            call_stack: Vec::new(),
            instruction_pointer,
            heap_start: rb.top.size(),
            top: rb.top,
            stack: Stack::new(),
        }
    }

    pub fn next_ins(&mut self) -> Result<ins::Instruction, Error> {
        ins::read_ins(self).map_err(Error::InsError)
    }
    pub fn deref_mut(&mut self, p: Pointer, size: usize) -> Option<&mut [u8]> {
        let index = p.0.get();

        if index < self.heap_start {
            // RO text and data
            None
        } else if index <= self.top.size() {
            // Heap
            self.top.deref_mut(index, size)
        } else {
            // Stack
            let index = usize::MAX - index;
            self.stack.deref_mut(index, size)
        }
    }
    pub fn deref(&self, p: Pointer, size: usize) -> Option<&[u8]> {
        let index = p.0.get();

        if index <= self.top.size() {
            // RO text and data or heap
            self.top.deref(index, size)
        } else {
            // Stack
            let index = usize::MAX - index;
            self.stack.deref(index, size)
        }
    }
    pub fn dyn_alloc(&mut self, _size: usize) -> Pointer {
        todo!()
    }
    pub fn read<T: FType>(&self, p: Pointer) -> Result<T, Error> {
        let data: Vec<_> = self.deref(p, T::size()).ok_or(Error::InvalidRead)?.into_iter().copied().collect();
        Ok(T::from_bytes(&data))
    }
    pub fn pop_stack<T: FType>(&mut self) -> Result<T, Error> {
        let val: Vec<_> = self.stack.pop(T::size() as u16).collect();
        let v = T::from_bytes(&val);

        Ok(v)
    }
    pub fn push_stack<T: FType>(&mut self, value: T) {
        self.stack.push(value.as_bytes().into_iter());
    }
    pub fn run(&mut self) -> Result<(), Error> {
        use self::ins::Instruction::*;
        loop {
            match self.next_ins()? {
                Return => {
                    match self.call_stack.pop() {
                        None => break,
                        Some(ip) => self.instruction_pointer = ip,
                    }
                }
                Call(p) => {
                    self.call_stack.push(self.instruction_pointer);
                    match self.native_functions.get(&p) {
                        Some(f) => {
                            f(self)?;
                            match self.call_stack.pop() {
                                None => break,
                                Some(ip) => self.instruction_pointer = ip,
                            }
                        }
                        None => self.instruction_pointer = p,
                    }
                }
                Pop(so) => { let _ = self.stack.pop(so); }
                PushLiteral(v) => self.stack.push(v.into_iter()),
                StackMove { size, from, to } => {
                    let v: Vec<_> = self.stack.read(from, size).ok_or(Error::InvalidRead)?.iter().copied().collect();
                    self.stack.read_mut(to, size).ok_or(Error::InvalidWrite)?
                        .iter_mut()
                        .zip(v.into_iter())
                        .for_each(|(p, v)| *p = v);
                }
                Jump(p) => self.instruction_pointer = p,
                SkipIf(so) => {
                    let b = self.stack.read(so, 1).ok_or(Error::InvalidRead)?[0];
                    if b != 0 {
                        self.next_ins()?;
                    }
                },
                PopStackAddr(addr) => {
                    self.push_stack(self.stack.offset_to_pointer(addr));
                }
                MemRead{addr, size, offset} => {
                    let addr = self.stack.read(addr, 8).ok_or(Error::InvalidRead)?;
                    let offset = self.stack.read(offset, 8).ok_or(Error::InvalidRead)?;

                    let p: Pointer = (LittleEndian::read_u64(&addr) + LittleEndian::read_u64(&offset)).into();

                    let to_push: Vec<_> = self.deref(p, size).ok_or(Error::InvalidRead)?.into_iter().copied().collect();

                    self.stack.push(to_push.into_iter());
                }
            }
        }
        Ok(())
    }
}

impl Read for Runtime {
    #[inline(always)]
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, IoError> {
        self.read_exact(buf).map(|_| buf.len())
    }
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), IoError> {
        let len = buf.len();
        self.instruction_pointer.0 = match self.deref(self.instruction_pointer, len) {
            Some(p) => unsafe {
                ptr::copy_nonoverlapping(p.as_ptr(), buf.as_mut_ptr(), len);
                NonZeroUsize::new_unchecked(self.instruction_pointer.0.get() + len)
            }
            None => return Err(IoError::from(std::io::ErrorKind::PermissionDenied))
        };
        Ok(())
    }
}

struct DebugKeyHashMap<'a, D: Debug, V>(&'a HashMap<D, V>);

impl<D: Debug, V> Debug for DebugKeyHashMap<'_, D, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
        .entries(self.0
            .iter()
            .map(|(d, v)| (d, v as *const V))
        )
        .finish()
    }
}

impl Debug for Runtime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RuntimeEnvironment")
            .field("native_functions", &DebugKeyHashMap(&self.native_functions))
            .field("call_stack", &self.call_stack)
            .field("instruction_pointer", &self.instruction_pointer)
            .field("top", &self.top)
            .field("heap_start", &self.heap_start)
            .field("stack", &self.stack)
            .finish()
    }
}
