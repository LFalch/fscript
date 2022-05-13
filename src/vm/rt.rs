use std::{
    collections::HashMap,
    io::{copy, Read, Error as IoError},
    fmt::{self, Debug},
    ptr, num::NonZeroUsize,
};

use crate::{types::{self, Type, Pointer, FType}};

use super::ins;
use super::memory_area::{MemoryAreaCursor, MemoryArea, Stack};

pub type FunctionSignature = (Vec<Type>, Type);

pub struct RuntimeBuilder {
    symbol_table: HashMap<(String, FunctionSignature), Pointer>,
    native_functions: HashMap<Pointer, NativeFunction>,
    data: MemoryArea,
    text: MemoryArea,
}

impl RuntimeBuilder {
    pub fn new() -> Self {
        let mut data = MemoryArea::with_size(8);
        data.deref(0, 8)
            .unwrap()
            .iter_mut()
            .for_each(|p| *p = 0);

        RuntimeBuilder {
            symbol_table: HashMap::new(),
            native_functions: HashMap::new(),
            data,
            text: MemoryArea::new(),
        }
    }
    pub fn lookup(&self, name: &(String, FunctionSignature)) -> Pointer {
        self.symbol_table[name]
    }
    pub fn add_native<S: ToString>(mut self, name: S, signature: FunctionSignature, func: NativeFunction) -> Self {
        let p = self.text.size();
        self.text.grow_by(1);
        // Make it some invalid opcode
        self.text.deref(p, 1).unwrap()[0] = 255;
        let p = Pointer::text(p);

        self.symbol_table.insert((name.to_string(), signature), p);
        self.native_functions.insert(p, func);

        self
    }
    pub fn add(mut self, signature: (String, FunctionSignature), inss: &[ins::Instruction]) -> Self {
        let p = self.write_text(inss);
        self.symbol_table.insert(signature, p);

        self
    }
    pub fn add_from<R: Read>(mut self, signature: (String, FunctionSignature), reader: R) -> Self {
        let p = self.write_text_from(reader);
        self.symbol_table.insert(signature, p);

        self
    }

    pub fn finish_from<R: Read>(mut self, reader: R) -> Runtime {
        let instruction_pointer = self.write_text_from(reader);

        Runtime::new(self, instruction_pointer)
    }
    pub fn finish(mut self, inss: &[ins::Instruction]) -> Runtime {
        let instruction_pointer = self.write_text(inss);

        Runtime::new(self, instruction_pointer)
    }

    fn write_text_from<R: Read>(&mut self, mut reader: R) -> Pointer {
        let mut cursor = MemoryAreaCursor::new(self.text.size(), &mut self.text);
        copy(&mut reader, &mut cursor).unwrap();
        let at = cursor.0;
        drop(cursor);

        Pointer::text(at)
    }
    fn write_text(&mut self, inss: &[ins::Instruction]) -> Pointer {
        let inss_size = inss.iter().map(|i| i.encoded_len()).sum();
        let at = self.text.size();
        self.text.grow_by(inss_size);

        let mut cursor = MemoryAreaCursor::new(at, &mut self.text);

        for ins in inss {
            ins::write_ins(&mut cursor, ins).unwrap();
        }

        Pointer::text(at)
    }
}

pub struct RuntimeError;

pub type NativeFunction = fn(&mut Runtime) -> Result<(), RuntimeError>;

#[derive(Clone)]
pub struct Runtime {
    native_functions: HashMap<Pointer, NativeFunction>,
    call_stack: Vec<Pointer>,
    instruction_pointer: Pointer,
    data: MemoryArea,
    text: MemoryArea,
    heap: MemoryArea,
    stack: Stack,
}

impl Runtime {
    fn new(rb: RuntimeBuilder, instruction_pointer: Pointer) -> Self {
        Runtime {
            native_functions: rb.native_functions,
            call_stack: Vec::new(),
            instruction_pointer,
            data: rb.data,
            text: rb.text,
            heap: MemoryArea::new(),
            stack: Stack::new(),
        }
    }

    pub fn next_ins(&mut self) -> Option<ins::Instruction> {
        ins::read_ins(self).ok()
    }
    pub fn deref(&mut self, p: Pointer, size: usize) -> Option<&mut [u8]> {
        let index = p.0.get();
        let masked = index & types::MASK;
        let index = index & (!types::MASK);

        match masked.count_ones() {
            0 => self.data.deref(index, size),
            1 => self.text.deref(index, size),
            2 => self.heap.deref(index, size),
            3 => self.stack.deref(index, size),
            _ => unreachable!(),
        }
    }
    pub fn dyn_alloc(&mut self, _size: usize) -> Pointer {
        todo!()
    }
    pub fn read<T: FType>(&mut self, p: Pointer) -> T {
        todo!()
    }
    pub fn pop_stack<T: FType>(&mut self) -> Result<T, RuntimeError> {
        let val: Vec<_> = self.stack.pop(T::static_type().unwrap().size() as u16).collect();
        let v = T::from_bytes(&val);

        Ok(v)
    }
    pub fn push_stack<T: FType + Debug>(&mut self, value: T) {
        self.stack.push(value.as_bytes().into_iter());
    }
    pub fn run(&mut self) -> Result<(), RuntimeError> {
        use self::ins::Instruction::*;
        loop {
            match self.next_ins().ok_or(RuntimeError)? {
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
                    let v: Vec<_> = self.stack.read(from, size).ok_or(RuntimeError)?.iter().copied().collect();
                    self.stack.read_mut(to, size).ok_or(RuntimeError)?
                        .iter_mut()
                        .zip(v.into_iter())
                        .for_each(|(p, v)| *p = v);
                }
                Jump(p) => self.instruction_pointer = p,
                SkipIf(so) => {
                    let b = self.stack.read(so, 1).ok_or(RuntimeError)?[0];
                    if b != 0 {
                        self.next_ins().ok_or(RuntimeError)?;
                    }
                },
                DynAlloc(_) => todo!(),
                DynRead{..} => todo!(),
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
                ptr::copy_nonoverlapping(p.as_mut_ptr(), buf.as_mut_ptr(), len);
                NonZeroUsize::new_unchecked(self.instruction_pointer.0.get() + len)
            }
            None => return Err(IoError::from(std::io::ErrorKind::NotFound))
        };
        Ok(())
    }
}

struct DebugKeyHashMap<'a, D: Debug, V>(&'a HashMap<D, V>);

impl<D: Debug, V> Debug for DebugKeyHashMap<'_, D, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.0.keys()).finish()
    }
}

impl Debug for Runtime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RuntimeEnvironment")
            .field("native_functions", &DebugKeyHashMap(&self.native_functions))
            .field("call_stack", &self.call_stack)
            .field("instruction_pointer", &self.instruction_pointer)
            .field("text", &self.text)
            .field("heap", &self.heap)
            .field("stack", &self.stack)
            .finish()
    }
}
