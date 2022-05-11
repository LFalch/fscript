use std::io::{Write, Read, Error as IoError};
use std::mem::size_of;

use byteorder::{ByteOrder, LittleEndian};

use crate::types::{Offset, Pointer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    PushLiteral(Vec<u8>),
    PushPop{
        position: Offset,
        size: Offset,
    },
    Pop(Offset),
    Call(Pointer),
    Return,
}

impl Instruction {
    pub fn encoded_len(&self) -> usize {
        use self::Instruction::*;
        1 + match self {
            PushLiteral(v) => size_of::<Offset>() + v.len(),
            PushPop{..} => 2 * size_of::<Offset>(),
            Pop(_) => size_of::<Offset>(),
            Call(_) => size_of::<Pointer>(),
            Return => 0,
        }
    }
}

pub mod opcode {
    pub const RET: u8 = 0;
    pub const CALL: u8 = 1;
    pub const POP: u8 = 2;
    pub const PUSH_LIT: u8 = 3;
    pub const PUSH_POP: u8 = 4;
}

#[derive(Debug)]
pub enum Error {
    InvalidOpcode,
    IoError(IoError),
}

impl From<IoError> for Error {
    #[inline(always)]
    fn from(io: IoError) -> Self {
        Error::IoError(io)
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct InstructionCoder<T>(pub T);

impl<T> InstructionCoder<T> {
    #[inline(always)]
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T: Write> InstructionCoder<T> {
    pub fn write_ins(&mut self, ins: Instruction) -> Result<(), Error> {
        use self::Instruction::*;
        match ins {
            Return => self.0.write_all(&[opcode::RET])?,
            Call(p) => {
                let mut to_write = [opcode::CALL, 0, 0, 0, 0, 0, 0, 0, 0];
                LittleEndian::write_u64(&mut to_write[1..], p.into());
                self.0.write_all(&to_write)?;
            }
            Pop(o) => {
                let mut to_write = [opcode::POP, 0, 0, 0, 0, 0, 0, 0, 0];
                LittleEndian::write_u64(&mut to_write[1..], o as u64);
                self.0.write_all(&to_write)?;
            }
            PushPop{position, size} => {
                let mut to_write = [opcode::PUSH_POP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
                LittleEndian::write_u64(&mut to_write[1..9], position as u64);
                LittleEndian::write_u64(&mut to_write[9..17], size as u64);
                self.0.write_all(&to_write)?;
            }
            PushLiteral(v) => {
                let mut to_write = [opcode::PUSH_LIT, 0, 0, 0, 0, 0, 0, 0, 0];
                LittleEndian::write_u64(&mut to_write[1..], v.len() as u64);
                self.0.write_all(&to_write)?;
                self.0.write_all(&v)?;
            }
        }

        Ok(())
    }
}
impl<T: Read> InstructionCoder<T> {
    pub fn read_ins(&mut self) -> Result<Instruction, Error> {
        let mut opcode = [0];
        self.0.read_exact(&mut opcode)?;
        let [opcode] = opcode;
        match opcode {
            opcode::RET => Ok(Instruction::Return),
            opcode::CALL => {
                let mut location = [0; 8];
                self.0.read_exact(&mut location)?;
                let location = LittleEndian::read_u64(&location);
                Ok(Instruction::Call(location.into()))
            }
            opcode::POP => {
                let mut offset = [0; 8];
                self.0.read_exact(&mut offset)?;
                let offset = LittleEndian::read_u64(&offset) as usize;
                Ok(Instruction::Pop(offset))
            }
            opcode::PUSH_POP => {
                let mut buf = [0; 16];
                self.0.read_exact(&mut buf)?;
                let position = LittleEndian::read_u64(&buf[..8]) as usize;
                let size = LittleEndian::read_u64(&buf[8..]) as usize;
                Ok(Instruction::PushPop{position, size})
            }
            opcode::PUSH_LIT => {
                let mut length = [0; 8];
                self.0.read_exact(&mut length)?;
                let length = LittleEndian::read_u64(&length) as usize;

                let mut v = vec![0; length];

                self.0.read_exact(&mut *v)?;

                Ok(Instruction::PushLiteral(v))
            }
            _ => Err(Error::InvalidOpcode),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use super::*;
    #[test]
    fn write_read() {
        let inss = [
            Instruction::Pop(16),
            Instruction::Call(Pointer::text(0xdead)),
            Instruction::PushLiteral(vec![b'h', b'e', b'l', b'l', b'o']),
            Instruction::PushPop{position: 16, size: 4},
            Instruction::Return,
        ];
        let mut coder = InstructionCoder(Vec::<u8>::new());
        for ins in &inss {
            coder.write_ins(ins.clone()).unwrap();
        }
        let v = coder.into_inner();
        assert_eq!(v.len(), inss.iter().map(Instruction::encoded_len).sum());
        let mut coder = InstructionCoder(Cursor::new(dbg!(v)));
        let mut inss_read = Vec::with_capacity(inss.len());
        loop {
            let ins = coder.read_ins().unwrap();
            let done = matches!(&ins, Instruction::Return);
            inss_read.push(ins);
            if done {
                break
            }
        }
        assert_eq!(&inss[..], &*inss_read);
    }
}