use std::io::{Write, Read, Error as IoError};
use std::mem::size_of;

use byteorder::{ByteOrder, LittleEndian};

use crate::types::{Offset, Pointer, FType};

pub type StackOffset = u16;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Return,
    Call(Pointer),
    Pop(StackOffset),
    PushLiteral(Vec<u8>),
    StackMove{
        size: Offset,
        from: StackOffset,
        to: StackOffset,
    },
    Jump(Pointer),
    SkipIf(StackOffset),
    PopStackAddr(StackOffset),
    MemRead {
        size: Offset,
        addr: StackOffset,
        offset: StackOffset,
    }
}

impl Instruction {
    pub fn push_literal<T: FType>(v: &T) -> Self {
        Self::PushLiteral(v.as_bytes())
    }
    pub fn encoded_len(&self) -> usize {
        use self::Instruction::*;
        1 + match self {
            Return => 0,
            Call(_) => size_of::<Pointer>(),
            Pop(_) => size_of::<StackOffset>(),
            PushLiteral(v) => size_of::<Offset>() + v.len(),
            StackMove { .. } => size_of::<Offset>() + size_of::<StackOffset>()  + size_of::<StackOffset>(),
            Jump(_) => size_of::<Pointer>(),
            SkipIf(_) => size_of::<StackOffset>(),
            PopStackAddr(_) => size_of::<StackOffset>(),
            MemRead {..} => size_of::<Offset>() + size_of::<StackOffset>() + size_of::<StackOffset>(),
        }
    }
}

pub mod opcode {
    pub const RET: u8 = 0;
    pub const CALL: u8 = 1;
    pub const POP: u8 = 2;
    pub const PUSH_LIT: u8 = 3;
    pub const SMOV: u8 = 4;
    pub const JUMP: u8 = 5;
    pub const SKIPIF: u8 = 6;
    pub const PSA: u8 = 7;
    pub const READ: u8 = 8;
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

pub fn write_ins<W: Write>(w: &mut W, ins: &Instruction) -> Result<(), Error> {
    use self::Instruction::*;
    match ins {
        Return => w.write_all(&[opcode::RET])?,
        &Call(p) => {
            let mut to_write = [opcode::CALL, 0, 0, 0, 0, 0, 0, 0, 0];
            LittleEndian::write_u64(&mut to_write[1..], p.into());
            w.write_all(&to_write)?;
        }
        &Pop(o) => {
            let mut to_write = [opcode::POP, 0, 0];
            LittleEndian::write_u16(&mut to_write[1..], o as u16);
            w.write_all(&to_write)?;
        }
        PushLiteral(v) => {
            let mut to_write = [opcode::PUSH_LIT, 0, 0, 0, 0, 0, 0, 0, 0];
            LittleEndian::write_u64(&mut to_write[1..], v.len() as u64);
            w.write_all(&to_write)?;
            w.write_all(v)?;
        }
        &StackMove{size, from, to} => {
            let mut to_write = [opcode::SMOV, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
            LittleEndian::write_u64(&mut to_write[1..9], size as u64);
            LittleEndian::write_u16(&mut to_write[9..11], from as u16);
            LittleEndian::write_u16(&mut to_write[11..13], to as u16);
            w.write_all(&to_write)?;
        }
        &Jump(p) => {
            let mut to_write = [opcode::JUMP, 0, 0, 0, 0, 0, 0, 0, 0];
            LittleEndian::write_u64(&mut to_write[1..], p.into());
            w.write_all(&to_write)?;
        }
        &SkipIf(o) => {
            let mut to_write = [opcode::SKIPIF, 0, 0];
            LittleEndian::write_u16(&mut to_write[1..], o as u16);
            w.write_all(&to_write)?;
        }
        &PopStackAddr(size) => {
            let mut to_write = [opcode::PSA, 0, 0];
            LittleEndian::write_u16(&mut to_write[1..3], size);
            w.write_all(&to_write)?;
        }
        &MemRead { size, addr, offset } => {
            let mut to_write = [opcode::READ, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
            LittleEndian::write_u64(&mut to_write[1..9], size as u64);
            LittleEndian::write_u16(&mut to_write[9..11], addr as u16);
            LittleEndian::write_u16(&mut to_write[11..13], offset as u16);
            w.write_all(&to_write)?;
        }
    }

    Ok(())
}

pub fn read_ins<R: Read>(r: &mut R) -> Result<Instruction, Error> {
    let mut opcode = [0];
    r.read_exact(&mut opcode)?;
    let [opcode] = opcode;
    match opcode {
        opcode::RET => Ok(Instruction::Return),
        opcode::CALL => {
            let mut location = [0; 8];
            r.read_exact(&mut location)?;
            let location = LittleEndian::read_u64(&location);
            Ok(Instruction::Call(location.into()))
        }
        opcode::POP => {
            let mut offset = [0; 2];
            r.read_exact(&mut offset)?;
            let offset = LittleEndian::read_u16(&offset);
            Ok(Instruction::Pop(offset))
        }
        opcode::PUSH_LIT => {
            let mut length = [0; 8];
            r.read_exact(&mut length)?;
            let length = LittleEndian::read_u64(&length) as usize;

            let mut v = vec![0; length];

            r.read_exact(&mut *v)?;

            Ok(Instruction::PushLiteral(v))
        }
        opcode::SMOV => {
            let mut buf = [0; 12];
            r.read_exact(&mut buf)?;
            let size = LittleEndian::read_u64(&buf[..8]) as Offset;
            let from = LittleEndian::read_u16(&buf[8..10]);
            let to = LittleEndian::read_u16(&buf[10..]);
            Ok(Instruction::StackMove{size, from, to})
        }
        opcode::JUMP => {
            let mut location = [0; 8];
            r.read_exact(&mut location)?;
            let location = LittleEndian::read_u64(&location);
            Ok(Instruction::Jump(location.into()))
        }
        opcode::SKIPIF => {
            let mut offset = [0; 2];
            r.read_exact(&mut offset)?;
            let offset = LittleEndian::read_u16(&offset);
            Ok(Instruction::SkipIf(offset))
        }
        opcode::PSA => {
            let mut addr = [0; 2];
            r.read_exact(&mut addr)?;
            let addr = LittleEndian::read_u16(&addr);
            Ok(Instruction::PopStackAddr(addr))
        }
        opcode::READ => {
            let mut buf = [0; 12];
            r.read_exact(&mut buf)?;
            let size = LittleEndian::read_u64(&buf[..8]) as Offset;
            let addr = LittleEndian::read_u16(&buf[8..10]);
            let offset = LittleEndian::read_u16(&buf[10..]);
            Ok(Instruction::MemRead{size, addr, offset})
        }
        _ => Err(Error::InvalidOpcode),
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
            Instruction::Call(Pointer::top(0xdead)),
            Instruction::PushLiteral(vec![b'h', b'e', b'l', b'l', b'o']),
            Instruction::StackMove{from: 8, size: 4, to: 16},
            Instruction::SkipIf(0),
            Instruction::Jump(Pointer::bottom(125)),
            Instruction::PopStackAddr(128),
            Instruction::MemRead { size: 16, addr: 0, offset: 9 },
            Instruction::MemRead { size: 16, addr: 0, offset: 0 },
            Instruction::Return,
        ];
        let mut code = Vec::<u8>::new();
        for ins in &inss {
            write_ins(&mut code, ins).unwrap();
        }
        assert_eq!(code.len(), inss.iter().map(Instruction::encoded_len).sum());
        let mut code = Cursor::new(dbg!(code));
        let mut inss_read = Vec::with_capacity(inss.len());
        loop {
            let ins = read_ins(&mut code).unwrap();
            let done = matches!(&ins, Instruction::Return);
            inss_read.push(ins);
            if done {
                break
            }
        }
        assert_eq!(&inss[..], &*inss_read);
    }
}
