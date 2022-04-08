use super::{bytecode::Ops, Value};

pub struct Disassembler<'a> {
    offset: usize,
    bytes: &'a [u8],
    constants: &'a [Value],
}

impl<'a> Disassembler<'a> {
    pub fn new(bytes: &'a [u8], constants: &'a [Value]) -> Self {
        Disassembler {
            offset: 0,
            bytes,
            constants,
        }
    }
    fn read_op(&mut self) -> Ops {
        let op = self.bytes[self.offset];
        let op: Ops = if op < (Ops::Unknown as u8) {
            unsafe { std::mem::transmute(op) }
        } else {
            Ops::Unknown
        };
        self.offset += 1;
        op
    }
    fn read_i16(&mut self) -> i16 {
        self.offset += 2;
        let bytes = [self.bytes[self.offset - 2], self.bytes[self.offset - 1]];
        i16::from_le_bytes(bytes)
    }
    fn read_u8(&mut self) -> u8 {
        let byte = self.bytes[self.offset];
        self.offset += 1;
        byte
    }
    pub fn disassemble(&mut self) {
        println!("{:?}", self.bytes);
        let mut instr_bytes = vec![];
        while self.offset < self.bytes.len() {
            print!("{:04} ", self.offset);
            instr_bytes.clear();
            instr_bytes.push(self.bytes[self.offset]);
            print!(" {:02} ", self.bytes[self.offset]);
            let op = self.read_op();
            print!("{:>12?} ", op);
            match op {
                Ops::LoadConst => {
                    print!(" ");
                    let const_id = self.bytes[self.offset];
                    instr_bytes.push(const_id);
                    self.offset += 1;
                    println!("{}", self.constants[const_id as usize]);
                }
                Ops::Branch | Ops::Jump => {
                    let offset = self.read_i16();
                    let target = (self.offset as isize + offset as isize) as usize;
                    println!("{:04}", target);
                }
                Ops::LoadGlobal
                | Ops::StoreGlobal
                | Ops::LoadStack
                | Ops::StoreStack
                | Ops::Call
                | Ops::DefineClass => {
                    let idx = self.read_u8();
                    println!(" {:03x}", idx);
                }
                Ops::DefineFn => {
                    let stack_vars = self.read_u8();
                    let closure_count = self.read_u8();
                    println!(" [{}; {}]", stack_vars, closure_count);
                }
                _ => println!(),
            }
        }
    }
}
