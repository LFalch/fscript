use fscript::vm::{ins::Instruction, std};
use fscript::types::Type::{Function as F, Int, Unit};

fn main() {
    let rb = std();
    let inss = &[
        Instruction::push_literal(&(3i64, 7i64)),
        Instruction::Call(rb.lookup(&("add".to_string(), F(vec![Int, Int], Box::new(Int))))),
        Instruction::Call(rb.lookup(&("printint".to_string(), F(vec![Int], Box::new(Unit))))),
        Instruction::Return,
    ];
    let mut rt = rb.finish(inss);
    rt.run().unwrap();
}
