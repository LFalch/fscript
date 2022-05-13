use fscript::vm::{ins::Instruction, runtime_builder};
use fscript::types::Type::{Int, Unit};

fn main() {
    let rb = runtime_builder();
    let inss = &[
        Instruction::PushLiteral(vec![3, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0]),
        Instruction::Call(rb.lookup(&("add".to_string(), (vec![Int, Int], Int)))),
        Instruction::Call(rb.lookup(&("printint".to_string(), (vec![Int], Unit)))),
    ];
    let mut rt = rb.finish(inss);
    rt.run();
}
