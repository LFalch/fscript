#[cfg(not(target_pointer_width = "64"))]
compile_error!("Does not support anything other than 64-bit");

pub mod types;

pub mod stack_table;
pub mod source;
pub mod interpreter;
pub mod vm;
