pub mod aarch64;

use crate::Instruction;

use cerium_ir::Label;

pub trait Backend {
    fn add_label(&mut self, label: Label);
    fn add_instruction(&mut self, instruction: Instruction);
    fn combine_code_segments(&self) -> String;
}
