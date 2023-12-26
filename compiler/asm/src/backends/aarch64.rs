use super::Backend;

use crate::{Code, Instruction};

#[derive(Default)]
pub struct Aarch64Backend {
    code: Code,
}

impl Backend for Aarch64Backend {
    fn add_label(&mut self, label: cerium_ir::Label) {
        self.code.text_segment.push(format!(".global {}", label));
        self.code.text_segment.push(format!("{}:", label));
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.code
            .text_segment
            .push(format!("\t{}", instruction.to_aarch64()))
    }

    fn combine_code_segments(&self) -> String {
        let mut result = String::new();

        result.push_str(".text\n");
        result.push_str(&self.code.text_segment.join("\n"));

        result.push('\n');

        result.push_str(".data\n");
        result.push_str(&self.code.data_segment.join("\n"));

        result
    }
}

impl Instruction {
    pub fn to_aarch64(&self) -> String {
        unreachable!();
    }
}
