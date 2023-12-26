pub mod backends;
use backends::Backend;

use cerium_errors::Diagnostic;

use cerium_ir as ir;

pub enum Instruction {}

#[derive(Default)]
pub struct Code {
    text_segment: Vec<String>,
    data_segment: Vec<String>,
}

pub struct Generator {
    backend: Box<dyn Backend>,
    ir_code: ir::Code,
}

impl Generator {
    pub fn new(backend: Box<dyn Backend>, ir_code: ir::Code) -> Generator {
        Generator { backend, ir_code }
    }

    pub fn generate(&mut self) -> Result<(), Diagnostic> {
        for block in self.ir_code.blocks.clone() {
            self.handle_block(block)?;
        }

        Ok(())
    }

    fn handle_block(&mut self, block: ir::Block) -> Result<(), Diagnostic> {
        self.backend.add_label(block.label);

        for instruction in block.instructions {
            let converted_instruction = self.convert_instruction(instruction)?;

            self.backend.add_instruction(converted_instruction);
        }

        Ok(())
    }

    fn convert_instruction(
        &mut self,
        instruction: ir::Instruction,
    ) -> Result<Instruction, Diagnostic> {
        unreachable!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::backends::aarch64::Aarch64Backend;

    use cerium_ast::FunctionSignature;
    use cerium_position::Position;
    use cerium_ty::Ty;

    #[test]
    fn test_function_declaration_aarch64() {
        // Hardcoded from "test_function_declaration" in the ir tests
        let ir_code = ir::Code {
            blocks: vec![ir::Block {
                label: ir::Label("main".to_string()),
                signature: FunctionSignature {
                    parameters: Vec::new(),
                    return_type: Ty::Void,
                },
                instructions: Vec::new(),
                position: Position {
                    line: 2,
                    column: 11,
                },
            }],
            position: Position {
                line: 4,
                column: 10,
            },
        };

        let backend = Aarch64Backend::default();

        let mut asm_generator = Generator::new(Box::new(backend), ir_code);

        asm_generator.generate().unwrap();

        assert_eq!(
            asm_generator.backend.combine_code_segments(),
            ".text\n.global main\nmain:\n.data\n".to_string()
        );
    }
}
