use cerium_ast::*;

use cerium_errors::Diagnostic;

use cerium_position::Position;

use cerium_ty::Ty;

#[derive(Debug, PartialEq)]
pub enum Value {
    String { value: String, position: Position },
    Char { value: char, position: Position },
    Int { value: i64, position: Position },
    Float { value: f64, position: Position },
}

impl Value {
    pub fn get_position(&self) -> Position {
        match self {
            Value::String { position, .. } => *position,
            Value::Char { position, .. } => *position,
            Value::Int { position, .. } => *position,
            Value::Float { position, .. } => *position,
        }
    }

    pub fn get_type(&self) -> Ty {
        match self {
            Value::String { .. } => Ty::String,
            Value::Char { .. } => Ty::Char,
            Value::Int { .. } => Ty::Int,
            Value::Float { .. } => Ty::Float,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {}

impl TryFrom<Value> for Instruction {
    type Error = ();

    fn try_from(_value: Value) -> Result<Self, Self::Error> {
        // No value currently can be used as an instruction
        Err(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Label(String);

#[derive(Debug, PartialEq)]
pub struct Block {
    pub label: Label,
    pub signature: BlockSignature,
    pub instructions: Vec<Instruction>,
    pub position: Position,
}

type BlockSignature = FunctionSignature;

#[derive(Default, Debug, PartialEq)]
pub struct Code {
    pub blocks: Vec<Block>,
    pub position: Position,
}

#[derive(Default)]
pub struct Generator {
    pub code: Code,
    current_block: Option<usize>,
}

impl Generator {
    pub fn generate(&mut self, package: Package) -> Result<(), Diagnostic> {
        self.code.position = package.position;

        for declaration in package.declarations {
            self.handle_declaration(declaration)?;
        }

        Ok(())
    }

    fn handle_declaration(&mut self, declaration: Declaration) -> Result<(), Diagnostic> {
        match declaration {
            Declaration::FunctionDeclaration {
                name,
                signature,
                body,
                position,
            } => self.handle_function_declaration(name, signature, body, position),
        }
    }

    fn handle_function_declaration(
        &mut self,
        name: ExprKind,
        signature: FunctionSignature,
        body: Vec<Node>,
        position: cerium_position::Position,
    ) -> Result<(), Diagnostic> {
        let label = match name {
            ExprKind::Identifier { symbol, .. } => Label(symbol),
            _ => unreachable!(),
        };

        self.code.blocks.push(Block {
            label,
            signature,
            instructions: Vec::new(),
            position,
        });

        self.current_block = Some(self.code.blocks.len() - 1);

        for node in body {
            self.handle_node(node)?;
        }

        self.current_block = None;

        Ok(())
    }

    fn handle_node(&mut self, node: Node) -> Result<(), Diagnostic> {
        match node {
            Node::Stmt(kind) => self.handle_stmt(*kind),
            Node::Expr(kind) => {
                if let Ok(instruction) = Instruction::try_from(self.handle_expr(*kind)?) {
                    self.code.blocks[self.current_block.unwrap()]
                        .instructions
                        .push(instruction);
                };

                Ok(())
            }
        }
    }

    fn handle_stmt(&self, _kind: StmtKind) -> Result<(), Diagnostic> {
        Ok(())
    }

    fn handle_expr(&self, kind: ExprKind) -> Result<Value, Diagnostic> {
        Ok(match kind {
            // Identifiers requires symbol table
            ExprKind::Identifier { .. } => unimplemented!(),
            ExprKind::String { value, position } => Value::String { value, position },
            ExprKind::Char { value, position } => Value::Char { value, position },
            ExprKind::Int { value, position } => Value::Int { value, position },
            ExprKind::Float { value, position } => Value::Float { value, position },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_declaration() {
        // Hardcoded from "test_function_declaration" in the parser tests
        let package = Package {
            name: "".to_string(),
            declarations: vec![Declaration::FunctionDeclaration {
                name: ExprKind::Identifier {
                    symbol: "main".to_string(),
                    position: Position {
                        line: 2,
                        column: 11,
                    },
                },
                signature: FunctionSignature {
                    parameters: Vec::new(),
                    return_type: Ty::Void,
                },
                body: Vec::new(),
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

        let mut ir_generator = Generator::default();

        ir_generator.generate(package).unwrap();

        assert_eq!(
            ir_generator.code,
            Code {
                blocks: vec![Block {
                    label: Label("main".to_string()),
                    signature: FunctionSignature {
                        parameters: Vec::new(),
                        return_type: Ty::Void
                    },
                    instructions: Vec::new(),
                    position: Position {
                        line: 2,
                        column: 11
                    }
                }],
                position: Position {
                    line: 4,
                    column: 10,
                },
            }
        );
    }
}
