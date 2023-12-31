use cerium_position::Position;
use cerium_ty::Ty;

#[derive(Debug, Default, PartialEq)]
pub struct Package {
    pub name: String,
    pub declarations: Vec<Declaration>,
    pub position: Position,
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    FunctionDeclaration {
        name: ExprKind,
        signature: FunctionSignature,
        body: Vec<Node>,
        position: Position,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
    pub name: ExprKind,
    pub expected_type: Ty,
    pub position: Position,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Stmt(Box<StmtKind>),
    Expr(Box<ExprKind>),
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Identifier { symbol: String, position: Position },
    String { value: String, position: Position },
    Char { value: char, position: Position },
    Int { value: i64, position: Position },
    Float { value: f64, position: Position },
}

impl ExprKind {
    pub fn get_position(&self) -> Position {
        match self {
            ExprKind::Identifier { position, .. } => *position,
            ExprKind::String { position, .. } => *position,
            ExprKind::Char { position, .. } => *position,
            ExprKind::Int { position, .. } => *position,
            ExprKind::Float { position, .. } => *position,
        }
    }
}
