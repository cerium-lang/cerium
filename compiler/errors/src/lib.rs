use cerium_position::Position;

use std::fmt::Display;
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub file_path: PathBuf,
    position: Position,
    message: String,
}

impl Error {
    pub fn invalid(position: Position, ctx: &str) -> Error {
        Error {
            file_path: "".into(),
            position,
            message: format!("invalid {}", ctx),
        }
    }

    pub fn expected(position: Position, ctx: &str) -> Error {
        Error {
            file_path: "".into(),
            position,
            message: format!("expected {}", ctx),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: {}",
            self.file_path.display(),
            self.position.line,
            self.position.column,
            self.message
        )
    }
}
