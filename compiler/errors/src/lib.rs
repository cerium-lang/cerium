use cerium_position::Position;

use std::fmt::Display;
use std::path::PathBuf;

#[derive(Debug, PartialEq)]
pub struct Error<'a> {
    pub file_path: PathBuf,
    position: &'a Position,
    message: String,
}

impl<'a> Error<'a> {
    pub fn invalid(position: &'a Position, ctx: &str) -> Error<'a> {
        Error {
            file_path: "".into(),
            position,
            message: format!("invalid {}", ctx),
        }
    }

    pub fn expected(position: &'a Position, ctx: &str) -> Error<'a> {
        Error {
            file_path: "".into(),
            position,
            message: format!("expected {}", ctx),
        }
    }
}

impl<'a> Display for Error<'a> {
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
