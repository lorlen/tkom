//! Error handling module and error types

use std::process::exit;

use utf8_read::StreamPosition;

#[derive(PartialEq, Eq, Hash)]
pub enum FatalError {
    IoError(String),
    MalformedUtf8(StreamPosition),
    SyntaxError {
        pos: StreamPosition,
        expected: String,
        got: String,
    },
    UnexpectedCharacter(char, StreamPosition),
    UnexpectedEof(StreamPosition),
    ValueOutOfBounds(StreamPosition),
}

pub struct ErrorHandler;

impl ErrorHandler {
    pub fn handle_error(error: FatalError) -> ! {
        let message = match error {
            FatalError::IoError(message) => message,
            FatalError::MalformedUtf8(pos) => format!(
                "Malformed UTF-8 at line {}, column {}",
                pos.line_position().0,
                pos.line_position().1,
            ),
            FatalError::SyntaxError { pos, expected, got } => format!(
                "Syntax error at line {}, column {}: expected {}, got {}",
                pos.line_position().0,
                pos.line_position().1,
                expected,
                got,
            ),
            FatalError::UnexpectedCharacter(char, pos) => format!(
                "Unexpected character '{}' at line {}, column {}",
                char,
                pos.line_position().0,
                pos.line_position().1,
            ),
            FatalError::UnexpectedEof(pos) => format!(
                "Unexpected EOF while parsing: line {}, column {}",
                pos.line_position().0,
                pos.line_position().1,
            ),
            FatalError::ValueOutOfBounds(pos) => format!(
                "Value out of bounds: line {}, column {}",
                pos.line_position().0,
                pos.line_position().1
            ),
        };
        eprintln!("{}", message);
        exit(1);
    }

    pub fn handle_result<T>(result: Result<T, FatalError>) -> T {
        match result {
            Ok(val) => val,
            Err(error_type) => Self::handle_error(error_type),
        }
    }
}
