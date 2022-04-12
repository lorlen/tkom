//! Error handling module and error types

use std::process::exit;

use utf8_read::StreamPosition;

#[derive(PartialEq, Eq, Hash)]
pub enum FatalError {
    IoError(String),
    MalformedUtf8(StreamPosition),
    SyntaxError(StreamPosition),
    UnexpectedEof(StreamPosition),
    ValueOutOfBounds(StreamPosition),
}

pub struct ErrorHandler;

impl ErrorHandler {
    pub fn handle_error(error: FatalError) -> ! {
        let message = match error {
            FatalError::IoError(message) => message,
            FatalError::MalformedUtf8(position) => format!(
                "Malformed UTF-8 at line {}, column {}",
                position.line_position().0,
                position.line_position().1,
            ),
            FatalError::SyntaxError(position) => format!(
                "Syntax error at line {}, column {}",
                position.line_position().0,
                position.line_position().1,
            ),
            FatalError::UnexpectedEof(position) => format!(
                "Unexpected EOF while parsing: line {}, column {}",
                position.line_position().0,
                position.line_position().1,
            ),
            FatalError::ValueOutOfBounds(position) => format!(
                "Value out of bounds: line {}, column {}",
                position.line_position().0,
                position.line_position().1
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
