//! Error handling module and error types

use std::process::exit;

use colored::*;
use utf8_read::StreamPosition;

use crate::lexer::token::{Token, TokenKind};

pub enum FatalError<'a> {
    // Reader errors
    IoError(String),
    MalformedUtf8(StreamPosition),

    // Lexer errors
    InvalidEscapeChar(char, StreamPosition),
    UnexpectedCharacter(char, StreamPosition),
    LiteralOutOfBounds(StreamPosition),

    // Parser errors
    UnexpectedToken(Token, TokenKind),
    UnexpectedTokenMulti(Token, &'a [TokenKind]),
    BlockExpected(StreamPosition),
    ExprExpected(StreamPosition),
    MatchArmExpected(StreamPosition),
    RangeExpected(StreamPosition),
    LiteralExpected(StreamPosition),

    // Multi-purpose errors
    UnexpectedEof(StreamPosition),
}

pub struct ErrorHandler;

impl ErrorHandler {
    pub fn handle_error(error: FatalError) -> ! {
        let message = match error {
            FatalError::IoError(message) => message,
            FatalError::MalformedUtf8(pos) => format!(
                "Malformed UTF-8 [{}:{}]",
                pos.line_position().0,
                pos.line_position().1,
            ),
            FatalError::InvalidEscapeChar(char, pos) => format!(
                "Invalid escape character: '{}' [{}:{}]",
                char,
                pos.line_position().0,
                pos.line_position().1,
            ),
            FatalError::UnexpectedCharacter(char, pos) => format!(
                "Unexpected character '{}' [{}:{}]",
                char,
                pos.line_position().0,
                pos.line_position().1,
            ),
            FatalError::UnexpectedEof(pos) => format!(
                "Unexpected EOF while parsing [{}:{}]",
                pos.line_position().0,
                pos.line_position().1,
            ),
            FatalError::LiteralOutOfBounds(pos) => format!(
                "Number literal out of bounds [{}:{}]",
                pos.line_position().0,
                pos.line_position().1
            ),
            FatalError::UnexpectedToken(token, kind) => format!(
                "Expected {}, got {} instead [{}:{}]",
                kind,
                token.kind,
                token.position.line_position().0,
                token.position.line_position().1
            ),
            FatalError::UnexpectedTokenMulti(token, kinds) => format!(
                "Expected one of {}; got {} [{}:{}]",
                kinds
                    .iter()
                    .map(|kind| format!("{}", kind))
                    .collect::<Vec<_>>()
                    .join(", "),
                token.kind,
                token.position.line_position().0,
                token.position.line_position().1
            ),
            FatalError::BlockExpected(pos) => format!(
                "Expected statement block [{}, {}]",
                pos.line_position().0,
                pos.line_position().1
            ),
            FatalError::ExprExpected(pos) => format!(
                "Expected expression [{}, {}]",
                pos.line_position().0,
                pos.line_position().1
            ),
            FatalError::MatchArmExpected(pos) => format!(
                "Expected match arm [{}, {}]",
                pos.line_position().0,
                pos.line_position().1
            ),
            FatalError::RangeExpected(pos) => format!(
                "Expected range [{}, {}]",
                pos.line_position().0,
                pos.line_position().1
            ),
            FatalError::LiteralExpected(pos) => format!(
                "Expected literal [{}, {}]",
                pos.line_position().0,
                pos.line_position().1
            ),
        };

        if cfg!(test) {
            panic!("{}{}", message.red(), "".clear());
        } else {
            eprintln!("{}{}", message.red(), "".clear());
            exit(1);
        }
    }

    pub fn handle_result<T>(result: Result<T, FatalError>) -> T {
        match result {
            Ok(val) => val,
            Err(error_type) => Self::handle_error(error_type),
        }
    }
}
