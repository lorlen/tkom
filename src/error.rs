//! Error handling module and error types

use std::process;

use colored::Colorize;
use utf8_read::StreamPosition;

use crate::data::token::{Token, TokenKind};

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

    // Lexer/Parser errors
    UnexpectedEof(StreamPosition),

    // SemCheck errors
    DuplicateDeclaration(String),
    NoMainFunction,
    NotInLoop,
    MismatchedTypes(String, String),
    InvalidCast(String, String),
    NonPrimitiveCast,
    UndeclaredVariable(String),
    NonStructMemberAccess(String),
    NoSuchMember(String, String),

    // Runtime errors
    UserError(String, Vec<String>),
    InexhaustiveMatch(Vec<String>),

    // Indicates a bug in the interpreter (should never happen!)
    InterpreterBug(String),
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
            FatalError::DuplicateDeclaration(name) => {
                format!("Duplicate declaration of \"{}\"", name)
            }
            FatalError::NoMainFunction => {
                "A main() function is required to run the program".to_string()
            }
            FatalError::NotInLoop => "A break or continue must be inside of a loop".to_string(),
            FatalError::MismatchedTypes(expected, got) => {
                format!("Mismatched types: expected {}, got {}", expected, got)
            }
            FatalError::InvalidCast(value, type_) => {
                format!("Cannot cast '{}' to '{}'", value, type_)
            }
            FatalError::NonPrimitiveCast => {
                "Only primitive types (int, float and bool) can be cast using 'as' expression"
                    .to_string()
            }
            FatalError::UndeclaredVariable(name) => format!("Undeclared variable '{}'", name),
            FatalError::NonStructMemberAccess(name) => {
                format!("Member access of non-struct variable '{}'", name)
            }
            FatalError::NoSuchMember(type_, member) => {
                format!("'{}' has no such member: '{}'", type_, member)
            }
            FatalError::UserError(msg, stack_trace) => {
                format!("{}\n\n{}", msg, Self::format_stack_trace(stack_trace))
            }
            FatalError::InexhaustiveMatch(stack_trace) => format!(
                "Inexhaustive match. Hint: use '_' to create a catch-all pattern.\n\n{}",
                Self::format_stack_trace(stack_trace)
            ),
            FatalError::InterpreterBug(msg) => format!("BUG: {}", msg),
        };

        if cfg!(test) {
            panic!("Error: {}", message.red().clear());
        } else {
            eprintln!("Error: {}", message.red().clear());
            process::exit(1);
        }
    }

    pub fn handle_result<T>(result: Result<T, FatalError>) -> T {
        match result {
            Ok(val) => val,
            Err(error_type) => Self::handle_error(error_type),
        }
    }

    fn format_stack_trace(stack_trace: Vec<String>) -> String {
        format!(
            "Stack trace:\n{}",
            stack_trace
                .iter()
                .map(|elem| format!("\t- {}", elem))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}
