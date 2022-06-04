//! Error handling module and error types

use std::{cmp::Ordering, process};

use colored::Colorize;
use utf8_read::StreamPosition;

use crate::data::{
    progtree::Syntax,
    runtime::Declaration,
    token::{Token, TokenKind},
};

pub enum FatalError {
    // Reader errors
    IoError(String),
    MalformedUtf8(StreamPosition),

    // Lexer errors
    InvalidEscapeChar(char, StreamPosition),
    UnexpectedCharacter(char, StreamPosition),
    LiteralOutOfBounds(StreamPosition),

    // Parser errors
    UnexpectedToken(Token, TokenKind),
    UnexpectedSyntax(Vec<Syntax>, Token),

    // Lexer/Parser errors
    UnexpectedEof(StreamPosition),

    // SemCheck errors
    DuplicateDeclaration(String),
    Undeclared(Declaration, String),
    NotInLoop,
    MismatchedTypes(Vec<String>, String),
    InvalidCast(String, String),
    NonPrimitiveCast,
    NonStructMemberAccess(String),
    NoSuchMember(String, String),
    InvalidNumberOfArgs(usize, isize, usize),
    OnlyBindingCatchAll,

    // Runtime errors
    RuntimeError(String, Vec<String>),
    InexhaustiveMatch(Vec<String>),
    DivideByZero,

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
                "Expected '{}', got '{}' instead [{}:{}]",
                kind,
                token.kind,
                token.position.line_position().0,
                token.position.line_position().1
            ),
            FatalError::UnexpectedSyntax(syntax, token) => {
                if syntax.len() == 1 {
                    format!(
                        "Expected {}, got '{}' [{}, {}]",
                        syntax[0],
                        token.kind,
                        token.position.line_position().0,
                        token.position.line_position().1
                    )
                } else {
                    format!(
                        "Expected one of: {}; got '{}' [{}, {}]",
                        syntax
                            .iter()
                            .map(|e| format!("{}", e))
                            .collect::<Vec<_>>()
                            .join(", "),
                        token.kind,
                        token.position.line_position().0,
                        token.position.line_position().1
                    )
                }
            }
            FatalError::DuplicateDeclaration(name) => {
                format!("Duplicate declaration of \"{}\"", name)
            }
            FatalError::Undeclared(decl, name) => format!("Undeclared {} '{}'", decl, name),
            FatalError::NotInLoop => "A break or continue must be inside of a loop".to_string(),
            FatalError::MismatchedTypes(expected, got) => {
                if expected.len() == 1 {
                    format!("Mismatched types: expected {}, got {}", expected[0], got)
                } else {
                    format!(
                        "Mismatched types: expected one of {}; got {}",
                        expected.join(", "),
                        got,
                    )
                }
            }
            FatalError::InvalidCast(value, type_) => {
                format!("Cannot cast '{}' to '{}'", value, type_)
            }
            FatalError::NonPrimitiveCast => {
                "Only primitive types (int, float and bool) can be cast using 'as' expression"
                    .to_string()
            }
            FatalError::NonStructMemberAccess(name) => {
                format!("Member access of non-struct variable '{}'", name)
            }
            FatalError::NoSuchMember(type_, member) => {
                format!("'{}' has no such member: '{}'", type_, member)
            }
            FatalError::InvalidNumberOfArgs(from, to, got) => match to.cmp(&0) {
                Ordering::Less => format!("Expected {} or more arguments, got {}", from, got),
                Ordering::Equal => format!("Expected {} arguments, got {}", from, got),
                Ordering::Greater => {
                    format!("Expected from {} to {} arguments, got {}", from, to, got)
                }
            },
            FatalError::OnlyBindingCatchAll => {
                "A binding or catch-all ('_') must be the only pattern alternative".to_string()
            }
            FatalError::RuntimeError(msg, stack_trace) => {
                format!("{}\n\n{}", msg, Self::format_stack_trace(stack_trace))
            }
            FatalError::InexhaustiveMatch(stack_trace) => format!(
                "Inexhaustive match. Hint: use '_' to create a catch-all pattern.\n\n{}",
                Self::format_stack_trace(stack_trace)
            ),
            FatalError::DivideByZero => "Integer division by zero".to_string(),
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
