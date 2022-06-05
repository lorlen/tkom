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
    TopLevelDuplicateDeclaration(String),

    // Lexer/Parser errors
    UnexpectedEof(StreamPosition),

    // SemCheck errors
    DuplicateDeclaration(String, Vec<String>),
    Undeclared(Declaration, String, Vec<String>),
    NotInLoop(Vec<String>),
    MismatchedTypes(Vec<String>, String, Vec<String>),
    InvalidCast(String, String, Vec<String>),
    NonPrimitiveCast(Vec<String>),
    NonStructMemberAccess(String, Vec<String>),
    NoSuchMember(String, String, Vec<String>),
    InvalidNumberOfArgs(usize, isize, usize, Vec<String>),
    OnlyBindingCatchAll(Vec<String>),

    // Runtime errors
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
            FatalError::TopLevelDuplicateDeclaration(name) => {
                format!("Duplicate declaration of \"{}\"", name)
            }
            FatalError::DuplicateDeclaration(name, stack_trace) => {
                format!("Duplicate declaration of \"{}\"\n\n{}", name, Self::format_stack_trace(stack_trace))
            }
            FatalError::Undeclared(decl, name, stack_trace) => format!("Undeclared {} '{}'\n\n{}", decl, name, Self::format_stack_trace(stack_trace)),
            FatalError::NotInLoop(stack_trace) => format!("A break or continue must be inside of a loop\n\n{}", Self::format_stack_trace(stack_trace)),
            FatalError::MismatchedTypes(expected, got, stack_trace) => {
                if expected.len() == 1 {
                    format!("Mismatched types: expected {}, got {}\n\n{}", expected[0], got, Self::format_stack_trace(stack_trace))
                } else {
                    format!(
                        "Mismatched types: expected one of {}; got {}\n\n{}",
                        expected.join(", "),
                        got,
                        Self::format_stack_trace(stack_trace),
                    )
                }
            }
            FatalError::InvalidCast(value, type_, stack_trace) => {
                format!("Cannot cast '{}' to '{}'\n\n{}", value, type_, Self::format_stack_trace(stack_trace))
            }
            FatalError::NonPrimitiveCast(stack_trace) => format!(
                "Only primitive types (int, float and bool) can be cast using 'as' expression\n\n{}",
                Self::format_stack_trace(stack_trace)
            ),
            FatalError::NonStructMemberAccess(name, stack_trace) => {
                format!("Member access of non-struct variable '{}'\n\n{}", name, Self::format_stack_trace(stack_trace))
            }
            FatalError::NoSuchMember(type_, member, stack_trace) => {
                format!("'{}' has no such member: '{}'\n\n{}", type_, member, Self::format_stack_trace(stack_trace))
            }
            FatalError::InvalidNumberOfArgs(from, to, got, stack_trace) => match to.cmp(&0) {
                Ordering::Less => format!("Expected {} or more arguments, got {}\n\n{}", from, got, Self::format_stack_trace(stack_trace)),
                Ordering::Equal => format!("Expected {} arguments, got {}\n\n{}", from, got, Self::format_stack_trace(stack_trace)),
                Ordering::Greater => {
                    format!("Expected from {} to {} arguments, got {}\n\n{}", from, to, got, Self::format_stack_trace(stack_trace))
                }
            },
            FatalError::OnlyBindingCatchAll(stack_trace) => format!(
                "A binding or catch-all ('_') must be the only pattern alternative\n\n{}",
                Self::format_stack_trace(stack_trace),
            ),
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
