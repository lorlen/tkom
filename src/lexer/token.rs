//! Types defining the lexical structure of the language

use std::{collections::HashMap, fmt::Display};

use lazy_static::lazy_static;
use utf8_read::StreamPosition;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberType {
    Integer(u64),
    Float(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Literals
    Number(NumberType),
    String(String),
    Identifier(String),

    // Comments
    Comment(String),

    // Arithmetic operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    // Relational operators
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,

    // Boolean operators
    And,
    Or,
    Not,

    // Assignment operators
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,

    // Cast operator
    As,

    // Punctuation
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    SqBracketOpen,
    SqBracketClose,
    Dot,
    Comma,
    Semicolon,
    ThinArrow,

    // Language constructs
    If,
    Else,
    Match,
    While,
    For,
    In,
    Fn,
    Return,
    Yield,
    Struct,
    Enum,
    Let,
    Mut,
    Const,
    Range,

    // Keyword literals
    True,
    False,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Number(_) => write!(f, "Number"),
            Self::String(_) => write!(f, "String"),
            Self::Identifier(_) => write!(f, "Identifier"),
            Self::Comment(_) => write!(f, "Comment"),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub position: StreamPosition,
}

impl Token {
    pub fn new(kind: TokenKind, position: StreamPosition) -> Token {
        Token { kind, position }
    }
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenKind> = HashMap::from([
        ("if", TokenKind::If),
        ("else", TokenKind::Else),
        ("match", TokenKind::Match),
        ("while", TokenKind::While),
        ("for", TokenKind::For),
        ("in", TokenKind::In),
        ("fn", TokenKind::Fn),
        ("return", TokenKind::Return),
        ("yield", TokenKind::Yield),
        ("struct", TokenKind::Struct),
        ("enum", TokenKind::Enum),
        ("let", TokenKind::Let),
        ("mut", TokenKind::Mut),
        ("const", TokenKind::Const),
        ("as", TokenKind::As),
        ("true", TokenKind::True),
        ("false", TokenKind::False),
    ]);
}

pub static OPERATOR_CHARS: &'static str = "+-*/%=!<>&|.";
