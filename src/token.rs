//! Types defining the lexical structure of the language

use std::collections::{HashMap, HashSet};

use lazy_static::lazy_static;
use utf8_read::StreamPosition;

#[derive(Clone, Debug, PartialEq)]
pub enum NumberType {
    Signed(i64),
    Unsigned(u64),
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
    FatArrow,

    // Language constructs
    If,
    Match,
    While,
    For,
    Fn,
    Return,
    Struct,
    Enum,
    Mut,
    Range,

    // Keyword literals
    True,
    False,
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenKind> = HashMap::from([
        ("if", TokenKind::If),
        ("match", TokenKind::Match),
        ("while", TokenKind::While),
        ("for", TokenKind::For),
        ("fn", TokenKind::Fn),
        ("return", TokenKind::Return),
        ("struct", TokenKind::Struct),
        ("enum", TokenKind::Enum),
        ("mut", TokenKind::Mut),
        ("true", TokenKind::True),
        ("false", TokenKind::False),
    ]);
    pub static ref OPERATOR_CHARS: HashSet<char> =
        HashSet::from(['+', '-', '*', '/', '%', '=', '!', '>', '<', '&', '|', '.']);
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
