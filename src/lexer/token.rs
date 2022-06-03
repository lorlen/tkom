//! Types defining the lexical structure of the language

use std::fmt::Display;

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

pub fn try_get_keyword(keyword: &str) -> Option<TokenKind> {
    match keyword {
        "if" => Some(TokenKind::If),
        "else" => Some(TokenKind::Else),
        "match" => Some(TokenKind::Match),
        "while" => Some(TokenKind::While),
        "for" => Some(TokenKind::For),
        "in" => Some(TokenKind::In),
        "fn" => Some(TokenKind::Fn),
        "return" => Some(TokenKind::Return),
        "yield" => Some(TokenKind::Yield),
        "struct" => Some(TokenKind::Struct),
        "enum" => Some(TokenKind::Enum),
        "let" => Some(TokenKind::Let),
        "mut" => Some(TokenKind::Mut),
        "const" => Some(TokenKind::Const),
        "as" => Some(TokenKind::As),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        _ => None,
    }
}

pub static OPERATOR_CHARS: &'static str = "+-*/%=!<>&|.";
