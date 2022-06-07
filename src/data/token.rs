//! Types defining the lexical structure of the language

use std::fmt::Display;

use utf8_read::StreamPosition;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberType {
    Integer(i64),
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
    Fn,
    Struct,
    Enum,
    Const,
    Let,
    In,
    Range,

    // Control flow constructs
    If,
    Else,
    Match,
    While,
    For,
    Return,
    Yield,
    Break,
    Continue,

    // Keyword literals
    True,
    False,
    None,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Number(n) => match n {
                NumberType::Integer(i) => write!(f, "{}", i),
                NumberType::Float(fl) => write!(f, "{}", fl),
            },
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Identifier(i) => write!(f, "{}", i),
            Self::Comment(_) => write!(f, "comment"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThan => write!(f, "<"),
            Self::GreaterEqual => write!(f, ">="),
            Self::LessEqual => write!(f, "<="),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Not => write!(f, "!"),
            Self::Assign => write!(f, "="),
            Self::PlusAssign => write!(f, "+="),
            Self::MinusAssign => write!(f, "-="),
            Self::MultiplyAssign => write!(f, "*="),
            Self::DivideAssign => write!(f, "/="),
            Self::ModuloAssign => write!(f, "%="),
            Self::As => write!(f, "as"),
            Self::ParenOpen => write!(f, "("),
            Self::ParenClose => write!(f, ")"),
            Self::BracketOpen => write!(f, "{{"),
            Self::BracketClose => write!(f, "}}"),
            Self::SqBracketOpen => write!(f, "["),
            Self::SqBracketClose => write!(f, "]"),
            Self::Dot => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::ThinArrow => write!(f, "->"),
            Self::Fn => write!(f, "fn"),
            Self::Struct => write!(f, "struct"),
            Self::Enum => write!(f, "enum"),
            Self::Const => write!(f, "const"),
            Self::Let => write!(f, "let"),
            Self::In => write!(f, "in"),
            Self::Range => write!(f, "range"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Match => write!(f, "match"),
            Self::While => write!(f, "while"),
            Self::For => write!(f, "for"),
            Self::Return => write!(f, "return"),
            Self::Yield => write!(f, "yield"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::None => write!(f, "none"),
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
        "const" => Some(TokenKind::Const),
        "as" => Some(TokenKind::As),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "break" => Some(TokenKind::Break),
        "continue" => Some(TokenKind::Continue),
        "none" => Some(TokenKind::None),
        _ => None,
    }
}

pub static OPERATOR_CHARS: &str = "+-*/%=!<>&|.";
