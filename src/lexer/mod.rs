//! The lexer module, being in charge of tokenizing the input stream.
//!
//! Some technical remarks:
//! - EOF, contrary to its abbreviation, means an end of stream, regardless
//!   of its source.
//! - there are two kinds of EOFs: expected and unexpected. Expected just cause
//!   the lexer to gracefully stop producing new tokens. They generally occur
//!   on token boundaries and in the whitespace. Unexpected EOFs occur inside
//!   of tokens, and cause a fatal error to be reported.

mod tests;
pub(crate) mod token;

use std::io::Read;

use utf8_read::{Char, Reader, StreamPosition};

use crate::{
    error::{ErrorHandler, FatalError},
    lexer::token::{try_get_keyword, NumberType, Token, TokenKind, OPERATOR_CHARS},
};

pub trait Lexer: Iterator<Item = Token> {
    fn curr_pos(&self) -> &StreamPosition;
    fn curr_token(&self) -> &Option<Token>;
}

pub struct LexerImpl {
    reader: Reader<Box<dyn Read>>,
    curr_pos: StreamPosition,
    curr_char: Option<char>,
    curr_token: Option<Token>,
}

impl LexerImpl {
    pub fn new(reader: Reader<Box<dyn Read>>) -> LexerImpl {
        let pos = *reader.borrow_pos();
        LexerImpl {
            reader,
            curr_pos: pos,
            curr_char: Some(' '),
            curr_token: None,
        }
    }

    fn token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.curr_pos)
    }

    fn next_char(&mut self) -> Option<char> {
        let char = ErrorHandler::handle_result(self.reader.next_char().map_err(|err| match err {
            utf8_read::Error::IoError(error) => FatalError::IoError(error.to_string()),
            utf8_read::Error::MalformedUtf8(position, _) => FatalError::MalformedUtf8(position),
        }));

        let char_opt = match char {
            Char::Char(c) => Some(c),
            _ => None,
        };

        self.curr_char = char_opt;
        char_opt
    }

    fn next_char_ensure_no_eof(&mut self) -> char {
        match self.next_char() {
            Some(c) => c,
            None => {
                ErrorHandler::handle_error(FatalError::UnexpectedEof(*self.reader.borrow_pos()))
            }
        }
    }

    fn check_and_consume(&mut self, chars: &str) -> Option<char> {
        match self.curr_char {
            Some(c) if chars.contains(c) => {
                self.next_char();
                Some(c)
            }
            _ => None,
        }
    }

    fn ignore_whitespace(&mut self) {
        while matches!(self.curr_char, Some(c) if c.is_whitespace()) {
            self.next_char();
        }
    }

    fn try_build_operator_or_comment(&mut self) -> Option<Token> {
        match self.check_and_consume("(){}[],;&|.+-*/%<>!=") {
            Some('(') => Some(self.token(TokenKind::ParenOpen)),
            Some(')') => Some(self.token(TokenKind::ParenClose)),
            Some('{') => Some(self.token(TokenKind::BracketOpen)),
            Some('}') => Some(self.token(TokenKind::BracketClose)),
            Some('[') => Some(self.token(TokenKind::SqBracketOpen)),
            Some(']') => Some(self.token(TokenKind::SqBracketClose)),
            Some(',') => Some(self.token(TokenKind::Comma)),
            Some(';') => Some(self.token(TokenKind::Semicolon)),

            Some('&') => Some(self.token(TokenKind::And)),
            Some('|') => Some(self.token(TokenKind::Or)),

            Some('.') => match self.check_and_consume(".") {
                Some('.') => Some(self.token(TokenKind::Range)),
                _ => Some(self.token(TokenKind::Dot)),
            },

            Some(first_char) if "+-*/%<>!=".contains(first_char) => {
                match self.check_and_consume(OPERATOR_CHARS) {
                    Some('/') if first_char == '/' => self.try_build_comment(),
                    Some('>') if first_char == '-' => Some(self.token(TokenKind::ThinArrow)),
                    Some('=') => match first_char {
                        '+' => Some(self.token(TokenKind::PlusAssign)),
                        '-' => Some(self.token(TokenKind::MinusAssign)),
                        '*' => Some(self.token(TokenKind::MultiplyAssign)),
                        '/' => Some(self.token(TokenKind::DivideAssign)),
                        '%' => Some(self.token(TokenKind::ModuloAssign)),
                        '<' => Some(self.token(TokenKind::LessEqual)),
                        '>' => Some(self.token(TokenKind::GreaterEqual)),
                        '!' => Some(self.token(TokenKind::NotEqual)),
                        '=' => Some(self.token(TokenKind::Equal)),
                        _ => unreachable!(),
                    },
                    Some(c) => ErrorHandler::handle_error(FatalError::UnexpectedCharacter(
                        c,
                        self.curr_pos,
                    )),
                    _ => match first_char {
                        '+' => Some(self.token(TokenKind::Plus)),
                        '-' => Some(self.token(TokenKind::Minus)),
                        '*' => Some(self.token(TokenKind::Multiply)),
                        '/' => Some(self.token(TokenKind::Divide)),
                        '%' => Some(self.token(TokenKind::Modulo)),
                        '<' => Some(self.token(TokenKind::LessThan)),
                        '>' => Some(self.token(TokenKind::GreaterThan)),
                        '!' => Some(self.token(TokenKind::Not)),
                        '=' => Some(self.token(TokenKind::Assign)),
                        _ => unreachable!(),
                    },
                }
            }

            _ => None,
        }
    }

    fn try_build_comment(&mut self) -> Option<Token> {
        let mut content = String::new();
        let mut content_char = self.next_char().unwrap_or('\n');

        while content_char != '\n' && content_char != '\r' {
            content.push(content_char);
            content_char = self.next_char().unwrap_or('\n');
        }

        Some(self.token(TokenKind::Comment(content.trim().to_owned())))
    }

    fn try_build_identifier_or_keyword(&mut self) -> Option<Token> {
        let mut content = String::new();

        match self.curr_char {
            Some(c) if c == '_' || c.is_alphabetic() => {
                content.push(c);
                let mut char = self.next_char().unwrap_or(' ');

                while char == '_' || char.is_alphanumeric() {
                    content.push(char);
                    char = self.next_char().unwrap_or(' ');
                }

                Some(
                    self.token(
                        try_get_keyword(&content[..]).unwrap_or(TokenKind::Identifier(content)),
                    ),
                )
            }
            _ => None,
        }
    }

    fn try_build_string(&mut self) -> Option<Token> {
        match self.curr_char {
            Some('"') => {
                let mut content = String::new();
                let mut char = self.next_char_ensure_no_eof();

                while char != '"' {
                    match char {
                        '\\' => {
                            let escape_char = self.next_char_ensure_no_eof();
                            match escape_char {
                                'n' => content.push('\n'),
                                'r' => content.push('\r'),
                                't' => content.push('\t'),
                                '\\' => content.push('\\'),
                                '0' => content.push('\0'),
                                '"' => content.push('"'),
                                _ => ErrorHandler::handle_error(FatalError::InvalidEscapeChar(
                                    escape_char,
                                    *self.reader.borrow_pos(),
                                )),
                            }
                        }
                        _ => content.push(char),
                    }

                    char = self.next_char_ensure_no_eof();
                }

                self.next_char();
                Some(self.token(TokenKind::String(content)))
            }
            _ => None,
        }
    }

    fn try_build_number(&mut self) -> Option<Token> {
        match self.curr_char {
            Some(c) if c.is_ascii_digit() => {
                let integer = self.build_integer();

                match self.curr_char {
                    Some('.') => {
                        self.next_char();
                        let float = self.build_float(integer);
                        Some(self.token(TokenKind::Number(NumberType::Float(float))))
                    }
                    Some(c) if c.is_alphabetic() => ErrorHandler::handle_error(
                        FatalError::UnexpectedCharacter(c, *self.reader.borrow_pos()),
                    ),
                    _ => Some(self.token(TokenKind::Number(NumberType::Integer(integer)))),
                }
            }
            _ => None,
        }
    }

    fn build_integer(&mut self) -> u64 {
        let mut value = 0u64;

        while let Some(c) = self.check_and_consume("0123456789") {
            match value
                .checked_mul(10)
                .and_then(|new_val| new_val.checked_add(c as u64 - '0' as u64))
            {
                Some(new_val) => value = new_val,
                None => ErrorHandler::handle_error(FatalError::LiteralOutOfBounds(self.curr_pos)),
            }
        }

        value
    }

    fn build_float(&mut self, integer_part: u64) -> f64 {
        let mut value = integer_part as f64;
        let mut float_counter = 1;

        while let Some(c) = self.check_and_consume("0123456789") {
            value += (c as u64 - '0' as u64) as f64 * (10f64).powi(-float_counter);
            float_counter += 1;
        }

        value
    }
}

impl Iterator for LexerImpl {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.ignore_whitespace();

        if self.reader.eof() {
            self.curr_token = None;
            return None;
        }

        self.curr_pos = *self.reader.borrow_pos();

        self.curr_token = self
            .try_build_identifier_or_keyword()
            .or_else(|| self.try_build_string())
            .or_else(|| self.try_build_number())
            .or_else(|| self.try_build_operator_or_comment());

        match self.curr_token.clone() {
            Some(token) => Some(token),
            None => match self.curr_char {
                Some(c) => {
                    ErrorHandler::handle_error(FatalError::UnexpectedCharacter(c, self.curr_pos))
                }
                _ => {
                    ErrorHandler::handle_error(FatalError::UnexpectedEof(*self.reader.borrow_pos()))
                }
            },
        }
    }
}

impl Lexer for LexerImpl {
    fn curr_pos(&self) -> &StreamPosition {
        &self.curr_pos
    }

    fn curr_token(&self) -> &Option<Token> {
        &self.curr_token
    }
}
