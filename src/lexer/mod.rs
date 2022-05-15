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
    lexer::token::{NumberType, Token, TokenKind, KEYWORDS, OPERATOR_CHARS},
};

pub trait Lexer: Iterator<Item = Token> {
    fn curr_pos(&self) -> &StreamPosition;
    fn curr_token(&self) -> &Option<Token>;
}

pub struct LexerImpl {
    reader: Reader<Box<dyn Read>>,
    curr_char: char,
    curr_pos: StreamPosition,
    curr_token: Option<Token>,
}

impl LexerImpl {
    pub fn new(reader: Reader<Box<dyn Read>>) -> LexerImpl {
        let pos = *reader.borrow_pos();
        LexerImpl {
            reader,
            curr_char: ' ',
            curr_pos: pos,
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

        match char {
            Char::Char(c) => {
                self.curr_char = c;
                Some(c)
            }
            _ => None,
        }
    }

    fn next_char_eof(&mut self) -> char {
        match self.next_char() {
            Some(c) => c,
            None => {
                ErrorHandler::handle_error(FatalError::UnexpectedEof(*self.reader.borrow_pos()))
            }
        }
    }

    fn ignore_whitespace(&mut self) -> Option<()> {
        if self.curr_char.is_whitespace() {
            while self.next_char()?.is_whitespace() {}
        }
        Some(())
    }

    fn try_build_operator_or_comment(&mut self) -> Option<Token> {
        let mut needs_to_read_next = true;

        let token = match self.curr_char {
            '(' => Some(self.token(TokenKind::ParenOpen)),
            ')' => Some(self.token(TokenKind::ParenClose)),
            '{' => Some(self.token(TokenKind::BracketOpen)),
            '}' => Some(self.token(TokenKind::BracketClose)),
            '[' => Some(self.token(TokenKind::SqBracketOpen)),
            ']' => Some(self.token(TokenKind::SqBracketClose)),
            ',' => Some(self.token(TokenKind::Comma)),
            ';' => Some(self.token(TokenKind::Semicolon)),

            '&' => Some(self.token(TokenKind::And)),
            '|' => Some(self.token(TokenKind::Or)),

            '.' => match self.next_char() {
                Some('.') => Some(self.token(TokenKind::Range)),
                _ => {
                    needs_to_read_next = false;
                    Some(self.token(TokenKind::Dot))
                }
            },

            '+' | '-' | '*' | '/' | '%' | '<' | '>' | '!' | '=' => {
                let first_char = self.curr_char;

                match self.next_char() {
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
                    Some(c) if OPERATOR_CHARS.contains(&c) => ErrorHandler::handle_error(
                        FatalError::UnexpectedCharacter(c, self.curr_pos),
                    ),
                    _ => {
                        needs_to_read_next = false;
                        match first_char {
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
                        }
                    }
                }
            }

            _ => {
                needs_to_read_next = false;
                None
            }
        };

        if needs_to_read_next {
            self.next_char();
        }

        token
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

        if self.curr_char == '_' || self.curr_char.is_alphabetic() {
            content.push(self.curr_char);
            let mut char = self.next_char().unwrap_or(' ');

            while char == '_' || char.is_alphanumeric() {
                content.push(char);
                char = self.next_char().unwrap_or(' ');
            }

            match KEYWORDS.get(&content[..]) {
                Some(kind) => Some(self.token(kind.clone())),
                None => Some(self.token(TokenKind::Identifier(content))),
            }
        } else {
            None
        }
    }

    fn try_build_string(&mut self) -> Option<Token> {
        let token = match self.curr_char {
            '"' => {
                let mut content = String::new();
                let mut char = self.next_char_eof();

                loop {
                    match char {
                        '\\' => {
                            let escape_char = self.next_char_eof();
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
                        '"' => break,
                        _ => content.push(char),
                    }

                    char = self.next_char_eof();
                }

                Some(self.token(TokenKind::String(content)))
            }
            _ => None,
        };

        if token.is_some() {
            self.next_char();
        }

        token
    }

    fn try_build_number(&mut self) -> Option<Token> {
        let mut unsigned = 0u64;
        let mut float = 0.0;
        let mut is_float = false;
        let mut float_counter = 1;

        if self.curr_char.is_ascii_digit() {
            unsigned = unsigned * 10 + (self.curr_char as u64 - '0' as u64);
        } else {
            return None;
        }

        while self.next_char().is_some() {
            if self.curr_char.is_ascii_digit() {
                if !is_float {
                    match unsigned
                        .checked_mul(10)
                        .and_then(|val| val.checked_add(self.curr_char as u64 - '0' as u64))
                    {
                        Some(val) => unsigned = val,
                        None => ErrorHandler::handle_error(FatalError::LiteralOutOfBounds(
                            self.curr_pos,
                        )),
                    }
                } else {
                    float +=
                        (self.curr_char as u64 - '0' as u64) as f64 * (10f64).powi(-float_counter);
                    float_counter += 1;
                }
            } else if !is_float && self.curr_char == '.' {
                float = unsigned as f64;
                is_float = true;
            } else {
                break;
            }
        }

        match is_float {
            true => Some(self.token(TokenKind::Number(NumberType::Float(float)))),
            false => Some(self.token(TokenKind::Number(NumberType::Integer(unsigned)))),
        }
    }
}

impl Iterator for LexerImpl {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ignore_whitespace().is_none() || self.reader.eof() {
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
            None => ErrorHandler::handle_error(FatalError::UnexpectedCharacter(
                self.curr_char,
                self.curr_pos,
            )),
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
