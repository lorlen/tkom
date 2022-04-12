//! The lexer module, being in charge of tokenizing the input stream.
//!
//! Some technical remarks:
//! - EOF, contrary to its abbreviation, means an end of stream, regardless
//!   of its source.
//! - there are two kinds of EOFs: expected and unexpected. Expected just cause
//!   the lexer to gracefully stop producing new tokens. They generally occur
//!   on token boundaries and in the whitespace. Unexpected EOFs occur inside
//!   of tokens, and cause a fatal error to be reported.

use std::io::Read;

use utf8_read::{Char, Reader, StreamPosition};

use crate::{
    error::{ErrorHandler, FatalError},
    token::{NumberType, Token, TokenKind, KEYWORDS},
};

pub struct Lexer {
    reader: Reader<Box<dyn Read>>,
    curr_char: char,
    curr_pos: StreamPosition,
}

impl Lexer {
    pub fn new(reader: Reader<Box<dyn Read>>) -> Lexer {
        let pos = *reader.borrow_pos();
        Lexer {
            reader,
            curr_char: '\0',
            curr_pos: pos,
        }
    }

    pub fn curr_pos(&self) -> &StreamPosition {
        &self.curr_pos
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
        while self.next_char()?.is_whitespace() {}
        Some(())
    }

    fn try_build_operator(&mut self, first_char: char) -> Option<Token> {
        match first_char {
            '(' => Some(self.token(TokenKind::ParenOpen)),
            ')' => Some(self.token(TokenKind::ParenClose)),
            '{' => Some(self.token(TokenKind::BracketOpen)),
            '}' => Some(self.token(TokenKind::BracketClose)),
            '[' => Some(self.token(TokenKind::SqBracketOpen)),
            ']' => Some(self.token(TokenKind::SqBracketClose)),

            '&' | '|' => {
                if first_char == self.curr_char {
                    match first_char {
                        '&' => Some(self.token(TokenKind::And)),
                        '|' => Some(self.token(TokenKind::Or)),
                        _ => unreachable!(),
                    }
                } else {
                    None
                }
            }

            '+' | '-' | '*' | '/' | '%' | '<' | '>' | '!' | '=' => match self.curr_char {
                '=' => match first_char {
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
            },

            _ => None,
        }
    }

    fn try_build_comment(&mut self, first_char: char) -> Option<Token> {
        match first_char {
            '/' => match self.curr_char {
                '/' => {
                    let mut content = String::new();
                    let mut content_char = self.next_char().unwrap_or('\n');

                    while content_char != '\n' && content_char != '\r' {
                        content.push(content_char);
                        content_char = self.next_char().unwrap_or('\n');
                    }

                    Some(self.token(TokenKind::Comment(content.trim().to_owned())))
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn try_build_identifier_or_keyword(&mut self, first_char: char) -> Option<Token> {
        let mut content = String::new();

        if first_char == '_' || first_char.is_alphabetic() {
            content.push(first_char);
            let mut char = self.curr_char;

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

    fn try_build_string(&mut self, first_char: char) -> Option<Token> {
        match first_char {
            '"' => {
                let mut content = String::new();
                let mut char = self.curr_char;

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
                                _ => ErrorHandler::handle_error(FatalError::SyntaxError(
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
        }
    }

    fn try_build_number(&mut self, first_char: char) -> Option<Token> {
        let mut unsigned = 0u64;
        let mut float = 0.0;
        let mut sign = 1;
        let mut is_float = false;
        let mut float_counter = 1;
        let mut char = self.curr_char;

        match first_char {
            '+' | '-' | '.' => {
                if !char.is_ascii_digit() {
                    ErrorHandler::handle_error(FatalError::SyntaxError(*self.reader.borrow_pos()))
                }
                match first_char {
                    '+' => (),
                    '-' => sign = -1,
                    '.' => is_float = true,
                    _ => unreachable!(),
                }
            }
            digit if digit.is_ascii_digit() => {
                unsigned = unsigned * 10 + (digit as u64 - '0' as u64)
            }
            _ => return None,
        }

        loop {
            if char.is_ascii_digit() {
                if !is_float {
                    match unsigned
                        .checked_mul(10)
                        .and_then(|val| val.checked_add(char as u64 - '0' as u64))
                    {
                        Some(val) => unsigned = val,
                        None => {
                            ErrorHandler::handle_error(FatalError::ValueOutOfBounds(self.curr_pos))
                        }
                    }
                } else {
                    float += (char as u64 - '0' as u64) as f64 * (10f64).powi(-float_counter);
                    float_counter += 1;
                }
            } else if !is_float && char == '.' {
                float = unsigned as f64;
                is_float = true;
            } else {
                break;
            }

            match self.next_char() {
                Some(c) => char = c,
                None => break,
            }
        }

        if is_float {
            Some(self.token(TokenKind::Number(NumberType::Float(float))))
        } else if sign == -1 {
            match unsigned
                .try_into()
                .ok()
                .and_then(|val: i64| val.checked_mul(sign))
            {
                Some(val) => Some(self.token(TokenKind::Number(NumberType::Signed(val)))),
                None => ErrorHandler::handle_error(FatalError::ValueOutOfBounds(self.curr_pos)),
            }
        } else {
            Some(self.token(TokenKind::Number(NumberType::Unsigned(unsigned))))
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.ignore_whitespace()?;

        self.curr_pos = *self.reader.borrow_pos();

        let first_char = self.curr_char;
        // TODO: what to do, when EOF here? It may be that we don't need the second character,
        // so just erroring here would be a bad idea
        self.next_char();

        let result = self
            .try_build_comment(first_char)
            .or_else(|| self.try_build_operator(first_char))
            .or_else(|| self.try_build_identifier_or_keyword(first_char))
            .or_else(|| self.try_build_string(first_char))
            .or_else(|| self.try_build_number(first_char));

        match result {
            Some(token) => Some(token),
            None => ErrorHandler::handle_error(FatalError::SyntaxError(self.curr_pos)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lexer_from_str(s: &'static str) -> Lexer {
        Lexer::new(Reader::new(Box::new(s.as_bytes())))
    }

    #[test]
    fn test_ignore_whitespace() {
        let mut lexer = lexer_from_str("   a");
        assert_eq!(lexer.ignore_whitespace(), Some(()));
        assert_eq!(lexer.curr_char, 'a');
    }

    #[test]
    fn test_operator() {
        let lexer = lexer_from_str("+ += &&");
        assert_eq!(
            lexer.map(|elem| elem.kind).collect::<Vec<TokenKind>>(),
            vec![TokenKind::Plus, TokenKind::PlusAssign, TokenKind::And]
        )
    }

    #[test]
    fn test_comment() {
        let mut lexer = lexer_from_str("// comment");
        let token = lexer.next();
        assert!(
            matches!(token, Some(Token { kind: TokenKind::Comment(content), .. }) if content == "comment")
        )
    }

    #[test]
    fn test_keywords() {
        let lexer = lexer_from_str("if match while for fn return struct enum mut");
        assert_eq!(
            lexer.map(|elem| elem.kind).collect::<Vec<TokenKind>>(),
            vec![
                TokenKind::If,
                TokenKind::Match,
                TokenKind::While,
                TokenKind::For,
                TokenKind::Fn,
                TokenKind::Return,
                TokenKind::Struct,
                TokenKind::Enum,
                TokenKind::Mut
            ]
        )
    }

    #[test]
    fn test_identifiers() {
        let lexer = lexer_from_str("asdf _asdf asdf123 ___123asdf");
        assert_eq!(
            lexer.map(|elem| elem.kind).collect::<Vec<TokenKind>>(),
            vec![
                TokenKind::Identifier("asdf".to_owned()),
                TokenKind::Identifier("_asdf".to_owned()),
                TokenKind::Identifier("asdf123".to_owned()),
                TokenKind::Identifier("___123asdf".to_owned()),
            ]
        )
    }

    #[test]
    fn test_strings() {
        let mut lexer = lexer_from_str("  \"abc \\\"def\\\" 123\\n\\r\\t\"   ");
        let token_content = match lexer.next() {
            Some(Token {
                kind: TokenKind::String(content),
                ..
            }) => content,
            _ => panic!(),
        };
        assert_eq!(token_content, "abc \"def\" 123\n\r\t");
    }

    #[test]
    fn test_numbers() {
        let mut lexer = lexer_from_str("123.456");
        let token_content = match lexer.next() {
            Some(Token {
                kind: TokenKind::Number(NumberType::Float(content)),
                ..
            }) => content,
            _ => panic!(),
        };
        assert!((token_content - 123.456).abs() < 1e-10)
    }
}
