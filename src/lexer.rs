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
    token::{NumberType, Token, TokenKind, KEYWORDS, OPERATOR_CHARS},
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
            curr_char: ' ',
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
                    Some('>') if first_char == '=' => Some(self.token(TokenKind::FatArrow)),
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
                                _ => ErrorHandler::handle_error(FatalError::SyntaxError {
                                    pos: *self.reader.borrow_pos(),
                                    expected: "an escape character".to_owned(),
                                    got: format!("'{}'", escape_char),
                                }),
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

        if let Some(_) = token {
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

        while let Some(_) = self.next_char() {
            if self.curr_char.is_ascii_digit() {
                if !is_float {
                    match unsigned
                        .checked_mul(10)
                        .and_then(|val| val.checked_add(self.curr_char as u64 - '0' as u64))
                    {
                        Some(val) => unsigned = val,
                        None => {
                            ErrorHandler::handle_error(FatalError::ValueOutOfBounds(self.curr_pos))
                        }
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
            false => Some(self.token(TokenKind::Number(NumberType::Unsigned(unsigned)))),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.ignore_whitespace()?;

        if self.reader.eof() {
            return None;
        }

        self.curr_pos = *self.reader.borrow_pos();

        let result = self
            .try_build_identifier_or_keyword()
            .or_else(|| self.try_build_string())
            .or_else(|| self.try_build_number())
            .or_else(|| self.try_build_operator_or_comment());

        match result {
            Some(token) => Some(token),
            None => ErrorHandler::handle_error(FatalError::UnexpectedCharacter(
                self.curr_char,
                self.curr_pos,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;

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
        let lexer = lexer_from_str("+ += ..");
        assert_eq!(
            lexer.map(|elem| elem.kind).collect::<Vec<TokenKind>>(),
            vec![TokenKind::Plus, TokenKind::PlusAssign, TokenKind::Range]
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
        let lexer = lexer_from_str("if match while for fn return struct enum mut true false");
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
                TokenKind::Mut,
                TokenKind::True,
                TokenKind::False,
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

    #[test]
    fn test_file() {
        let lexer = Lexer::new(Reader::new(Box::new(File::open("snippet.txt").unwrap())));
        assert_eq!(
            lexer.map(|token| token.kind).collect::<Vec<_>>(),
            vec![
                TokenKind::Comment("comment".to_owned()),
                TokenKind::Comment("constant".to_owned()),
                TokenKind::Identifier("const".to_owned()),
                TokenKind::Identifier("u64".to_owned()),
                TokenKind::Identifier("SOME_CONSTANT".to_owned()),
                TokenKind::Assign,
                TokenKind::Number(NumberType::Unsigned(1)),
                TokenKind::Semicolon,
                TokenKind::Comment("struct".to_owned()),
                TokenKind::Struct,
                TokenKind::Identifier("SomeStruct".to_owned()),
                TokenKind::BracketOpen,
                TokenKind::Comment("some primitive types".to_owned()),
                TokenKind::Identifier("i64".to_owned()),
                TokenKind::Identifier("field1".to_owned()),
                TokenKind::Comma,
                TokenKind::Identifier("u64".to_owned()),
                TokenKind::Identifier("field2".to_owned()),
                TokenKind::Comma,
                TokenKind::Identifier("f64".to_owned()),
                TokenKind::Identifier("field3".to_owned()),
                TokenKind::Comma,
                TokenKind::Identifier("string".to_owned()),
                TokenKind::Identifier("field4".to_owned()),
                TokenKind::Comma,
                TokenKind::Comment("trailing comma should work".to_owned()),
                TokenKind::BracketClose,
                TokenKind::Comment("enum (tagged union)".to_owned()),
                TokenKind::Enum,
                TokenKind::Identifier("SomeEnum".to_owned()),
                TokenKind::BracketOpen,
                TokenKind::Identifier("EmptyVariant".to_owned()),
                TokenKind::Comma,
                TokenKind::Identifier("PrimitiveVariant".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::Identifier("u64".to_owned()),
                TokenKind::ParenClose,
                TokenKind::Comma,
                TokenKind::Identifier("StructVariant".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::Identifier("SomeStruct".to_owned()),
                TokenKind::ParenClose,
                TokenKind::Comma,
                TokenKind::BracketClose,
                TokenKind::Comment("function definition".to_owned()),
                TokenKind::Comment("main - entry point, required".to_owned()),
                TokenKind::Fn,
                TokenKind::Identifier("main".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::ParenClose,
                TokenKind::BracketOpen,
                TokenKind::Comment("immutable variable".to_owned()),
                TokenKind::Identifier("u64".to_owned()),
                TokenKind::Identifier("var1".to_owned()),
                TokenKind::Assign,
                TokenKind::Number(NumberType::Unsigned(1)),
                TokenKind::Semicolon,
                TokenKind::Comment("mutable variable".to_owned()),
                TokenKind::Mut,
                TokenKind::Identifier("u64".to_owned()),
                TokenKind::Identifier("var2".to_owned()),
                TokenKind::Assign,
                TokenKind::Number(NumberType::Unsigned(1)),
                TokenKind::Semicolon,
                TokenKind::Comment("if expression".to_owned()),
                TokenKind::Identifier("string".to_owned()),
                TokenKind::Identifier("var3".to_owned()),
                TokenKind::Assign,
                TokenKind::If,
                TokenKind::Identifier("var1".to_owned()),
                TokenKind::Equal,
                TokenKind::Number(NumberType::Unsigned(1)),
                TokenKind::BracketOpen,
                TokenKind::String("var1 is 1".to_owned()),
                TokenKind::BracketClose,
                TokenKind::Identifier("else".to_owned()),
                TokenKind::BracketOpen,
                TokenKind::String("var1 is something else".to_owned()),
                TokenKind::BracketClose,
                TokenKind::Comment("for loop".to_owned()),
                TokenKind::For,
                TokenKind::Identifier("u64".to_owned()),
                TokenKind::Identifier("i".to_owned()),
                TokenKind::Identifier("in".to_owned()),
                TokenKind::Number(NumberType::Unsigned(0)),
                TokenKind::Range,
                TokenKind::Number(NumberType::Unsigned(10)),
                TokenKind::BracketOpen,
                TokenKind::Identifier("print".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::String("index is ".to_owned()),
                TokenKind::ParenClose,
                TokenKind::Semicolon,
                TokenKind::Identifier("println".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::Identifier("i".to_owned()),
                TokenKind::ParenClose,
                TokenKind::Semicolon,
                TokenKind::BracketClose,
                TokenKind::Comment("for loop (inclusive end of range)".to_owned()),
                TokenKind::For,
                TokenKind::Identifier("u64".to_owned()),
                TokenKind::Identifier("i".to_owned()),
                TokenKind::Identifier("in".to_owned()),
                TokenKind::Number(NumberType::Unsigned(0)),
                TokenKind::Range,
                TokenKind::Assign,
                TokenKind::Number(NumberType::Unsigned(10)),
                TokenKind::BracketOpen,
                TokenKind::Identifier("print".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::String("index is ".to_owned()),
                TokenKind::ParenClose,
                TokenKind::Semicolon,
                TokenKind::Identifier("println".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::Identifier("i".to_owned()),
                TokenKind::ParenClose,
                TokenKind::Semicolon,
                TokenKind::BracketClose,
                TokenKind::Comment("while loop".to_owned()),
                TokenKind::While,
                TokenKind::Identifier("var2".to_owned()),
                TokenKind::LessThan,
                TokenKind::Number(NumberType::Unsigned(10)),
                TokenKind::BracketOpen,
                TokenKind::Identifier("var2".to_owned()),
                TokenKind::PlusAssign,
                TokenKind::Number(NumberType::Unsigned(1)),
                TokenKind::Semicolon,
                TokenKind::BracketClose,
                TokenKind::Identifier("SomeEnum".to_owned()),
                TokenKind::Identifier("var3".to_owned()),
                TokenKind::Assign,
                TokenKind::Identifier("EmptyVariant".to_owned()),
                TokenKind::Semicolon,
                TokenKind::Comment("match expression".to_owned()),
                TokenKind::Match,
                TokenKind::Identifier("var3".to_owned()),
                TokenKind::BracketOpen,
                TokenKind::Comment("simple match".to_owned()),
                TokenKind::Identifier("EmptyVariant".to_owned()),
                TokenKind::FatArrow,
                TokenKind::Number(NumberType::Unsigned(1)),
                TokenKind::Comma,
                TokenKind::Comment("match with variable binding".to_owned()),
                TokenKind::Identifier("PrimitiveVariant".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::Identifier("inner_val".to_owned()),
                TokenKind::ParenClose,
                TokenKind::FatArrow,
                TokenKind::Identifier("inner_val".to_owned()),
                TokenKind::Comma,
                TokenKind::Identifier("StructVariant".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::Identifier("str".to_owned()),
                TokenKind::ParenClose,
                TokenKind::FatArrow,
                TokenKind::BracketOpen,
                TokenKind::Comment("block of code in an arm".to_owned()),
                TokenKind::Identifier("println".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::Identifier("str".to_owned()),
                TokenKind::Dot,
                TokenKind::Identifier("field1".to_owned()),
                TokenKind::ParenClose,
                TokenKind::Semicolon,
                TokenKind::BracketClose,
                TokenKind::BracketClose,
                TokenKind::Match,
                TokenKind::Identifier("var1".to_owned()),
                TokenKind::BracketOpen,
                TokenKind::Number(NumberType::Unsigned(1)),
                TokenKind::Or,
                TokenKind::Number(NumberType::Unsigned(2)),
                TokenKind::FatArrow,
                TokenKind::String("1 or 2".to_owned()),
                TokenKind::Comma,
                TokenKind::Number(NumberType::Unsigned(3)),
                TokenKind::Range,
                TokenKind::Assign,
                TokenKind::Number(NumberType::Unsigned(5)),
                TokenKind::FatArrow,
                TokenKind::String("from 3 to 5, inclusive".to_owned()),
                TokenKind::Comma,
                TokenKind::Identifier("_".to_owned()),
                TokenKind::FatArrow,
                TokenKind::String("all the rest".to_owned()),
                TokenKind::Comma,
                TokenKind::BracketClose,
                TokenKind::Comment("math expression".to_owned()),
                TokenKind::Identifier("println".to_owned()),
                TokenKind::ParenOpen,
                TokenKind::Number(NumberType::Unsigned(2)),
                TokenKind::Plus,
                TokenKind::Number(NumberType::Unsigned(2)),
                TokenKind::Multiply,
                TokenKind::Number(NumberType::Unsigned(2)),
                TokenKind::Equal,
                TokenKind::Number(NumberType::Unsigned(6)),
                TokenKind::And,
                TokenKind::Number(NumberType::Unsigned(5)),
                TokenKind::Minus,
                TokenKind::Number(NumberType::Unsigned(4)),
                TokenKind::Divide,
                TokenKind::Number(NumberType::Unsigned(2)),
                TokenKind::Equal,
                TokenKind::Number(NumberType::Unsigned(3)),
                TokenKind::ParenClose,
                TokenKind::Semicolon,
                TokenKind::BracketClose,
            ]
        )
    }
}
