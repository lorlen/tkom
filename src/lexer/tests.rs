#![cfg(test)]

use std::fs::File;

use super::*;

fn lexer_from_str(s: &'static str) -> LexerImpl {
    LexerImpl::new(Reader::new(Box::new(s.as_bytes())))
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
    let lexer = LexerImpl::new(Reader::new(Box::new(File::open("snippet.txt").unwrap())));
    assert_eq!(
        lexer.map(|token| token.kind).collect::<Vec<_>>(),
        vec![
            TokenKind::Comment("comment".to_owned()),
            TokenKind::Comment("constant".to_owned()),
            TokenKind::Const,
            TokenKind::Identifier("u64".to_owned()),
            TokenKind::Identifier("SOME_CONSTANT".to_owned()),
            TokenKind::Assign,
            TokenKind::Number(NumberType::Integer(1)),
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
            TokenKind::Let,
            TokenKind::Identifier("u64".to_owned()),
            TokenKind::Identifier("var1".to_owned()),
            TokenKind::Assign,
            TokenKind::Number(NumberType::Integer(1)),
            TokenKind::Semicolon,
            TokenKind::Comment("mutable variable".to_owned()),
            TokenKind::Let,
            TokenKind::Mut,
            TokenKind::Identifier("u64".to_owned()),
            TokenKind::Identifier("var2".to_owned()),
            TokenKind::Assign,
            TokenKind::Number(NumberType::Integer(1)),
            TokenKind::Semicolon,
            TokenKind::Comment("if expression".to_owned()),
            TokenKind::Let,
            TokenKind::Identifier("string".to_owned()),
            TokenKind::Identifier("var3".to_owned()),
            TokenKind::Assign,
            TokenKind::If,
            TokenKind::Identifier("var1".to_owned()),
            TokenKind::Equal,
            TokenKind::Number(NumberType::Integer(1)),
            TokenKind::BracketOpen,
            TokenKind::Yield,
            TokenKind::String("var1 is 1".to_owned()),
            TokenKind::Semicolon,
            TokenKind::BracketClose,
            TokenKind::Else,
            TokenKind::BracketOpen,
            TokenKind::Yield,
            TokenKind::String("var1 is something else".to_owned()),
            TokenKind::Semicolon,
            TokenKind::BracketClose,
            TokenKind::Semicolon,
            TokenKind::Comment("for loop".to_owned()),
            TokenKind::For,
            TokenKind::Identifier("u64".to_owned()),
            TokenKind::Identifier("i".to_owned()),
            TokenKind::In,
            TokenKind::Number(NumberType::Integer(0)),
            TokenKind::Range,
            TokenKind::Number(NumberType::Integer(10)),
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
            TokenKind::In,
            TokenKind::Number(NumberType::Integer(0)),
            TokenKind::Range,
            TokenKind::Assign,
            TokenKind::Number(NumberType::Integer(10)),
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
            TokenKind::Number(NumberType::Integer(10)),
            TokenKind::BracketOpen,
            TokenKind::Identifier("var2".to_owned()),
            TokenKind::PlusAssign,
            TokenKind::Number(NumberType::Integer(1)),
            TokenKind::Semicolon,
            TokenKind::BracketClose,
            TokenKind::Let,
            TokenKind::Identifier("SomeEnum".to_owned()),
            TokenKind::Identifier("var3".to_owned()),
            TokenKind::Assign,
            TokenKind::Identifier("new".to_owned()),
            TokenKind::ParenOpen,
            TokenKind::Identifier("EmptyVariant".to_owned()),
            TokenKind::ParenClose,
            TokenKind::Semicolon,
            TokenKind::Comment("match expression".to_owned()),
            TokenKind::Match,
            TokenKind::Identifier("var3".to_owned()),
            TokenKind::BracketOpen,
            TokenKind::Comment("simple match".to_owned()),
            TokenKind::Identifier("EmptyVariant".to_owned()),
            TokenKind::ParenOpen,
            TokenKind::ParenClose,
            TokenKind::ThinArrow,
            TokenKind::Number(NumberType::Integer(1)),
            TokenKind::Comma,
            TokenKind::Comment("match with variable binding".to_owned()),
            TokenKind::Identifier("PrimitiveVariant".to_owned()),
            TokenKind::ParenOpen,
            TokenKind::Identifier("inner_val".to_owned()),
            TokenKind::ParenClose,
            TokenKind::ThinArrow,
            TokenKind::Identifier("inner_val".to_owned()),
            TokenKind::Comma,
            TokenKind::Identifier("StructVariant".to_owned()),
            TokenKind::ParenOpen,
            TokenKind::Identifier("str".to_owned()),
            TokenKind::ParenClose,
            TokenKind::ThinArrow,
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
            TokenKind::Number(NumberType::Integer(1)),
            TokenKind::Or,
            TokenKind::Number(NumberType::Integer(2)),
            TokenKind::ThinArrow,
            TokenKind::String("1 or 2".to_owned()),
            TokenKind::Comma,
            TokenKind::Number(NumberType::Integer(3)),
            TokenKind::Range,
            TokenKind::Assign,
            TokenKind::Number(NumberType::Integer(5)),
            TokenKind::ThinArrow,
            TokenKind::String("from 3 to 5, inclusive".to_owned()),
            TokenKind::Comma,
            TokenKind::Identifier("_".to_owned()),
            TokenKind::ThinArrow,
            TokenKind::String("all the rest".to_owned()),
            TokenKind::Comma,
            TokenKind::BracketClose,
            TokenKind::Comment("math expression".to_owned()),
            TokenKind::Identifier("println".to_owned()),
            TokenKind::ParenOpen,
            TokenKind::Number(NumberType::Integer(2)),
            TokenKind::Plus,
            TokenKind::Number(NumberType::Integer(2)),
            TokenKind::Multiply,
            TokenKind::Number(NumberType::Integer(2)),
            TokenKind::Equal,
            TokenKind::Number(NumberType::Integer(6)),
            TokenKind::And,
            TokenKind::Number(NumberType::Integer(5)),
            TokenKind::Minus,
            TokenKind::Number(NumberType::Integer(4)),
            TokenKind::Divide,
            TokenKind::Number(NumberType::Integer(2)),
            TokenKind::Equal,
            TokenKind::Number(NumberType::Integer(3)),
            TokenKind::ParenClose,
            TokenKind::Semicolon,
            TokenKind::BracketClose,
        ]
    )
}
