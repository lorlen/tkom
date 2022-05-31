#![cfg(test)]

use std::fs::File;

use super::*;

macro_rules! lexer_test {
    (string, $name:ident, $text:expr, $tokens:expr) => {
        #[test]
        fn $name() {
            let lexer = LexerImpl::new(Reader::new(Box::new($text.as_bytes())));
            assert_eq!(
                lexer.map(|elem| elem.kind).collect::<Vec<TokenKind>>(),
                $tokens
            );
        }
    };
    (file, $name:ident, $filename:expr, $tokens:expr) => {
        #[test]
        fn $name() {
            let lexer = LexerImpl::new(Reader::new(Box::new(File::open($filename).unwrap())));
            assert_eq!(
                lexer.map(|elem| elem.kind).collect::<Vec<TokenKind>>(),
                $tokens
            );
        }
    };
}

#[test]
fn test_ignore_whitespace() {
    let mut lexer = LexerImpl::new(Reader::new(Box::new("   a".as_bytes())));
    lexer.ignore_whitespace();
    assert_eq!(lexer.curr_char, Some('a'));
}

lexer_test!(string, test_operator_plus, "+", vec![TokenKind::Plus]);

lexer_test!(
    string,
    test_operator_plus_assign,
    "+=",
    vec![TokenKind::PlusAssign]
);

lexer_test!(string, test_operator_range, "..", vec![TokenKind::Range]);

lexer_test!(
    string,
    test_comment,
    "// comment",
    vec![TokenKind::Comment("comment".to_string())]
);

lexer_test!(string, test_keyword_if, "if", vec![TokenKind::If]);

lexer_test!(string, test_keyword_else, "else", vec![TokenKind::Else]);

lexer_test!(string, test_keyword_match, "match", vec![TokenKind::Match]);

lexer_test!(string, test_keyword_while, "while", vec![TokenKind::While]);

lexer_test!(string, test_keyword_for, "for", vec![TokenKind::For]);

lexer_test!(string, test_keyword_in, "in", vec![TokenKind::In]);

lexer_test!(string, test_keyword_fn, "fn", vec![TokenKind::Fn]);

lexer_test!(
    string,
    test_keyword_return,
    "return",
    vec![TokenKind::Return]
);

lexer_test!(string, test_keyword_yield, "yield", vec![TokenKind::Yield]);

lexer_test!(
    string,
    test_keyword_struct,
    "struct",
    vec![TokenKind::Struct]
);

lexer_test!(
    string,
    test_identifiers,
    "asdf _asdf asdf123 ___123asdf",
    vec![
        TokenKind::Identifier("asdf".to_string()),
        TokenKind::Identifier("_asdf".to_string()),
        TokenKind::Identifier("asdf123".to_string()),
        TokenKind::Identifier("___123asdf".to_string()),
    ]
);

lexer_test!(
    string,
    test_string,
    "  \"abc \\\"def\\\" 123\\n\\r\\t\"   ",
    vec![TokenKind::String("abc \"def\" 123\n\r\t".to_string())]
);

lexer_test!(
    string,
    test_integer,
    "2137",
    vec![TokenKind::Number(NumberType::Integer(2137))]
);

#[test]
fn test_float() {
    let mut lexer = LexerImpl::new(Reader::new(Box::new("123.456".as_bytes())));
    let token_content = match lexer.next() {
        Some(Token {
            kind: TokenKind::Number(NumberType::Float(content)),
            ..
        }) => content,
        _ => panic!(),
    };
    assert!((token_content - 123.456).abs() < 1e-10)
}

lexer_test!(
    file,
    test_snippet,
    "snippet.txt",
    vec![
        TokenKind::Comment("comment".to_string()),
        TokenKind::Comment("constant".to_string()),
        TokenKind::Const,
        TokenKind::Identifier("u64".to_string()),
        TokenKind::Identifier("SOME_CONSTANT".to_string()),
        TokenKind::Assign,
        TokenKind::Number(NumberType::Integer(1)),
        TokenKind::Semicolon,
        TokenKind::Comment("struct".to_string()),
        TokenKind::Struct,
        TokenKind::Identifier("SomeStruct".to_string()),
        TokenKind::BracketOpen,
        TokenKind::Comment("some primitive types".to_string()),
        TokenKind::Identifier("i64".to_string()),
        TokenKind::Identifier("field1".to_string()),
        TokenKind::Comma,
        TokenKind::Identifier("u64".to_string()),
        TokenKind::Identifier("field2".to_string()),
        TokenKind::Comma,
        TokenKind::Identifier("f64".to_string()),
        TokenKind::Identifier("field3".to_string()),
        TokenKind::Comma,
        TokenKind::Identifier("string".to_string()),
        TokenKind::Identifier("field4".to_string()),
        TokenKind::Comma,
        TokenKind::Comment("trailing comma should work".to_string()),
        TokenKind::BracketClose,
        TokenKind::Comment("enum (tagged union)".to_string()),
        TokenKind::Enum,
        TokenKind::Identifier("SomeEnum".to_string()),
        TokenKind::BracketOpen,
        TokenKind::Identifier("EmptyVariant".to_string()),
        TokenKind::Comma,
        TokenKind::Identifier("PrimitiveVariant".to_string()),
        TokenKind::ParenOpen,
        TokenKind::Identifier("u64".to_string()),
        TokenKind::ParenClose,
        TokenKind::Comma,
        TokenKind::Identifier("StructVariant".to_string()),
        TokenKind::ParenOpen,
        TokenKind::Identifier("SomeStruct".to_string()),
        TokenKind::ParenClose,
        TokenKind::Comma,
        TokenKind::BracketClose,
        TokenKind::Comment("function definition".to_string()),
        TokenKind::Comment("main - entry point, required".to_string()),
        TokenKind::Fn,
        TokenKind::Identifier("main".to_string()),
        TokenKind::ParenOpen,
        TokenKind::ParenClose,
        TokenKind::BracketOpen,
        TokenKind::Comment("immutable variable".to_string()),
        TokenKind::Let,
        TokenKind::Identifier("u64".to_string()),
        TokenKind::Identifier("var1".to_string()),
        TokenKind::Assign,
        TokenKind::Number(NumberType::Integer(1)),
        TokenKind::Semicolon,
        TokenKind::Comment("mutable variable".to_string()),
        TokenKind::Let,
        TokenKind::Mut,
        TokenKind::Identifier("u64".to_string()),
        TokenKind::Identifier("var2".to_string()),
        TokenKind::Assign,
        TokenKind::Number(NumberType::Integer(1)),
        TokenKind::Semicolon,
        TokenKind::Comment("if expression".to_string()),
        TokenKind::Let,
        TokenKind::Identifier("string".to_string()),
        TokenKind::Identifier("var3".to_string()),
        TokenKind::Assign,
        TokenKind::If,
        TokenKind::Identifier("var1".to_string()),
        TokenKind::Equal,
        TokenKind::Number(NumberType::Integer(1)),
        TokenKind::BracketOpen,
        TokenKind::Yield,
        TokenKind::String("var1 is 1".to_string()),
        TokenKind::Semicolon,
        TokenKind::BracketClose,
        TokenKind::Else,
        TokenKind::BracketOpen,
        TokenKind::Yield,
        TokenKind::String("var1 is something else".to_string()),
        TokenKind::Semicolon,
        TokenKind::BracketClose,
        TokenKind::Semicolon,
        TokenKind::Comment("for loop".to_string()),
        TokenKind::For,
        TokenKind::Identifier("u64".to_string()),
        TokenKind::Identifier("i".to_string()),
        TokenKind::In,
        TokenKind::Number(NumberType::Integer(0)),
        TokenKind::Range,
        TokenKind::Number(NumberType::Integer(10)),
        TokenKind::BracketOpen,
        TokenKind::Identifier("print".to_string()),
        TokenKind::ParenOpen,
        TokenKind::String("index is ".to_string()),
        TokenKind::ParenClose,
        TokenKind::Semicolon,
        TokenKind::Identifier("println".to_string()),
        TokenKind::ParenOpen,
        TokenKind::Identifier("i".to_string()),
        TokenKind::ParenClose,
        TokenKind::Semicolon,
        TokenKind::BracketClose,
        TokenKind::Comment("for loop (inclusive end of range)".to_string()),
        TokenKind::For,
        TokenKind::Identifier("u64".to_string()),
        TokenKind::Identifier("i".to_string()),
        TokenKind::In,
        TokenKind::Number(NumberType::Integer(0)),
        TokenKind::Range,
        TokenKind::Assign,
        TokenKind::Number(NumberType::Integer(10)),
        TokenKind::BracketOpen,
        TokenKind::Identifier("print".to_string()),
        TokenKind::ParenOpen,
        TokenKind::String("index is ".to_string()),
        TokenKind::ParenClose,
        TokenKind::Semicolon,
        TokenKind::Identifier("println".to_string()),
        TokenKind::ParenOpen,
        TokenKind::Identifier("i".to_string()),
        TokenKind::ParenClose,
        TokenKind::Semicolon,
        TokenKind::BracketClose,
        TokenKind::Comment("while loop".to_string()),
        TokenKind::While,
        TokenKind::Identifier("var2".to_string()),
        TokenKind::LessThan,
        TokenKind::Number(NumberType::Integer(10)),
        TokenKind::BracketOpen,
        TokenKind::Identifier("var2".to_string()),
        TokenKind::PlusAssign,
        TokenKind::Number(NumberType::Integer(1)),
        TokenKind::Semicolon,
        TokenKind::BracketClose,
        TokenKind::Let,
        TokenKind::Identifier("SomeEnum".to_string()),
        TokenKind::Identifier("var3".to_string()),
        TokenKind::Assign,
        TokenKind::Identifier("new".to_string()),
        TokenKind::ParenOpen,
        TokenKind::Identifier("SomeEnum".to_string()),
        TokenKind::Dot,
        TokenKind::Identifier("EmptyVariant".to_string()),
        TokenKind::ParenClose,
        TokenKind::Semicolon,
        TokenKind::Comment("match expression".to_string()),
        TokenKind::Match,
        TokenKind::Identifier("var3".to_string()),
        TokenKind::BracketOpen,
        TokenKind::Comment("simple match".to_string()),
        TokenKind::Identifier("SomeEnum".to_string()),
        TokenKind::Dot,
        TokenKind::Identifier("EmptyVariant".to_string()),
        TokenKind::ParenOpen,
        TokenKind::ParenClose,
        TokenKind::ThinArrow,
        TokenKind::Number(NumberType::Integer(1)),
        TokenKind::Comma,
        TokenKind::Comment("match with variable binding".to_string()),
        TokenKind::Identifier("SomeEnum".to_string()),
        TokenKind::Dot,
        TokenKind::Identifier("PrimitiveVariant".to_string()),
        TokenKind::ParenOpen,
        TokenKind::Identifier("inner_val".to_string()),
        TokenKind::ParenClose,
        TokenKind::ThinArrow,
        TokenKind::Identifier("inner_val".to_string()),
        TokenKind::Comma,
        TokenKind::Identifier("SomeEnum".to_string()),
        TokenKind::Dot,
        TokenKind::Identifier("StructVariant".to_string()),
        TokenKind::ParenOpen,
        TokenKind::Identifier("str".to_string()),
        TokenKind::ParenClose,
        TokenKind::ThinArrow,
        TokenKind::BracketOpen,
        TokenKind::Comment("block of code in an arm".to_string()),
        TokenKind::Identifier("println".to_string()),
        TokenKind::ParenOpen,
        TokenKind::Identifier("str".to_string()),
        TokenKind::Dot,
        TokenKind::Identifier("field1".to_string()),
        TokenKind::ParenClose,
        TokenKind::Semicolon,
        TokenKind::BracketClose,
        TokenKind::BracketClose,
        TokenKind::Match,
        TokenKind::Identifier("var1".to_string()),
        TokenKind::BracketOpen,
        TokenKind::Number(NumberType::Integer(1)),
        TokenKind::Or,
        TokenKind::Number(NumberType::Integer(2)),
        TokenKind::ThinArrow,
        TokenKind::String("1 or 2".to_string()),
        TokenKind::Comma,
        TokenKind::Number(NumberType::Integer(3)),
        TokenKind::Range,
        TokenKind::Assign,
        TokenKind::Number(NumberType::Integer(5)),
        TokenKind::ThinArrow,
        TokenKind::String("from 3 to 5, inclusive".to_string()),
        TokenKind::Comma,
        TokenKind::Identifier("_".to_string()),
        TokenKind::ThinArrow,
        TokenKind::String("all the rest".to_string()),
        TokenKind::Comma,
        TokenKind::BracketClose,
        TokenKind::Comment("math expression".to_string()),
        TokenKind::Identifier("println".to_string()),
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
);
