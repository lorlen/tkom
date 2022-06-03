#![cfg(test)]

use std::fs::File;

use utf8_read::Reader;

use crate::lexer::LexerImpl;

use super::*;

macro_rules! parser_test {
    (string, $name:ident, $text:expr, $program:expr) => {
        #[test]
        fn $name() {
            let lexer = LexerImpl::new(Reader::new(Box::new($text.as_bytes())));
            let mut parser = Parser::new(Box::new(lexer));
            assert_eq!(parser.parse(), $program);
        }
    };
    (file, $name:ident, $filename:expr, $program:expr) => {
        #[test]
        fn $name() {
            let lexer = LexerImpl::new(Reader::new(Box::new(File::open($filename).unwrap())));
            let mut parser = Parser::new(Box::new(lexer));
            assert_eq!(parser.parse(), $program);
        }
    };
    (string, $name:ident, $text:expr, $program:expr, should_panic) => {
        #[test]
        #[should_panic]
        fn $name() {
            let lexer = LexerImpl::new(Reader::new(Box::new($text.as_bytes())));
            let mut parser = Parser::new(Box::new(lexer));
            assert_eq!(parser.parse(), $program);
        }
    };
    (file, $name:ident, $filename:expr, $program:expr, should_panic) => {
        #[test]
        #[should_panic]
        fn $name() {
            let lexer = LexerImpl::new(Reader::new(Box::new(File::open($filename).unwrap())));
            let mut parser = Parser::new(Box::new(lexer));
            assert_eq!(parser.parse(), $program);
        }
    };
}

parser_test!(
    string,
    test_empty_function,
    "fn main() {}",
    Program {
        type_defs: vec![],
        const_defs: vec![],
        function_defs: vec![Function {
            name: "main".to_string(),
            parameters: vec![],
            return_type: None,
            block: Box::new(BlockExpr { statements: vec![] })
        }]
    }
);

parser_test!(
    string,
    test_function_return,
    "fn test() -> unsigned { return 1; }",
    Program {
        type_defs: vec![],
        const_defs: vec![],
        function_defs: vec![Function {
            name: "test".to_string(),
            parameters: vec![],
            return_type: Some("unsigned".to_string()),
            block: Box::new(BlockExpr {
                statements: vec![Statement::Return(Some(Box::new(Expression::Value(
                    Value::Literal(Literal::Integer(1))
                ))))]
            })
        }]
    }
);

parser_test!(
    string,
    test_match,
    "fn test() {
        match a {
            1 | 2 .. 4 | 5 ..= 6 | 7 -> 1,
            true -> 2,
            \"string\" -> 3,
            Variant1(Variant2(binding)) -> binding,
        }
    }",
    Program {
        type_defs: vec![],
        function_defs: vec![Function {
            name: "test".to_string(),
            parameters: vec![],
            return_type: None,
            block: Box::new(BlockExpr {
                statements: vec![Statement::Match(Match {
                    expr: Box::new(Expression::Value(Value::Identifier("a".to_string()))),
                    arms: vec![
                        MatchArm {
                            pattern: vec![
                                PatternPart::Literal(LiteralOrRange::Literal(Literal::Integer(1))),
                                PatternPart::Literal(LiteralOrRange::Range {
                                    lower: Literal::Integer(2),
                                    upper: Literal::Integer(4),
                                    inclusive: false,
                                }),
                                PatternPart::Literal(LiteralOrRange::Range {
                                    lower: Literal::Integer(5),
                                    upper: Literal::Integer(6),
                                    inclusive: true,
                                }),
                                PatternPart::Literal(LiteralOrRange::Literal(Literal::Integer(7))),
                            ],
                            expression: Box::new(Expression::Value(Value::Literal(
                                Literal::Integer(1)
                            ))),
                        },
                        MatchArm {
                            pattern: vec![PatternPart::Literal(LiteralOrRange::Literal(
                                Literal::Bool(true)
                            ))],
                            expression: Box::new(Expression::Value(Value::Literal(
                                Literal::Integer(2)
                            ))),
                        },
                        MatchArm {
                            pattern: vec![PatternPart::Literal(LiteralOrRange::Literal(
                                Literal::String("string".to_string())
                            ))],
                            expression: Box::new(Expression::Value(Value::Literal(
                                Literal::Integer(3)
                            ))),
                        },
                        MatchArm {
                            pattern: vec![PatternPart::EnumVariant(
                                "Variant1".to_string(),
                                vec![PatternPart::EnumVariant(
                                    "Variant2".to_string(),
                                    vec![PatternPart::Binding("binding".to_string())]
                                )],
                            )],
                            expression: Box::new(Expression::Value(Value::Identifier(
                                "binding".to_string()
                            ))),
                        },
                    ],
                })],
            }),
        }],
        const_defs: vec![],
    }
);

parser_test!(
    string,
    test_empty_match,
    "fn test() { match 1 {} }",
    Program::default(),
    should_panic
);

parser_test!(
    string,
    test_elseif,
    "fn test(unsigned i) { if i == 1 {} else if i == 2 {} else {} }",
    Program {
        type_defs: vec![],
        const_defs: vec![],
        function_defs: vec![Function {
            name: "test".to_string(),
            parameters: vec![("unsigned".to_string(), "i".to_string())],
            return_type: None,
            block: Box::new(BlockExpr {
                statements: vec![Statement::If(If {
                    branches: vec![
                        IfBranch {
                            condition: Some(Box::new(Expression::Binary(BinaryExpr {
                                ops: vec![Operator::Equal],
                                subexprs: vec![
                                    Expression::Value(Value::Identifier("i".to_string())),
                                    Expression::Value(Value::Literal(Literal::Integer(1)))
                                ]
                            }))),
                            block: Box::new(BlockExpr { statements: vec![] })
                        },
                        IfBranch {
                            condition: Some(Box::new(Expression::Binary(BinaryExpr {
                                ops: vec![Operator::Equal],
                                subexprs: vec![
                                    Expression::Value(Value::Identifier("i".to_string())),
                                    Expression::Value(Value::Literal(Literal::Integer(2)))
                                ]
                            }))),
                            block: Box::new(BlockExpr { statements: vec![] })
                        },
                        IfBranch {
                            condition: None,
                            block: Box::new(BlockExpr { statements: vec![] })
                        },
                    ]
                })]
            })
        }]
    }
);

parser_test!(
    string,
    test_expr,
    "fn test() { return 1 + 2 - 3 < 2 * -2 & call() == (1 + 2) * 3 as unsigned & !false; }",
    Program {
        type_defs: vec![],
        function_defs: vec![Function {
            name: "test".to_string(),
            parameters: vec![],
            return_type: None,
            block: Box::new(BlockExpr {
                statements: vec![Statement::Return(Some(Box::new(Expression::Binary(
                    BinaryExpr {
                        ops: vec![Operator::And],
                        subexprs: vec![
                            Expression::Binary(BinaryExpr {
                                ops: vec![Operator::LessThan],
                                subexprs: vec![
                                    Expression::Binary(BinaryExpr {
                                        ops: vec![Operator::Plus, Operator::Minus],
                                        subexprs: vec![
                                            Expression::Value(Value::Literal(Literal::Integer(1))),
                                            Expression::Value(Value::Literal(Literal::Integer(2))),
                                            Expression::Value(Value::Literal(Literal::Integer(3))),
                                        ],
                                    }),
                                    Expression::Binary(BinaryExpr {
                                        ops: vec![Operator::Multiply],
                                        subexprs: vec![
                                            Expression::Value(Value::Literal(Literal::Integer(2))),
                                            Expression::Unary(UnaryExpr {
                                                ops: vec![Operator::Minus],
                                                subexpr: Box::new(Expression::Value(
                                                    Value::Literal(Literal::Integer(2))
                                                )),
                                            }),
                                        ],
                                    }),
                                ],
                            }),
                            Expression::Binary(BinaryExpr {
                                ops: vec![Operator::Equal],
                                subexprs: vec![
                                    Expression::Value(Value::FunctionCall(FunctionCall {
                                        name: "call".to_string(),
                                        arguments: vec![],
                                    })),
                                    Expression::Binary(BinaryExpr {
                                        ops: vec![Operator::Multiply],
                                        subexprs: vec![
                                            Expression::Binary(BinaryExpr {
                                                ops: vec![Operator::Plus],
                                                subexprs: vec![
                                                    Expression::Value(Value::Literal(
                                                        Literal::Integer(1)
                                                    )),
                                                    Expression::Value(Value::Literal(
                                                        Literal::Integer(2)
                                                    )),
                                                ],
                                            }),
                                            Expression::As(AsExpr {
                                                expr: Box::new(Expression::Value(Value::Literal(
                                                    Literal::Integer(3)
                                                ))),
                                                cast_type: "unsigned".to_string(),
                                            }),
                                        ],
                                    }),
                                ],
                            }),
                            Expression::Unary(UnaryExpr {
                                ops: vec![Operator::Not],
                                subexpr: Box::new(Expression::Value(Value::Literal(
                                    Literal::Bool(false)
                                ))),
                            }),
                        ],
                    }
                ))))],
            }),
        }],
        const_defs: vec![],
    }
);

parser_test!(
    file,
    test_snippet,
    "snippet.txt",
    Program {
        type_defs: vec![
            TypeDef::StructDef(StructDef {
                name: "SomeStruct".to_string(),
                fields: vec![
                    ("i64".to_string(), "field1".to_string()),
                    ("u64".to_string(), "field2".to_string()),
                    ("f64".to_string(), "field3".to_string()),
                    ("string".to_string(), "field4".to_string()),
                ],
            }),
            TypeDef::EnumDef(EnumDef {
                name: "SomeEnum".to_string(),
                variants: vec![
                    ("EmptyVariant".to_string(), None),
                    ("PrimitiveVariant".to_string(), Some("u64".to_string())),
                    ("StructVariant".to_string(), Some("SomeStruct".to_string())),
                ],
            }),
        ],
        function_defs: vec![Function {
            name: "main".to_string(),
            parameters: vec![],
            return_type: None,
            block: Box::new(BlockExpr {
                statements: vec![
                    Statement::VarDef(VarDef {
                        mutable: false,
                        name: "var1".to_string(),
                        type_: "u64".to_string(),
                        expr: Box::new(Expression::Value(Value::Literal(Literal::Integer(1)))),
                    }),
                    Statement::VarDef(VarDef {
                        mutable: true,
                        name: "var2".to_string(),
                        type_: "u64".to_string(),
                        expr: Box::new(Expression::Value(Value::Literal(Literal::Integer(1)))),
                    }),
                    Statement::VarDef(VarDef {
                        mutable: false,
                        name: "var3".to_string(),
                        type_: "string".to_string(),
                        expr: Box::new(Expression::If(If {
                            branches: vec![
                                IfBranch {
                                    condition: Some(Box::new(Expression::Binary(BinaryExpr {
                                        ops: vec![Operator::Equal],
                                        subexprs: vec![
                                            Expression::Value(Value::Identifier(
                                                "var1".to_string()
                                            )),
                                            Expression::Value(Value::Literal(Literal::Integer(1))),
                                        ],
                                    }))),
                                    block: Box::new(BlockExpr {
                                        statements: vec![Statement::Yield(Box::new(
                                            Expression::Value(Value::Literal(Literal::String(
                                                "var1 is 1".to_string()
                                            )))
                                        ))],
                                    }),
                                },
                                IfBranch {
                                    condition: None,
                                    block: Box::new(BlockExpr {
                                        statements: vec![Statement::Yield(Box::new(
                                            Expression::Value(Value::Literal(Literal::String(
                                                "var1 is something else".to_string(),
                                            )))
                                        ))],
                                    }),
                                },
                            ],
                        })),
                    }),
                    Statement::For(For {
                        var_name: "i".to_string(),
                        var_type: "u64".to_string(),
                        range: Range {
                            lower: Box::new(Expression::Value(Value::Literal(Literal::Integer(0)))),
                            upper: Box::new(Expression::Value(Value::Literal(Literal::Integer(
                                10
                            )))),
                            inclusive: false,
                        },
                        block: Box::new(BlockExpr {
                            statements: vec![
                                Statement::FunctionCall(FunctionCall {
                                    name: "print".to_string(),
                                    arguments: vec![Expression::Value(Value::Literal(
                                        Literal::String("index is ".to_string())
                                    ))],
                                }),
                                Statement::FunctionCall(FunctionCall {
                                    name: "println".to_string(),
                                    arguments: vec![Expression::Value(Value::Identifier(
                                        "i".to_string()
                                    ))],
                                }),
                            ],
                        }),
                    }),
                    Statement::For(For {
                        var_name: "i".to_string(),
                        var_type: "u64".to_string(),
                        range: Range {
                            lower: Box::new(Expression::Value(Value::Literal(Literal::Integer(0)))),
                            upper: Box::new(Expression::Value(Value::Literal(Literal::Integer(
                                10
                            )))),
                            inclusive: true,
                        },
                        block: Box::new(BlockExpr {
                            statements: vec![
                                Statement::FunctionCall(FunctionCall {
                                    name: "print".to_string(),
                                    arguments: vec![Expression::Value(Value::Literal(
                                        Literal::String("index is ".to_string())
                                    ))],
                                }),
                                Statement::FunctionCall(FunctionCall {
                                    name: "println".to_string(),
                                    arguments: vec![Expression::Value(Value::Identifier(
                                        "i".to_string()
                                    ))],
                                }),
                            ],
                        }),
                    }),
                    Statement::While(While {
                        condition: Box::new(Expression::Binary(BinaryExpr {
                            ops: vec![Operator::LessThan],
                            subexprs: vec![
                                Expression::Value(Value::Identifier("var2".to_string())),
                                Expression::Value(Value::Literal(Literal::Integer(10)))
                            ],
                        })),
                        block: Box::new(BlockExpr {
                            statements: vec![Statement::Assignment(Assignment {
                                name: "var2".to_string(),
                                op: Operator::PlusAssign,
                                expr: Box::new(Expression::Value(Value::Literal(
                                    Literal::Integer(1)
                                ))),
                            })],
                        }),
                    }),
                    Statement::VarDef(VarDef {
                        mutable: false,
                        name: "var3".to_string(),
                        type_: "SomeEnum".to_string(),
                        expr: Box::new(Expression::Value(Value::FunctionCall(FunctionCall {
                            name: "new".to_string(),
                            arguments: vec![Expression::Value(Value::Identifier(
                                "EmptyVariant".to_string()
                            ))],
                        }))),
                    }),
                    Statement::Match(Match {
                        expr: Box::new(Expression::Value(Value::Identifier("var3".to_string()))),
                        arms: vec![
                            MatchArm {
                                pattern: vec![PatternPart::EnumVariant(
                                    "EmptyVariant".to_string(),
                                    vec![]
                                )],
                                expression: Box::new(Expression::Value(Value::Literal(
                                    Literal::Integer(1)
                                ))),
                            },
                            MatchArm {
                                pattern: vec![PatternPart::EnumVariant(
                                    "PrimitiveVariant".to_string(),
                                    vec![PatternPart::Binding("inner_val".to_string())]
                                )],
                                expression: Box::new(Expression::Value(Value::Identifier(
                                    "inner_val".to_string()
                                ))),
                            },
                            MatchArm {
                                pattern: vec![PatternPart::EnumVariant(
                                    "StructVariant".to_string(),
                                    vec![PatternPart::Binding("str".to_string())]
                                )],
                                expression: Box::new(Expression::Block(BlockExpr {
                                    statements: vec![Statement::FunctionCall(FunctionCall {
                                        name: "println".to_string(),
                                        arguments: vec![Expression::Value(Value::MemberAccess(
                                            vec!["str".to_string(), "field1".to_string()]
                                        ))],
                                    })],
                                })),
                            },
                        ],
                    }),
                    Statement::Match(Match {
                        expr: Box::new(Expression::Value(Value::Identifier("var1".to_string()))),
                        arms: vec![
                            MatchArm {
                                pattern: vec![
                                    PatternPart::Literal(LiteralOrRange::Literal(
                                        Literal::Integer(1)
                                    )),
                                    PatternPart::Literal(LiteralOrRange::Literal(
                                        Literal::Integer(2)
                                    )),
                                ],
                                expression: Box::new(Expression::Value(Value::Literal(
                                    Literal::String("1 or 2".to_string())
                                ))),
                            },
                            MatchArm {
                                pattern: vec![PatternPart::Literal(LiteralOrRange::Range {
                                    lower: Literal::Integer(3),
                                    upper: Literal::Integer(5),
                                    inclusive: true,
                                })],
                                expression: Box::new(Expression::Value(Value::Literal(
                                    Literal::String("from 3 to 5, inclusive".to_string())
                                ))),
                            },
                            MatchArm {
                                pattern: vec![PatternPart::CatchAll],
                                expression: Box::new(Expression::Value(Value::Literal(
                                    Literal::String("all the rest".to_string())
                                ))),
                            },
                        ],
                    }),
                    Statement::FunctionCall(FunctionCall {
                        name: "println".to_string(),
                        arguments: vec![Expression::Binary(BinaryExpr {
                            ops: vec![Operator::And],
                            subexprs: vec![
                                Expression::Binary(BinaryExpr {
                                    ops: vec![Operator::Equal],
                                    subexprs: vec![
                                        Expression::Binary(BinaryExpr {
                                            ops: vec![Operator::Plus],
                                            subexprs: vec![
                                                Expression::Value(Value::Literal(
                                                    Literal::Integer(2)
                                                )),
                                                Expression::Binary(BinaryExpr {
                                                    ops: vec![Operator::Multiply],
                                                    subexprs: vec![
                                                        Expression::Value(Value::Literal(
                                                            Literal::Integer(2)
                                                        )),
                                                        Expression::Value(Value::Literal(
                                                            Literal::Integer(2)
                                                        )),
                                                    ],
                                                }),
                                            ],
                                        }),
                                        Expression::Value(Value::Literal(Literal::Integer(6))),
                                    ],
                                }),
                                Expression::Binary(BinaryExpr {
                                    ops: vec![Operator::Equal],
                                    subexprs: vec![
                                        Expression::Binary(BinaryExpr {
                                            ops: vec![Operator::Minus],
                                            subexprs: vec![
                                                Expression::Value(Value::Literal(
                                                    Literal::Integer(5)
                                                )),
                                                Expression::Binary(BinaryExpr {
                                                    ops: vec![Operator::Divide],
                                                    subexprs: vec![
                                                        Expression::Value(Value::Literal(
                                                            Literal::Integer(4)
                                                        )),
                                                        Expression::Value(Value::Literal(
                                                            Literal::Integer(2)
                                                        )),
                                                    ],
                                                }),
                                            ],
                                        }),
                                        Expression::Value(Value::Literal(Literal::Integer(3))),
                                    ],
                                }),
                            ],
                        })],
                    }),
                ],
            }),
        }],
        const_defs: vec![ConstDef {
            name: "SOME_CONSTANT".to_string(),
            type_: "u64".to_string(),
            value: Box::new(Expression::Value(Value::Literal(Literal::Integer(1)))),
        }],
    }
);
