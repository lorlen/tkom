#![cfg(test)]

use std::{collections::HashMap, fs::File};

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
        type_defs: HashMap::new(),
        const_defs: HashMap::new(),
        function_defs: HashMap::from([(
            "main".to_string(),
            Function {
                name: "main".to_string(),
                parameters: vec![],
                block: Box::new(BlockExpr { statements: vec![] })
            }
        )])
    }
);

parser_test!(
    string,
    test_function_return,
    "fn test() { return 1; }",
    Program {
        type_defs: HashMap::new(),
        const_defs: HashMap::new(),
        function_defs: HashMap::from([(
            "test".to_string(),
            Function {
                name: "test".to_string(),
                parameters: vec![],
                block: Box::new(BlockExpr {
                    statements: vec![Statement::Return(Some(Box::new(Expression::Value(
                        Value::Literal(Literal::Integer(1))
                    ))))]
                })
            }
        )])
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
            Enum1.Variant1(Enum2.Variant2(binding)) -> binding,
        }
    }",
    Program {
        type_defs: HashMap::new(),
        function_defs: HashMap::from([(
            "test".to_string(),
            Function {
                name: "test".to_string(),
                parameters: vec![],
                block: Box::new(BlockExpr {
                    statements: vec![Statement::Match(Match {
                        expr: Box::new(Expression::Value(Value::Identifier(vec!["a".to_string()]))),
                        arms: vec![
                            MatchArm {
                                pattern: vec![
                                    PatternPart::Literal(LiteralOrRange::Literal(
                                        Literal::Integer(1)
                                    )),
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
                                    PatternPart::Literal(LiteralOrRange::Literal(
                                        Literal::Integer(7)
                                    )),
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
                                    "Enum1".to_string(),
                                    "Variant1".to_string(),
                                    vec![PatternPart::EnumVariant(
                                        "Enum2".to_string(),
                                        "Variant2".to_string(),
                                        vec![PatternPart::Binding("binding".to_string())]
                                    )],
                                )],
                                expression: Box::new(Expression::Value(Value::Identifier(vec![
                                    "binding".to_string()
                                ]))),
                            },
                        ],
                    })],
                }),
            }
        )]),
        const_defs: HashMap::new(),
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
    "fn test(i) { if i == 1 {} else if i == 2 {} else {} }",
    Program {
        type_defs: HashMap::new(),
        const_defs: HashMap::new(),
        function_defs: HashMap::from([(
            "test".to_string(),
            Function {
                name: "test".to_string(),
                parameters: vec!["i".to_string()],
                block: Box::new(BlockExpr {
                    statements: vec![Statement::If(If {
                        branches: vec![
                            IfBranch {
                                condition: Box::new(Expression::Binary(BinaryExpr::Rel(RelExpr {
                                    op: RelOperator::Equal,
                                    left: Box::new(Expression::Value(Value::Identifier(vec![
                                        "i".to_string()
                                    ]))),
                                    right: Box::new(Expression::Value(Value::Literal(
                                        Literal::Integer(1)
                                    )))
                                }))),
                                block: Box::new(BlockExpr { statements: vec![] })
                            },
                            IfBranch {
                                condition: Box::new(Expression::Binary(BinaryExpr::Rel(RelExpr {
                                    op: RelOperator::Equal,
                                    left: Box::new(Expression::Value(Value::Identifier(vec![
                                        "i".to_string()
                                    ]))),
                                    right: Box::new(Expression::Value(Value::Literal(
                                        Literal::Integer(2)
                                    )))
                                }))),
                                block: Box::new(BlockExpr { statements: vec![] })
                            },
                            IfBranch {
                                condition: Box::new(Expression::Value(Value::Literal(
                                    Literal::Bool(true)
                                ))),
                                block: Box::new(BlockExpr { statements: vec![] })
                            },
                        ]
                    })]
                })
            }
        )])
    }
);

parser_test!(
    string,
    test_expr,
    "fn test() { return 1 + 2 - 3 < 2 * -2 & call() == (1 + 2) * 3 as int & !false; }",
    Program {
        type_defs: HashMap::new(),
        function_defs: HashMap::from([(
            "test".to_string(),
            Function {
                name: "test".to_string(),
                parameters: vec![],
                block: Box::new(BlockExpr {
                    statements: vec![Statement::Return(Some(Box::new(Expression::Binary(
                        BinaryExpr::And(AndExpr {
                            subexprs: vec![
                                Expression::Binary(BinaryExpr::Rel(RelExpr {
                                    op: RelOperator::LessThan,
                                    left: Box::new(Expression::Binary(BinaryExpr::Add(AddExpr {
                                        ops: vec![AddOperator::Plus, AddOperator::Minus],
                                        subexprs: vec![
                                            Expression::Value(Value::Literal(Literal::Integer(1))),
                                            Expression::Value(Value::Literal(Literal::Integer(2))),
                                            Expression::Value(Value::Literal(Literal::Integer(3))),
                                        ],
                                    }))),
                                    right: Box::new(Expression::Binary(BinaryExpr::Mul(MulExpr {
                                        ops: vec![MulOperator::Multiply],
                                        subexprs: vec![
                                            Expression::Value(Value::Literal(Literal::Integer(2))),
                                            Expression::Neg(NegExpr {
                                                subexpr: Box::new(Expression::Value(
                                                    Value::Literal(Literal::Integer(2))
                                                )),
                                            }),
                                        ],
                                    }))),
                                })),
                                Expression::Binary(BinaryExpr::Rel(RelExpr {
                                    op: RelOperator::Equal,
                                    left: Box::new(Expression::Value(Value::FunctionCall(
                                        FunctionCall {
                                            name: "call".to_string(),
                                            arguments: vec![],
                                        }
                                    ))),
                                    right: Box::new(Expression::Binary(BinaryExpr::Mul(MulExpr {
                                        ops: vec![MulOperator::Multiply],
                                        subexprs: vec![
                                            Expression::Binary(BinaryExpr::Add(AddExpr {
                                                ops: vec![AddOperator::Plus],
                                                subexprs: vec![
                                                    Expression::Value(Value::Literal(
                                                        Literal::Integer(1)
                                                    )),
                                                    Expression::Value(Value::Literal(
                                                        Literal::Integer(2)
                                                    )),
                                                ],
                                            })),
                                            Expression::As(AsExpr {
                                                expr: Box::new(Expression::Value(Value::Literal(
                                                    Literal::Integer(3)
                                                ))),
                                                cast_type: "int".to_string(),
                                            }),
                                        ],
                                    }))),
                                })),
                                Expression::Not(NotExpr {
                                    subexpr: Box::new(Expression::Value(Value::Literal(
                                        Literal::Bool(false)
                                    ))),
                                }),
                            ],
                        })
                    ))))],
                }),
            }
        )]),
        const_defs: HashMap::new(),
    }
);

parser_test!(
    file,
    test_snippet,
    "snippet.txt",
    Program {
        type_defs: HashMap::from([
            (
                "SomeStruct".to_string(),
                TypeDef::StructDef(StructDef {
                    name: "SomeStruct".to_string(),
                    fields: vec![
                        ("i64".to_string(), "field1".to_string()),
                        ("u64".to_string(), "field2".to_string()),
                        ("f64".to_string(), "field3".to_string()),
                        ("string".to_string(), "field4".to_string()),
                    ],
                })
            ),
            (
                "SomeEnum".to_string(),
                TypeDef::EnumDef(EnumDef {
                    name: "SomeEnum".to_string(),
                    variants: vec![
                        ("EmptyVariant".to_string(), None),
                        ("PrimitiveVariant".to_string(), Some("u64".to_string())),
                        ("StructVariant".to_string(), Some("SomeStruct".to_string())),
                    ],
                })
            ),
        ]),
        function_defs: HashMap::from([(
            "main".to_string(),
            Function {
                name: "main".to_string(),
                parameters: vec![],
                block: Box::new(BlockExpr {
                    statements: vec![
                        Statement::VarDef(VarDef {
                            name: "var1".to_string(),
                            expr: Box::new(Expression::Value(Value::Literal(Literal::Integer(1)))),
                        }),
                        Statement::VarDef(VarDef {
                            name: "var2".to_string(),
                            expr: Box::new(Expression::Value(Value::Literal(Literal::Integer(1)))),
                        }),
                        Statement::VarDef(VarDef {
                            name: "var3".to_string(),
                            expr: Box::new(Expression::If(If {
                                branches: vec![
                                    IfBranch {
                                        condition: Box::new(Expression::Binary(BinaryExpr::Rel(
                                            RelExpr {
                                                op: RelOperator::Equal,
                                                left: Box::new(Expression::Value(
                                                    Value::Identifier(vec!["var1".to_string()])
                                                )),
                                                right: Box::new(Expression::Value(Value::Literal(
                                                    Literal::Integer(1)
                                                ))),
                                            }
                                        ))),
                                        block: Box::new(BlockExpr {
                                            statements: vec![Statement::Yield(Box::new(
                                                Expression::Value(Value::Literal(Literal::String(
                                                    "var1 is 1".to_string()
                                                )))
                                            ))],
                                        }),
                                    },
                                    IfBranch {
                                        condition: Box::new(Expression::Value(Value::Literal(
                                            Literal::Bool(true)
                                        ))),
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
                            range: Range {
                                lower: Box::new(Expression::Value(Value::Literal(
                                    Literal::Integer(0)
                                ))),
                                upper: Box::new(Expression::Value(Value::Literal(
                                    Literal::Integer(10)
                                ))),
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
                                            vec!["i".to_string()]
                                        ))],
                                    }),
                                ],
                            }),
                        }),
                        Statement::For(For {
                            var_name: "i".to_string(),
                            range: Range {
                                lower: Box::new(Expression::Value(Value::Literal(
                                    Literal::Integer(0)
                                ))),
                                upper: Box::new(Expression::Value(Value::Literal(
                                    Literal::Integer(10)
                                ))),
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
                                            vec!["i".to_string()]
                                        ))],
                                    }),
                                ],
                            }),
                        }),
                        Statement::While(While {
                            condition: Box::new(Expression::Binary(BinaryExpr::Rel(RelExpr {
                                op: RelOperator::LessThan,
                                left: Box::new(Expression::Value(Value::Identifier(vec![
                                    "var2".to_string()
                                ]))),
                                right: Box::new(Expression::Value(Value::Literal(
                                    Literal::Integer(10)
                                )))
                            }))),
                            block: Box::new(BlockExpr {
                                statements: vec![Statement::Assignment(Assignment {
                                    path: vec!["var2".to_string()],
                                    op: AssignOperator::PlusAssign,
                                    expr: Box::new(Expression::Value(Value::Literal(
                                        Literal::Integer(1)
                                    ))),
                                })],
                            }),
                        }),
                        Statement::VarDef(VarDef {
                            name: "var3".to_string(),
                            expr: Box::new(Expression::Value(Value::FunctionCall(FunctionCall {
                                name: "new".to_string(),
                                arguments: vec![Expression::Value(Value::Identifier(vec![
                                    "SomeEnum".to_string(),
                                    "EmptyVariant".to_string()
                                ]))],
                            }))),
                        }),
                        Statement::Match(Match {
                            expr: Box::new(Expression::Value(Value::Identifier(vec![
                                "var3".to_string()
                            ]))),
                            arms: vec![
                                MatchArm {
                                    pattern: vec![PatternPart::EnumVariant(
                                        "SomeEnum".to_string(),
                                        "EmptyVariant".to_string(),
                                        vec![]
                                    )],
                                    expression: Box::new(Expression::Value(Value::Literal(
                                        Literal::Integer(1)
                                    ))),
                                },
                                MatchArm {
                                    pattern: vec![PatternPart::EnumVariant(
                                        "SomeEnum".to_string(),
                                        "PrimitiveVariant".to_string(),
                                        vec![PatternPart::Binding("inner_val".to_string())]
                                    )],
                                    expression: Box::new(Expression::Value(Value::Identifier(
                                        vec!["inner_val".to_string()]
                                    ))),
                                },
                                MatchArm {
                                    pattern: vec![PatternPart::EnumVariant(
                                        "SomeEnum".to_string(),
                                        "StructVariant".to_string(),
                                        vec![PatternPart::Binding("str".to_string())]
                                    )],
                                    expression: Box::new(Expression::Block(BlockExpr {
                                        statements: vec![Statement::FunctionCall(FunctionCall {
                                            name: "println".to_string(),
                                            arguments: vec![Expression::Value(Value::Identifier(
                                                vec!["str".to_string(), "field1".to_string()]
                                            ))],
                                        })],
                                    })),
                                },
                            ],
                        }),
                        Statement::Match(Match {
                            expr: Box::new(Expression::Value(Value::Identifier(vec![
                                "var1".to_string()
                            ]))),
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
                            arguments: vec![Expression::Binary(BinaryExpr::And(AndExpr {
                                subexprs: vec![
                                    Expression::Binary(BinaryExpr::Rel(RelExpr {
                                        op: RelOperator::Equal,
                                        left: Box::new(Expression::Binary(BinaryExpr::Add(
                                            AddExpr {
                                                ops: vec![AddOperator::Plus],
                                                subexprs: vec![
                                                    Expression::Value(Value::Literal(
                                                        Literal::Integer(2)
                                                    )),
                                                    Expression::Binary(BinaryExpr::Mul(MulExpr {
                                                        ops: vec![MulOperator::Multiply],
                                                        subexprs: vec![
                                                            Expression::Value(Value::Literal(
                                                                Literal::Integer(2)
                                                            )),
                                                            Expression::Value(Value::Literal(
                                                                Literal::Integer(2)
                                                            )),
                                                        ],
                                                    })),
                                                ],
                                            }
                                        ))),
                                        right: Box::new(Expression::Value(Value::Literal(
                                            Literal::Integer(6)
                                        ))),
                                    })),
                                    Expression::Binary(BinaryExpr::Rel(RelExpr {
                                        op: RelOperator::Equal,
                                        left: Box::new(Expression::Binary(BinaryExpr::Add(
                                            AddExpr {
                                                ops: vec![AddOperator::Minus],
                                                subexprs: vec![
                                                    Expression::Value(Value::Literal(
                                                        Literal::Integer(5)
                                                    )),
                                                    Expression::Binary(BinaryExpr::Mul(MulExpr {
                                                        ops: vec![MulOperator::Divide],
                                                        subexprs: vec![
                                                            Expression::Value(Value::Literal(
                                                                Literal::Integer(4)
                                                            )),
                                                            Expression::Value(Value::Literal(
                                                                Literal::Integer(2)
                                                            )),
                                                        ],
                                                    })),
                                                ],
                                            }
                                        ))),
                                        right: Box::new(Expression::Value(Value::Literal(
                                            Literal::Integer(3)
                                        ))),
                                    })),
                                ],
                            }))],
                        }),
                    ],
                }),
            }
        )]),
        const_defs: HashMap::from([(
            "SOME_CONSTANT".to_string(),
            ConstDef {
                name: "SOME_CONSTANT".to_string(),
                value: Box::new(Expression::Value(Value::Literal(Literal::Integer(1)))),
            }
        )]),
    }
);
