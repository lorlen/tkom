#![cfg(test)]

use utf8_read::Reader;

use crate::{data::runtime::RuntimeValue, lexer::LexerImpl, parser::Parser};

use super::{executor::Executor, Visitor};

macro_rules! executor_test {
    ($name:ident, $text:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let lexer = LexerImpl::new(Reader::new(Box::new($text.as_bytes())));
            let mut parser = Parser::new(Box::new(lexer));
            let result = Executor::default().visit_program(&parser.parse());
            assert_eq!(result, $expected);
        }
    };
    ($name:ident, $text:expr, $expected:expr, should_panic) => {
        #[test]
        #[should_panic]
        fn $name() {
            let lexer = LexerImpl::new(Reader::new(Box::new($text.as_bytes())));
            let mut parser = Parser::new(Box::new(lexer));
            let result = Executor::default().visit_program(&parser.parse());
            assert_eq!(result, $expected);
        }
    };
}

executor_test!(
    test_arithmetic_expr,
    "
    fn main() {
        return 2 + 2 * 2 + 3 - 4 / 2;
    }
    ",
    RuntimeValue::from(7)
);

executor_test!(
    test_relational_expr,
    "
    fn main() {
        return 2 > 1 & 3 < 4;
    }
    ",
    RuntimeValue::from(true)
);

executor_test!(
    test_yield,
    "
    fn main() {
        return {
            yield {
                yield 1 + 1;
            };
        };
    }
    ",
    RuntimeValue::from(2)
);

executor_test!(
    test_for,
    "
    fn main() {
        let a = 0;
        for i in 1 ..= 10 {
            a += i;
        }
        return a;
    }
    ",
    RuntimeValue::from(55)
);

executor_test!(
    test_call,
    "
    fn test() {
        return 1;
    }

    fn main() {
        let a = test();
        return a;
    }
    ",
    RuntimeValue::from(1)
);

executor_test!(
    test_complex_control_flow,
    "
    fn test2() {
        return 1;
    }

    fn test() {
        let a = {
            let b = 1;
            yield if 1 < 2 {
                yield b + test2();
            } else {
                yield 0;
            };
        };

        return a + 1;
    }

    fn main() {
        let a = test();
        return a;
    }
    ",
    RuntimeValue::from(3)
);

executor_test!(
    test_match,
    "
    fn main() {
        return match 3 {
            1 | 2 -> 5,
            a -> a
        };
    }
    ",
    RuntimeValue::from(3)
);

executor_test!(
    test_recursion,
    "
    fn factorial(n) {
        return match n {
            0 -> 1,
            n -> n * factorial(n - 1)
        };
    }

    fn main() {
        return factorial(5);
    }
    ",
    RuntimeValue::from(120)
);

executor_test!(
    test_enum,
    "
    enum TestEnum {
        Variant1,
        Variant2(TestEnum2)
    }

    enum TestEnum2 {
        Variant1(string),
        Variant2(int)
    }

    fn main() {
        let data = new(\"TestEnum\", \"Variant2\", new(\"TestEnum2\", \"Variant2\", 1));

        return match data {
            TestEnum.Variant1 -> 0,
            TestEnum.Variant2(TestEnum2.Variant1(s)) -> s,
            TestEnum.Variant2(TestEnum2.Variant2(i)) -> i,
        };
    }
    ",
    RuntimeValue::from(1)
);

executor_test!(
    test_struct,
    "
    struct TestStruct {
        int a,
        int b
    }

    struct TestStruct2 {
        TestStruct s,
        int c
    }

    fn main() {
        let data = new(\"TestStruct2\", new(\"TestStruct\", 1, 2), 3);

        println(data.s.a);
        data.s.a = 2;
        return data.s.a;
    }
    ",
    RuntimeValue::from(2)
);

executor_test!(
    test_break,
    "
    fn main() {
        let a = 0;

        for i in 1 .. 10 {
            if i > 5 {
                break;
            }
            a += i;
        }

        return a;
    }
    ",
    RuntimeValue::from(15)
);

executor_test!(
    test_continue,
    "
    fn main() {
        let a = 0;

        for i in 1 .. 10 {
            if i < 5 {
                continue;
            }
            a += i;
        }

        return a;
    }
    ",
    RuntimeValue::from(35)
);
