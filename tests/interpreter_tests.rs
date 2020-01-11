use tempe_lang::interpreter::errors::{ErrorKind, Result as InterpretingResult};
use tempe_lang::interpreter::eval_program;
use tempe_lang::interpreter::object::{Environment, FunctionObject, Object};
use tempe_lang::lexer::Lexer;
use tempe_lang::parser::Parser;
use tempe_lang::representations::ast::{
    BlockStatement, Expression, ExpressionOperator, Identifier, InfixOperationExpression, Statement,
};
use tempe_lang::representations::token::Literal;

fn parse_eval(input: &str) -> InterpretingResult<Object> {
    let mut lexer = Lexer::new(input);

    let program = Parser::new(&mut lexer)
        .parse_program()
        .map_err(|errors| format!("parse_program returned errors {:?}", errors))?;

    let mut env = Environment::new_with_builtin_functions();

    eval_program(&program, &mut env)
}

#[test]
fn eval_if_else_expressions() {
    let inputs_to_expected_objects = vec![
        ("jika (benar) { 10 }", Object::Integer(10)),
        ("jika (salah) { 10 }", Object::Null),
        ("jika (1) { 10 }", Object::Integer(10)),
        ("jika (1 < 2) { 10 }", Object::Integer(10)),
        ("jika (1 > 2) { 10 }", Object::Null),
        ("jika (1 > 2) { 10 } jika tidak { 20 }", Object::Integer(20)),
        ("jika (1 < 2) { 10 } jika tidak { 20 }", Object::Integer(10)),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_infix_operations() {
    let inputs_to_expected_objects = vec![
        ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
        ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
        ("-50 + 100 + -50", Object::Integer(0)),
        ("5 * 2 + 10", Object::Integer(20)),
        ("5 + 2 * 10", Object::Integer(25)),
        ("20 + 2 * -10", Object::Integer(0)),
        ("50 / 2 * 2 + 10", Object::Integer(60)),
        ("2 * (5 + 10)", Object::Integer(30)),
        ("3 * 3 * 3 + 10", Object::Integer(37)),
        ("3 * (3 * 3) + 10", Object::Integer(37)),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ("1 < 2", Object::Boolean(true)),
        ("1 > 2", Object::Boolean(false)),
        ("1 < 1", Object::Boolean(false)),
        ("1 > 1", Object::Boolean(false)),
        ("1 == 1", Object::Boolean(true)),
        ("1 != 1", Object::Boolean(false)),
        ("1 == 2", Object::Boolean(false)),
        ("1 != 2", Object::Boolean(true)),
        (
            "\"hello\" + \" \" + \"world\"",
            Object::Str("hello world".to_string()),
        ),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_integer_expressions() {
    let inputs_to_expected_objects = vec![
        ("42", Object::Integer(42)),
        ("-1", Object::Integer(-1)),
        ("-4", Object::Integer(-4)),
        ("--4", Object::Integer(4)),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_str_expressions() {
    let inputs_to_expected_objects = vec![
        ("\"Hello world!\"", Object::Str("Hello world!".to_string())),
        ("\"\\\"\"", Object::Str("\"".to_string())),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_boolean_expressions() {
    let inputs_to_expected_objects = vec![
        ("benar", Object::Boolean(true)),
        ("salah", Object::Boolean(false)),
        ("benar == benar", Object::Boolean(true)),
        ("salah == salah", Object::Boolean(true)),
        ("benar == salah", Object::Boolean(false)),
        ("benar != salah", Object::Boolean(true)),
        ("salah != benar", Object::Boolean(true)),
        ("(1 < 2) == benar", Object::Boolean(true)),
        ("(1 < 2) == salah", Object::Boolean(false)),
        ("(1 > 2) == benar", Object::Boolean(false)),
        ("(1 > 2) == salah", Object::Boolean(true)),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_bang_operators() {
    let inputs_to_expected_objects = vec![
        ("!benar", Object::Boolean(false)),
        ("!salah", Object::Boolean(true)),
        ("!2", Object::Boolean(false)),
        ("!!benar", Object::Boolean(true)),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_return_statements() {
    let inputs_to_expected_objects = vec![
        ("kembalikan 10;", Object::Integer(10)),
        ("kembalikan 10; 9;", Object::Integer(10)),
        ("kembalikan 2 * 5; 9;", Object::Integer(10)),
        ("9; kembalikan 2 * 5; 9;", Object::Integer(10)),
        (
            "
                jika (10 > 1) {
                    jika (10 > 1) {
                        kembalikan 10;
                    }

                    kembalikan 1;
                }
                ",
            Object::Integer(10),
        ),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn error_handling() {
    let inputs_to_expected_errors = vec![
        (
            "5 + benar;",
            ErrorKind::UnknownInfixOperator(
                ExpressionOperator::Plus,
                Object::Integer(5),
                Object::Boolean(true),
            ),
        ),
        (
            "5 + benar; 5;",
            ErrorKind::UnknownInfixOperator(
                ExpressionOperator::Plus,
                Object::Integer(5),
                Object::Boolean(true),
            ),
        ),
        (
            "-benar;",
            ErrorKind::UnknownPrefixOperator(ExpressionOperator::Minus, Object::Boolean(true)),
        ),
        (
            "benar + salah;",
            ErrorKind::UnknownInfixOperator(
                ExpressionOperator::Plus,
                Object::Boolean(true),
                Object::Boolean(false),
            ),
        ),
        (
            "5; benar + salah; 5",
            ErrorKind::UnknownInfixOperator(
                ExpressionOperator::Plus,
                Object::Boolean(true),
                Object::Boolean(false),
            ),
        ),
        (
            "\"hello\" - \"world\"",
            ErrorKind::UnknownInfixOperator(
                ExpressionOperator::Minus,
                Object::Str("hello".to_string()),
                Object::Str("world".to_string()),
            ),
        ),
        (
            "jika (10 > 1) { benar + salah; }",
            ErrorKind::UnknownInfixOperator(
                ExpressionOperator::Plus,
                Object::Boolean(true),
                Object::Boolean(false),
            ),
        ),
        (
            "
                jika (10 > 1) {
                    jika (10 > 1) {
                        kembalikan benar + salah;
                    }

                    kembalikan 1;
                }
                ",
            ErrorKind::UnknownInfixOperator(
                ExpressionOperator::Plus,
                Object::Boolean(true),
                Object::Boolean(false),
            ),
        ),
        (
            "hahasiapakamu",
            ErrorKind::IdentifierNotFound(Identifier::new(Literal("hahasiapakamu".to_string()))),
        ),
        (
            "[1, 2, 3][3]",
            ErrorKind::OutOfBoundsArrayIndex(
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                3,
            ),
        ),
        (
            "[1, 2, 3][-1]",
            ErrorKind::OutOfBoundsArrayIndex(
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                -1,
            ),
        ),
    ];

    for (input, expected_error) in inputs_to_expected_errors {
        let error = parse_eval(input).unwrap_err();

        assert_eq!(error.description(), expected_error.description());
    }
}

#[test]
fn eval_let_statements() {
    let inputs_to_expected_objects = vec![
        ("diketahui a = 5; a;", Object::Integer(5)),
        ("diketahui a = 5 * 5; a;", Object::Integer(25)),
        ("diketahui a = 5; diketahui b = a; b;", Object::Integer(5)),
        (
            "diketahui a = 5; diketahui b = a; diketahui c = a + b + 5; c;",
            Object::Integer(15),
        ),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_function_definition() {
    let input = "fungsi(x) { x + 2; };";

    let expected_object = Object::Function(Box::new(FunctionObject::new(
        vec![Identifier::new(Literal("x".to_string()))],
        BlockStatement::new(vec![Statement::Expression(Expression::InfixOperation(
            InfixOperationExpression::new(
                ExpressionOperator::Plus,
                Expression::Identifier(Identifier::new(Literal("x".to_string()))),
                Expression::Integer(2),
            ),
        ))]),
        Environment::new_with_builtin_functions(),
    )));

    let object = parse_eval(input).unwrap();

    assert_eq!(object, expected_object);
}

#[test]
fn eval_function_call() {
    let inputs_to_expected_objects = vec![
        (
            "diketahui identity = fungsi(x) { x; }; identity(5);",
            Object::Integer(5),
        ),
        (
            "diketahui identity = fungsi(x) { kembalikan x; }; identity(5);",
            Object::Integer(5),
        ),
        (
            "diketahui double = fungsi(x) { x * 2; }; double(5);",
            Object::Integer(10),
        ),
        (
            "diketahui add = fungsi(x, y) { x + y; }; add(5, 5);",
            Object::Integer(10),
        ),
        (
            "diketahui add = fungsi(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            Object::Integer(20),
        ),
        ("fungsi(x) { x; }(5)", Object::Integer(5)),
        (
            "
                    diketahui fibonacci = fungsi(n) {
                        jika (n < 3) {
                            jika(n == 0) { 0 }
                            jika tidak { 1 }
                        } jika tidak {
                            fibonacci(n-1) + fibonacci(n-2)
                        }
                    };
                    
                    fibonacci(10)
                ",
            Object::Integer(55),
        ),
        (
            "
                    diketahui newAdder = fungsi(x) { fungsi(y) { x + y }; };
                    diketahui addTwo = newAdder(2);
                    addTwo(2);
                ",
            Object::Integer(4),
        ),
        (
            // the sky is the limit!! (but I don't recommend writing such spaghetti code ever...)
            "
                    fungsi() {
                        fungsi(y) {
                            fungsi(z) { z*10 }(y)+1000
                        }
                    }()(2)
                ",
            Object::Integer(1020),
        ),
        (
            "
                    diketahui add = fungsi(a, b) { a + b };
                    diketahui applyFunc = fungsi(a, b, func) { func(a, b) };
                    applyFunc(2, 2, add);
                ",
            Object::Integer(4),
        ),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_builtin_function_call() {
    let inputs_to_expected_objects = vec![
        ("panjang(\"salam!\")", Object::Integer(6)),
        ("panjang(\"你好！\")", Object::Integer(3)),
        ("pertama([])", Object::Null),
        ("pertama([1])", Object::Integer(1)),
        ("pertama([1, 2])", Object::Integer(1)),
        ("terakhir([])", Object::Null),
        ("terakhir([1])", Object::Integer(1)),
        ("terakhir([1, 2])", Object::Integer(2)),
        ("sisa([])", Object::Null),
        ("sisa([1])", Object::Array(Vec::new())),
        ("sisa([1, 2])", Object::Array(vec![Object::Integer(2)])),
        (
            "sisa([1, 2, 3])",
            Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
        ),
        ("tambah([], 42)", Object::Array(vec![Object::Integer(42)])),
        (
            "tambah([42], 84)",
            Object::Array(vec![Object::Integer(42), Object::Integer(84)]),
        ),
        (
            "tambah([42, 84], 168)",
            Object::Array(vec![
                Object::Integer(42),
                Object::Integer(84),
                Object::Integer(168),
            ]),
        ),
        (
            // original example from the book Writing An Interpreter In Go
            "
                    diketahui map = fungsi(arr, f) {
                        diketahui iter = fungsi(arr, accumulated) {
                            jika (panjang(arr) == 0) {
                                accumulated
                            } jika tidak {
                                iter(sisa(arr), tambah(accumulated, f(pertama(arr))));
                            }
                        };
                    
                        iter(arr, []);
                    };
                    
                    diketahui a = [1, 2, 3, 4];
                    diketahui double = fungsi(x) { x * 2 };

                    map(a, double);
                ",
            Object::Array(vec![
                Object::Integer(2),
                Object::Integer(4),
                Object::Integer(6),
                Object::Integer(8),
            ]),
        ),
        (
            "
                    diketahui reduce = fungsi(arr, initial, f) {
                        diketahui iter = fungsi(arr, result) {
                            jika (panjang(arr) == 0) {
                                result
                            } jika tidak {
                                iter(sisa(arr), f(result, pertama(arr)));
                            }
                        };

                        iter(arr, initial);
                    };

                    diketahui sum = fungsi(arr) {
                        reduce(arr, 0, fungsi(initial, el) { initial + el });
                    };

                    sum([1, 2, 3, 4, 5]);
                ",
            Object::Integer(15),
        ),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_array_expressions() {
    let inputs_to_expected_objects = vec![
        (
            "[1, 2]",
            Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
        ),
        ("panjang([42, benar, \"hi\"])", Object::Integer(3)),
        ("[1, 2, 3][0]", Object::Integer(1)),
        ("[1, 2, 3][1]", Object::Integer(2)),
        ("[1, 2, 3][2]", Object::Integer(3)),
        ("diketahui i = 0; [1][i];", Object::Integer(1)),
        ("[1, 2, 3][1 + 1];", Object::Integer(3)),
        (
            "diketahui myArray = [1, 2, 3]; myArray[2];",
            Object::Integer(3),
        ),
        (
            "diketahui myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            Object::Integer(6),
        ),
        (
            "diketahui myArray = [1, 2, 3]; diketahui i = myArray[0]; myArray[i]",
            Object::Integer(2),
        ),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}

#[test]
fn eval_hashmap_literal() {
    let input = "
            diketahui two = \"two\";

            {
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                \"4\": 4,
            }
        ";

    let expected_object = Object::HashMap(
        vec![
            ("one".to_string(), Object::Integer(1)),
            ("two".to_string(), Object::Integer(2)),
            ("three".to_string(), Object::Integer(3)),
            ("4".to_string(), Object::Integer(4)),
        ]
        .iter()
        .cloned()
        .collect(),
    );

    let object = parse_eval(input).unwrap();

    assert_eq!(object, expected_object);
}

#[test]
fn eval_hashmap_index_expressions() {
    let inputs_to_expected_objects = vec![
        ("{\"foo\": 5}[\"foo\"]", Object::Integer(5)),
        ("{\"foo\": 5}[\"bar\"]", Object::Null),
        (
            "diketahui key = \"foo\"; {\"foo\": 5}[key]",
            Object::Integer(5),
        ),
        ("{}[\"foo\"]", Object::Null),
    ];

    for (input, expected_object) in inputs_to_expected_objects {
        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }
}
