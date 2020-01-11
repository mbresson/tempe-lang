use tempe_lang::lexer::Lexer;
use tempe_lang::parser::Parser;
use tempe_lang::representations::ast::{
    BlockStatement, ConditionalExpression, Expression, ExpressionOperator, FunctionCallExpression,
    FunctionExpression, Identifier, IndexOperationExpression, InfixOperationExpression,
    LetStatement, PrefixOperationExpression, Program, ReturnStatement, Statement,
};
use tempe_lang::representations::token::Literal;

fn parse(input: &str) -> Result<Program, String> {
    let mut lexer = Lexer::new(input);

    Parser::new(&mut lexer)
        .parse_program()
        .map_err(|errors| format!("parse_program returned errors {:?}", errors))
}

#[test]
fn let_statements() {
    let input = "
        diketahui x = 5;
        diketahui y = 10;
        diketahui foobar = 838383;
        diketahui z = benar;
        diketahui foobar_bar = foobar;
    ";

    let program = parse(input).unwrap();

    let expected_statements = vec![
        Statement::Let(LetStatement::new(
            Identifier::new(Literal("x".to_string())),
            Expression::Integer(5),
        )),
        Statement::Let(LetStatement::new(
            Identifier::new(Literal("y".to_string())),
            Expression::Integer(10),
        )),
        Statement::Let(LetStatement::new(
            Identifier::new(Literal("foobar".to_string())),
            Expression::Integer(838_383),
        )),
        Statement::Let(LetStatement::new(
            Identifier::new(Literal("z".to_string())),
            Expression::Boolean(true),
        )),
        Statement::Let(LetStatement::new(
            Identifier::new(Literal("foobar_bar".to_string())),
            Expression::Identifier(Identifier::new(Literal("foobar".to_string()))),
        )),
    ];

    for (statement_index, expected_statement) in expected_statements.iter().enumerate() {
        let statement = &program.statements[statement_index];

        assert_eq!(statement, expected_statement);
    }
}

#[test]
fn return_statements() {
    let input = "
        kembalikan 5;
        kembalikan 10;
        kembalikan 993322;
        kembalikan salah;
        kembalikan 3*2;
    ";

    let program = parse(input).unwrap();

    let expected_statements = vec![
        Statement::Return(ReturnStatement::new(Expression::Integer(5))),
        Statement::Return(ReturnStatement::new(Expression::Integer(10))),
        Statement::Return(ReturnStatement::new(Expression::Integer(993_322))),
        Statement::Return(ReturnStatement::new(Expression::Boolean(false))),
        Statement::Return(ReturnStatement::new(Expression::InfixOperation(
            InfixOperationExpression::new(
                ExpressionOperator::Multiply,
                Expression::Integer(3),
                Expression::Integer(2),
            ),
        ))),
    ];

    for (statement_index, expected_statement) in expected_statements.iter().enumerate() {
        let statement = &program.statements[statement_index];

        assert_eq!(statement, expected_statement);
    }
}

#[test]
fn identifier_expressions() {
    let input = "foobar;";

    let program = parse(input).unwrap();

    let expected_identifier_expression = Statement::Expression(Expression::Identifier(
        Identifier::new(Literal("foobar".to_string())),
    ));

    assert_eq!(program.statements[0], expected_identifier_expression);
}

#[test]
fn integer_expressions() {
    let input = "
        42;
        52
    ";

    let program = parse(input).unwrap();

    let expected_expressions = vec![
        Statement::Expression(Expression::Integer(42)),
        Statement::Expression(Expression::Integer(52)),
    ];

    assert_eq!(program.statements, expected_expressions);
}

#[test]
fn str_expressions() {
    let input = "
        \"selamat makan!!! tempe itu enak :) \";
        \"a string with an escaped double quote: \\\" (it's getting tricky!)\"
    ";

    let program = parse(input).unwrap();

    let expected_expressions = vec![
        Statement::Expression(Expression::Str(
            "selamat makan!!! tempe itu enak :) ".to_string(),
        )),
        Statement::Expression(Expression::Str(
            "a string with an escaped double quote: \" (it's getting tricky!)".to_string(),
        )),
    ];

    assert_eq!(program.statements, expected_expressions);
}

#[test]
fn boolean_expressions() {
    let input = "
        benar;
        salah
    ";

    let program = parse(input).unwrap();

    let expected_expressions = vec![
        Statement::Expression(Expression::Boolean(true)),
        Statement::Expression(Expression::Boolean(false)),
    ];

    assert_eq!(program.statements, expected_expressions);
}

#[test]
fn prefix_expressions() {
    let input = "
        !5;
        -15;
        !benar;
        !salah;
    ";

    let program = parse(input).unwrap();

    let expected_expressions = vec![
        Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression::new(
            ExpressionOperator::Bang,
            Expression::Integer(5),
        ))),
        Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression::new(
            ExpressionOperator::Minus,
            Expression::Integer(15),
        ))),
        Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression::new(
            ExpressionOperator::Bang,
            Expression::Boolean(true),
        ))),
        Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression::new(
            ExpressionOperator::Bang,
            Expression::Boolean(false),
        ))),
    ];

    assert_eq!(program.statements, expected_expressions);
}

#[test]
fn infix_expressions() {
    let input = "
        5 + 6;
        5 + ab;
        5 - 6;
        5 * 6;
        5 / 6;
        5 > 6;
        5 < 6;
        5 == 6;
        5 != 6;
        benar == benar;
        benar != salah;
        salah == salah;
    ";

    let program = parse(input).unwrap();

    let expected_expressions = vec![
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Plus,
            Expression::Integer(5),
            Expression::Integer(6),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Plus,
            Expression::Integer(5),
            Expression::Identifier(Identifier::new(Literal("ab".to_string()))),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Minus,
            Expression::Integer(5),
            Expression::Integer(6),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Multiply,
            Expression::Integer(5),
            Expression::Integer(6),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Divide,
            Expression::Integer(5),
            Expression::Integer(6),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::GreaterThan,
            Expression::Integer(5),
            Expression::Integer(6),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::LessThan,
            Expression::Integer(5),
            Expression::Integer(6),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Equal,
            Expression::Integer(5),
            Expression::Integer(6),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::NotEqual,
            Expression::Integer(5),
            Expression::Integer(6),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Equal,
            Expression::Boolean(true),
            Expression::Boolean(true),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::NotEqual,
            Expression::Boolean(true),
            Expression::Boolean(false),
        ))),
        Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Equal,
            Expression::Boolean(false),
            Expression::Boolean(false),
        ))),
    ];

    assert_eq!(program.statements, expected_expressions);
}

#[test]
fn infix_expressions_with_operator_precedence() {
    let inputs_to_expected = vec![
        ("-a * b", "((-a) * b);"),
        ("!-a", "(!(-a));"),
        ("a + b + c", "((a + b) + c);"),
        ("a + b - c", "((a + b) - c);"),
        ("a * b * c", "((a * b) * c);"),
        ("a * b / c", "((a * b) / c);"),
        ("a + b / c", "(a + (b / c));"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
        (
            "3 + 4; -5 * 5",
            "(3 + 4);\n\
             ((-5) * 5);",
        ),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
        ),
        ("benar", "benar;"),
        ("salah", "salah;"),
        ("3 > 5 == false", "((3 > 5) == false);"),
        ("3 < 5 == true", "((3 < 5) == true);"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);"),
        ("(5 + 5) * 2", "((5 + 5) * 2);"),
        ("2 / (5 + 5)", "(2 / (5 + 5));"),
        ("-(5) + 1", "((-5) + 1);"),
        ("-(5 + 5)", "(-(5 + 5));"),
        ("!(true == true)", "(!(true == true));"),
        ("a + fun() + d", "((a + fun()) + d);"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d);"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g));",
        ),
    ];

    for (input, expected) in inputs_to_expected {
        let program = parse(input).unwrap();
        assert_eq!(format!("{}", program), expected);
    }
}

#[test]
fn if_expression() {
    let input = "jika (x < y) { x }";

    let program = parse(input).unwrap();

    let expected_statements = vec![Statement::Expression(Expression::Conditional(
        ConditionalExpression::new(
            Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::LessThan,
                Expression::Identifier(Identifier::new(Literal("x".to_string()))),
                Expression::Identifier(Identifier::new(Literal("y".to_string()))),
            )),
            BlockStatement::new(vec![Statement::Expression(Expression::Identifier(
                Identifier::new(Literal("x".to_string())),
            ))]),
            None,
        ),
    ))];

    assert_eq!(program.statements, expected_statements);
}

#[test]
fn if_else_expression() {
    let input = "jika (x < y) { x } jika tidak { y }";

    let program = parse(input).unwrap();

    let expected_statements = vec![Statement::Expression(Expression::Conditional(
        ConditionalExpression::new(
            Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::LessThan,
                Expression::Identifier(Identifier::new(Literal("x".to_string()))),
                Expression::Identifier(Identifier::new(Literal("y".to_string()))),
            )),
            BlockStatement::new(vec![Statement::Expression(Expression::Identifier(
                Identifier::new(Literal("x".to_string())),
            ))]),
            Some(BlockStatement::new(vec![Statement::Expression(
                Expression::Identifier(Identifier::new(Literal("y".to_string()))),
            )])),
        ),
    ))];

    assert_eq!(program.statements, expected_statements);
}

#[test]
fn function_expression() {
    let input = "fungsi (x, y) { x + y; }";

    let program = parse(input).unwrap();

    let expected_statements = vec![Statement::Expression(Expression::Function(
        FunctionExpression::new(
            vec![
                Identifier::new(Literal("x".to_string())),
                Identifier::new(Literal("y".to_string())),
            ],
            BlockStatement::new(vec![Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Plus,
                    Expression::Identifier(Identifier::new(Literal("x".to_string()))),
                    Expression::Identifier(Identifier::new(Literal("y".to_string()))),
                ),
            ))]),
        ),
    ))];

    assert_eq!(program.statements, expected_statements);
}

#[test]
fn function_parameters() {
    let inputs_to_expected = vec![
        (
            "fungsi() {};",
            vec![Statement::Expression(Expression::Function(
                FunctionExpression::new(vec![], BlockStatement::new(vec![])),
            ))],
        ),
        (
            "fungsi(x) {};",
            vec![Statement::Expression(Expression::Function(
                FunctionExpression::new(
                    vec![Identifier::new(Literal("x".to_string()))],
                    BlockStatement::new(vec![]),
                ),
            ))],
        ),
        (
            "fungsi(x, y, z) {};",
            vec![Statement::Expression(Expression::Function(
                FunctionExpression::new(
                    vec![
                        Identifier::new(Literal("x".to_string())),
                        Identifier::new(Literal("y".to_string())),
                        Identifier::new(Literal("z".to_string())),
                    ],
                    BlockStatement::new(vec![]),
                ),
            ))],
        ),
    ];

    for (input, expected) in inputs_to_expected {
        let program = parse(input).unwrap();

        assert_eq!(program.statements, expected);
    }
}

#[test]
fn function_call_expression() {
    let input = "add(1, 2 * 3, 4 + 5); ";

    let program = parse(input).unwrap();

    let expected_statements = vec![Statement::Expression(Expression::FunctionCall(
        FunctionCallExpression::new(
            Expression::Identifier(Identifier::new(Literal("add".to_string()))),
            vec![
                Expression::Integer(1),
                Expression::InfixOperation(InfixOperationExpression::new(
                    ExpressionOperator::Multiply,
                    Expression::Integer(2),
                    Expression::Integer(3),
                )),
                Expression::InfixOperation(InfixOperationExpression::new(
                    ExpressionOperator::Plus,
                    Expression::Integer(4),
                    Expression::Integer(5),
                )),
            ],
        ),
    ))];

    assert_eq!(program.statements, expected_statements);
}

#[test]
fn array_expression() {
    let input = "[1, 2 * 9, 3 + 4]";

    let program = parse(input).unwrap();

    let expected_statements = vec![Statement::Expression(Expression::Array(vec![
        Expression::Integer(1),
        Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Multiply,
            Expression::Integer(2),
            Expression::Integer(9),
        )),
        Expression::InfixOperation(InfixOperationExpression::new(
            ExpressionOperator::Plus,
            Expression::Integer(3),
            Expression::Integer(4),
        )),
    ]))];

    assert_eq!(program.statements, expected_statements);
}

#[test]
fn array_index_expression() {
    let inputs_to_expected = vec![
        (
            "[1, 2][0]",
            vec![Statement::Expression(Expression::IndexOperation(
                IndexOperationExpression::new(
                    Expression::Integer(0),
                    Expression::Array(vec![Expression::Integer(1), Expression::Integer(2)]),
                ),
            ))],
        ),
        (
            "a[0]",
            vec![Statement::Expression(Expression::IndexOperation(
                IndexOperationExpression::new(
                    Expression::Integer(0),
                    Expression::Identifier(Identifier::new(Literal("a".to_string()))),
                ),
            ))],
        ),
    ];

    for (input, expected) in inputs_to_expected {
        let program = parse(input).unwrap();

        assert_eq!(program.statements, expected);
    }
}

#[test]
fn hash_literals() {
    let inputs_to_expected = vec![
        (
            "{\"one\": 1, \"two\": 2, \"three\": 3}",
            vec![Statement::Expression(Expression::HashLiteral(vec![
                (Expression::Str("one".to_string()), Expression::Integer(1)),
                (Expression::Str("two".to_string()), Expression::Integer(2)),
                (Expression::Str("three".to_string()), Expression::Integer(3)),
            ]))],
        ),
        (
            "{}",
            vec![Statement::Expression(Expression::HashLiteral(Vec::new()))],
        ),
        (
            "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}",
            vec![Statement::Expression(Expression::HashLiteral(vec![
                (
                    Expression::Str("one".to_string()),
                    Expression::InfixOperation(InfixOperationExpression::new(
                        ExpressionOperator::Plus,
                        Expression::Integer(0),
                        Expression::Integer(1),
                    )),
                ),
                (
                    Expression::Str("two".to_string()),
                    Expression::InfixOperation(InfixOperationExpression::new(
                        ExpressionOperator::Minus,
                        Expression::Integer(10),
                        Expression::Integer(8),
                    )),
                ),
                (
                    Expression::Str("three".to_string()),
                    Expression::InfixOperation(InfixOperationExpression::new(
                        ExpressionOperator::Divide,
                        Expression::Integer(15),
                        Expression::Integer(5),
                    )),
                ),
            ]))],
        ),
    ];

    for (input, expected) in inputs_to_expected {
        let program = parse(input).unwrap();

        assert_eq!(program.statements, expected);
    }
}
