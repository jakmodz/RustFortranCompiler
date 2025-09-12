#[cfg(test)]
mod tests_lexer {
    use crate::lexer::lexer::Lexer;
    use super::*;
    use crate::lexer::token::{Token, TokenType, Keyword};

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("+ - * / = == /= < <= > >=".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token_type, TokenType::Plus);
        assert_eq!(tokens[1].token_type, TokenType::Minus);
        assert_eq!(tokens[2].token_type, TokenType::Star);
        assert_eq!(tokens[3].token_type, TokenType::Slash);
        assert_eq!(tokens[4].token_type, TokenType::Assign);
        assert_eq!(tokens[5].token_type, TokenType::Eq);
        assert_eq!(tokens[6].token_type, TokenType::Ne);
        assert_eq!(tokens[7].token_type, TokenType::Lt);
        assert_eq!(tokens[8].token_type, TokenType::Le);
        assert_eq!(tokens[9].token_type, TokenType::Gt);
        assert_eq!(tokens[10].token_type, TokenType::Ge);
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Lexer::new("123 456.78 1.23e-4 2.5E+10".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].token_type, TokenType::Int(123)));
        assert!(matches!(tokens[1].token_type, TokenType::Double(_)));
        assert!(matches!(tokens[2].token_type, TokenType::Double(_)));
        assert!(matches!(tokens[3].token_type, TokenType::Double(_)));
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("PROGRAM INTEGER REAL IF THEN ELSE END".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token_type, TokenType::Keyword(Keyword::Program));
        assert_eq!(tokens[1].token_type, TokenType::Keyword(Keyword::Integer));
        assert_eq!(tokens[2].token_type, TokenType::Keyword(Keyword::Real));
        assert_eq!(tokens[3].token_type, TokenType::Keyword(Keyword::If));
        assert_eq!(tokens[4].token_type, TokenType::Keyword(Keyword::Then));
        assert_eq!(tokens[5].token_type, TokenType::Keyword(Keyword::Else));
        assert_eq!(tokens[6].token_type, TokenType::Keyword(Keyword::End));
    }

    #[test]
    fn test_logical_operators() {
        let mut lexer = Lexer::new(".AND. .OR. .NOT. .TRUE. .FALSE.".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token_type, TokenType::And);
        assert_eq!(tokens[1].token_type, TokenType::Or);
        assert_eq!(tokens[2].token_type, TokenType::Not);
        assert_eq!(tokens[3].token_type, TokenType::Keyword(Keyword::True));
        assert_eq!(tokens[4].token_type, TokenType::Keyword(Keyword::False));
    }

    #[test]
    fn test_strings() {
        let mut lexer = Lexer::new("'hello' \"world\"".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].token_type, TokenType::Character(ref s) if s == "hello"));
        assert!(matches!(tokens[1].token_type, TokenType::Character(ref s) if s == "world"));
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("variable_name x123 _test".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].token_type, TokenType::Identifier(ref s) if s == "variable_name"));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(ref s) if s == "x123"));
        assert!(matches!(tokens[2].token_type, TokenType::Identifier(ref s) if s == "_test"));
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("x = 5 ! this is a comment\ny = 10".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should skip the comment and continue with y = 10
        assert!(matches!(tokens[0].token_type, TokenType::Identifier(ref s) if s == "x"));
        assert_eq!(tokens[1].token_type, TokenType::Assign);
        assert!(matches!(tokens[2].token_type, TokenType::Int(5)));
        assert!(matches!(tokens[3].token_type, TokenType::Identifier(ref s) if s == "y"));
        assert_eq!(tokens[4].token_type, TokenType::Assign);
        assert!(matches!(tokens[5].token_type, TokenType::Int(10)));
    }

    #[test]
    fn test_delimiters() {
        let mut lexer = Lexer::new("( ) [ ] , : :: ; %".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens[0].token_type, TokenType::LeftParen);
        assert_eq!(tokens[1].token_type, TokenType::RightParen);
        assert_eq!(tokens[2].token_type, TokenType::LeftBracket);
        assert_eq!(tokens[3].token_type, TokenType::RightBracket);
        assert_eq!(tokens[4].token_type, TokenType::Comma);
        assert_eq!(tokens[5].token_type, TokenType::Colon);
        assert_eq!(tokens[6].token_type, TokenType::ColonColon);
        assert_eq!(tokens[7].token_type, TokenType::Semicolon);
        assert_eq!(tokens[8].token_type, TokenType::Percent);
    }

    #[test]
    fn test_invalid_number() {
        let mut lexer = Lexer::new("123e".to_string());
        let result = lexer.tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn test_unterminated_string() {
        let mut lexer = Lexer::new("'hello".to_string());
        let result = lexer.tokenize();
        assert!(result.is_err());
    }
}
#[cfg(test)]
mod tests_parser {
    use crate::parser::program_unit::ProgramUnit;
    use super::*;
    use crate::parser::parser::Parser;
    use crate::lexer::{lexer::Lexer, token::{TokenType, Keyword}};
    use crate::parser::ast::{Declaration, Expr, Literal, Stmt, VarType};
    use crate::parser::parsing_error::ParsingError;
    fn parse_program(source: &str) -> Result<ProgramUnit, ParsingError> {
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse_all()
    }

    fn parse_expression(source: &str) -> Result<Box<Expr>, ParsingError> {
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse_expression()
    }

    #[test]
    fn test_simple_program() {
        let source = r#"
PROGRAM test
INTEGER :: x = 5
PRINT x
END PROGRAM test
"#;
        let result = parse_program(source);
        assert!(result.is_ok());

        if let Ok(ProgramUnit::Program { program }) = result {
            assert_eq!(program.name, "test");
            assert_eq!(program.declarations.len(), 1);
            assert_eq!(program.stmts.len(), 1);
        }
    }

    #[test]
    fn test_variable_declarations() {
        let source = r#"
PROGRAM test
INTEGER :: x, y = 10
REAL :: z = 3.14
CHARACTER(20) :: name
END PROGRAM test
"#;
        let result = parse_program(source);
        assert!(result.is_ok());

        if let Ok(ProgramUnit::Program { program }) = result {
            assert_eq!(program.declarations.len(), 4); // x, y, z, name
        }
    }

    #[test]
    fn test_parameter_declaration() {
        let source = r#"
PROGRAM test
INTEGER, PARAMETER :: MAX_SIZE = 100
END PROGRAM test
"#;
        let result = parse_program(source);
        assert!(result.is_ok());

        if let Ok(ProgramUnit::Program { program }) = result
        {
            assert_eq!(program.declarations.len(), 1);
            if let Declaration::Parameter { name, var_type, value: _ } = &program.declarations[0]
            {
                assert_eq!(name, "MAX_SIZE");
                assert!(matches!(var_type, VarType::Integer));
            }
            else
            {
                panic!("Expected parameter declaration");
            }
        }
    }

    #[test]
    fn test_arithmetic_expressions()
    {
        let expr = parse_expression("2 + 3 * 4").unwrap();
        // Should parse as 2 + (3 * 4) due to precedence
        if let Expr::BinaryOp { left, op, right } = expr.as_ref() {
            assert_eq!(op.lexeme(), "+");
            assert!(matches!(left.as_ref(), Expr::Literal { value: Literal::Int(2) }));
            assert!(matches!(right.as_ref(), Expr::BinaryOp { .. }));
        } else {
            panic!("Expected binary operation");
        }
    }

    #[test]
    fn test_comparison_expressions() {
        let expr = parse_expression("x >= y").unwrap();
        if let Expr::BinaryOp { left, op, right } = expr.as_ref() {
            assert_eq!(op.lexeme(), ">=");
            assert!(matches!(left.as_ref(), Expr::Variable { name } if name == "x"));
            assert!(matches!(right.as_ref(), Expr::Variable { name } if name == "y"));
        } else {
            panic!("Expected comparison operation");
        }
    }

    #[test]
    fn test_logical_expressions() {
        let expr = parse_expression("a .AND. b .OR. c").unwrap();
        // Should parse as (a .AND. b) .OR. c due to precedence
        if let Expr::BinaryOp { left, op, right } = expr.as_ref() {
            assert_eq!(op.lexeme(), ".OR.");
            assert!(matches!(left.as_ref(), Expr::BinaryOp { .. }));
            assert!(matches!(right.as_ref(), Expr::Variable { name } if name == "c"));
        } else {
            panic!("Expected logical operation");
        }
    }

    #[test]
    fn test_if_statement() {
        let source = r#"
PROGRAM test
INTEGER :: x = 5
IF (x > 0) THEN
    PRINT x
ELSEIF (x == 0) THEN
    PRINT 0
ELSE
    PRINT -1
END IF
END PROGRAM test
"#;
        let result = parse_program(source);
        assert!(result.is_ok());

        if let Ok(ProgramUnit::Program { program }) = result {
            assert_eq!(program.stmts.len(), 1);
            if let Stmt::If { init_if, else_ifs, else_last } = &program.stmts[0] {
                assert_eq!(init_if.statements.len(), 1);
                assert_eq!(else_ifs.len(), 1);
                assert!(else_last.is_some());
            } else {
                panic!("Expected if statement");
            }
        }
    }

    #[test]
    fn test_assignment_statement() {
        let source = r#"
PROGRAM test
INTEGER :: x
x = 42
END PROGRAM test
"#;
        let result = parse_program(source);
        assert!(result.is_ok());

        if let Ok(ProgramUnit::Program { program }) = result {
            assert_eq!(program.stmts.len(), 1);
            if let Stmt::Assignment { var_name, expr: _ } = &program.stmts[0] {
                assert_eq!(var_name, "x");
            } else {
                panic!("Expected assignment statement");
            }
        }
    }

    #[test]
    fn test_print_statement() {
        let source = r#"
PROGRAM test
PRINT 'Hello, World!'
END PROGRAM test
"#;
        let result = parse_program(source);
        assert!(result.is_ok());

        if let Ok(ProgramUnit::Program { program }) = result {
            assert_eq!(program.stmts.len(), 1);
            assert!(matches!(program.stmts[0], Stmt::Print { .. }));
        }
    }

    #[test]
    fn test_grouping_expressions() {
        let expr = parse_expression("(2 + 3) * 4").unwrap();
        if let Expr::BinaryOp { left, op, right } = expr.as_ref() {
            assert_eq!(op.lexeme(), "*");
            assert!(matches!(left.as_ref(), Expr::Grouping { .. }));
            assert!(matches!(right.as_ref(), Expr::Literal { value: Literal::Int(4) }));
        } else {
            panic!("Expected binary operation with grouping");
        }
    }

    #[test]
    fn test_literal_values() {
        let cases = vec![
            ("123", Literal::Int(123)),
            ("3.14", Literal::Double(3.14)),
            ("'hello'", Literal::Character("hello".to_string())),
            (".TRUE.", Literal::Logical(true)),
            (".FALSE.", Literal::Logical(false)),
        ];

        for (source, expected) in cases {
            let expr = parse_expression(source).unwrap();
            if let Expr::Literal { value } = expr.as_ref() {
                match (value, &expected) {
                    (Literal::Int(a), Literal::Int(b)) => assert_eq!(a, b),
                    (Literal::Double(a), Literal::Double(b)) => assert!((a - b).abs() < f64::EPSILON),
                    (Literal::Character(a), Literal::Character(b)) => assert_eq!(a, b),
                    (Literal::Logical(a), Literal::Logical(b)) => assert_eq!(a, b),
                    _ => panic!("Literal type mismatch"),
                }
            } else {
                panic!("Expected literal expression");
            }
        }
    }

    #[test]
    fn test_invalid_syntax() {
        let invalid_programs = vec![
            "PROGRAM", // Missing program name
            "PROGRAM test\nINTEGER ::\nEND PROGRAM test", // Missing variable name
            "PROGRAM test\nIF () THEN\nEND IF\nEND PROGRAM test", // Empty condition
            "PROGRAM test\n=\nEND PROGRAM test", // Invalid assignment
        ];

        for source in invalid_programs {
            let result = parse_program(source);
            assert!(result.is_err(), "Expected parsing error for: {}", source);
        }
    }
}



#[cfg(test)]
mod tests_compiler
{
    use crate::compiler::compiler::{generate_object_file, Compiler};
    use crate::lexer::lexer::Lexer;
    use crate::parser::parser::Parser;

    fn compile_program(source: &str) -> anyhow::Result<Compiler> {
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program_unit = parser.parse_all().unwrap();

        let mut compiler = Compiler::new("test_module")?;
        compiler.compile(vec![program_unit])?;
        Ok(compiler)
    }

    #[test]
    fn test_simple_program_compilation() {
        let source = r#"
PROGRAM test
INTEGER :: x = 42
PRINT x
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_arithmetic_compilation() {
        let source = r#"
PROGRAM test
INTEGER :: x = 10
INTEGER :: y = 20
INTEGER :: z
z = x + y
PRINT z
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_real_arithmetic_compilation() {
        let source = r#"
PROGRAM test
REAL :: x = 3.14
REAL :: y = 2.71
REAL :: z
z = x * y
PRINT z
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_statement_compilation() {
        let source = r#"
PROGRAM test
INTEGER :: x = 5
IF (x > 0) THEN
    PRINT x
ELSE
    PRINT 0
END IF
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_if_compilation() {
        let source = r#"
PROGRAM test
INTEGER :: x = 5
INTEGER :: y = 10
IF (x > y) THEN
    PRINT x
ELSEIF (x == y) THEN
    PRINT 0
ELSE
    PRINT y
END IF
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_logical_operations_compilation() {
        let source = r#"
PROGRAM test
INTEGER :: x = 5
INTEGER :: y = 10
IF (x > 0 .AND. y > 0) THEN
    PRINT 0
ELSE
   PRINT 1
END IF
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_conversion() {
        let source = r#"
PROGRAM test
INTEGER :: i = 5
REAL :: r = 3.14
REAL :: result
result = i + r
PRINT result
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parameter_compilation() {
        let source = r#"
PROGRAM test
INTEGER, PARAMETER :: MAX_VAL = 100
INTEGER :: x
x = MAX_VAL
PRINT x
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }
    //not implemented yet
    //     #[test]
    //     fn test_character_compilation() {
    //         let source = r#"
    // PROGRAM test
    // CHARACTER(10) :: greeting = 'Hello'
    // PRINT greeting
    // END PROGRAM test
    // "#;
    //         let result = compile_program(source);
    //         assert!(result.is_ok());
    //     }

    #[test]
    fn test_comparison_operations() {
        let source = r#"
PROGRAM test
INTEGER :: x = 5
INTEGER :: y = 10
IF (x < y) THEN
    PRINT 1
END IF
IF (x >= 5) THEN
    PRINT 2
END IF
IF (x /= y) THEN
    PRINT 3
END IF
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_undefined_variable_error() {
        let source = r#"
PROGRAM test
x = 5
PRINT x
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_object_file_generation() {
        let source = r#"
PROGRAM test
INTEGER :: x = 42
PRINT x
END PROGRAM test
"#;
        let compiler = compile_program(source).unwrap();
        let result = generate_object_file(compiler, "test_output.obj");
        assert!(result.is_ok());

        // Clean up the test file
        std::fs::remove_file("test_output.obj").ok();
    }

    #[test]
    fn test_multiple_declarations() {
        let source = r#"
PROGRAM test
INTEGER :: a = 1, b = 2, c = 3
REAL :: x, y = 1.5, z
x = a + b
z = x * y
PRINT z
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_expression_precedence() {
        let source = r#"
PROGRAM test
INTEGER :: result
result = 2 + 3 * 4
PRINT result
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_nested_expressions() {
        let source = r#"
PROGRAM test
INTEGER :: x = 5
INTEGER :: y = 10
INTEGER :: z
z = (x + y) * (x - y)
PRINT z
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_logical_values() {
        let source = r#"
PROGRAM test
LOGICAL :: flag = .TRUE.
IF (flag) THEN
    PRINT 1
ELSE
    PRINT 0
END IF
END PROGRAM test
"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_while_loop_compilation() {
        let source = r#"
        PROGRAM test
         INTEGER :: i = 1
          DO WHILE (i <= 5)
               print  i
               i = i + 1
          END DO
        END PROGRAM test
                "#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }
    #[test]
    fn test_nested_while_loop_compilation() {
        let source = r#"
        PROGRAM test
         INTEGER :: i = 1
         INTEGER :: j = 1
          DO WHILE (i <= 5)
           DO WHILE (j <= 5)
               print  j
               j = j + 1
            END DO
            i = i + 1
            print i
          END DO
        END PROGRAM test
                "#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }
    #[test]
    fn test_for_loop_compilation() {
        let source = r#"
        PROGRAM test
         INTEGER :: i
          DO i = 1, 5
               print  i
          END DO
        END PROGRAM test
                "#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }
    #[test]
    fn test_for_loop_with_step_compilation() {
        let source = r#"
        PROGRAM test
         INTEGER :: i
          DO i = 1, 5 , 1
               print  i
          END DO
        END PROGRAM test
                "#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }
    #[test]
    fn test_for_loop_with_step_and_condition_compilation() {
        let source = r#"
        PROGRAM test
        INTEGER :: x = 5
        if (.NOT.(x <= 5)) then
        print x
         end if
        END PROGRAM test"#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }
    #[test]
    fn test_infinite_loop_compilation() {
        let source = r#"
        PROGRAM test
          DO
               print  'infinite loop'
          END DO
        END PROGRAM test
                "#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }
    #[test]
    fn test_exit_statement_compilation() {
        let source = r#"
        PROGRAM test
         INTEGER :: i = 1
          DO WHILE (i <= 5)
               if (i == 3) exit
               print  i
               i = i + 1
          END DO
        END PROGRAM test
                "#;
        let result = compile_program(source);
        assert!(result.is_ok());
    }
    #[test]
    fn test_exit_statement_outside_loop() {
        let source = r#"
        PROGRAM test
            EXIT
        END PROGRAM test
                "#;
        let result = compile_program(source);
        assert!(result.is_err());
    }
}