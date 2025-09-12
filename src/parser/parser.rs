use crate::lexer::token::*;
use crate::parser::parsing_error::ParsingError;
use crate::parser::ast::{Declaration, Expr, If, Literal, Stmt, VarType};
use crate::parser::program_unit::*;
use crate::Common::type_resolver::resolve_simple_type;

pub struct Parser
{
    tokens: Vec<Token>,
    current: usize,
}


impl Parser
{
    pub fn new(tokens: Vec<Token>)->Self
    {
        Self { tokens, current: 0 }
    }

    fn is_at_end(&self) ->bool
    {
        self.current >= self.tokens.len()
    }
    fn previous(&self) ->&Token
    {
        &self.tokens[self.current - 1]
    }
    fn advance(&mut self) ->Option<&Token>
    {
        if !self.is_at_end()
        {
            self.current += 1;
            Some(self.previous())
        }
        else
        {
            None
        }
    }
    fn check_keyword(&mut self, keyword: Keyword) -> bool
    {
        if let Some(token) = self.current_token()
        {
            matches!(token.token_type.clone(), TokenType::Keyword(k) if k == keyword)
        }
        else
        {
            false
        }
    }
    fn is_declaration_keyword(&self) ->bool
    {
        if let Some(token) = self.current_token()
        {
            matches!(token.token_type.clone(),
                TokenType::Keyword(Keyword::Integer) |
                TokenType::Keyword(Keyword::Real) |
                TokenType::Keyword(Keyword::DoublePrecision) |
                TokenType::Keyword(Keyword::Complex) |
                TokenType::Keyword(Keyword::Logical) |
                TokenType::Keyword(Keyword::Character)
            )
        }
        else
        {
            false
        }
    }

    fn current_token(& self) ->Option<&Token>
    {
        if self.is_at_end()
        {
            None
        }
        else
        {
            Some(&self.tokens[self.current])
        }
    }
    fn match_keyword(&mut self, keyword: Keyword) -> bool
    {
        if let Some(token) = self.current_token()
        {
            if matches!(&token.token_type, TokenType::Keyword(k) if *k == keyword)
            {
                self.advance();
                return true;
            }
        }
        false
    }
    fn peek_next_is_keyword(&self, keyword: Keyword) -> bool
    {
        if self.current + 1 < self.tokens.len()
        {
            if let TokenType::Keyword(k) = &self.tokens[self.current + 1].token_type
            {
                return *k == keyword;
            }
        }
        false
    }

    fn consume(&mut self, expected: TokenType,msg:String)->Result<&Token, ParsingError>
    {
        if let Some(token) = self.current_token()
        {
            if std::mem::discriminant(&token.token_type) == std::mem::discriminant(&expected)
            {
                return Ok(self.advance().ok_or(ParsingError::EndOfInput)?);
            }
            else
            {
                return Err(ParsingError::SyntaxError(token.clone(),msg));
            }
        }
        Err(ParsingError::EndOfInput)
    }
    fn match_tokens(&mut self, types: &[TokenType])->bool
    {

        if let Some(token) = self.current_token()
        {
            for excepted in types
            {
                if  std::mem::discriminant(excepted) == std::mem::discriminant(&token.token_type)
                {
                    self.advance();
                    return true;
                }
            }
        }
        false
    }

    pub fn parse(&mut self)->Result<Vec<Box<Stmt>>, ParsingError>
    {
        let mut statements = Vec::new();

        while !self.is_at_end()
        {
            if let Some(token) = self.current_token()
            {
                if matches!(token.token_type, TokenType::Eof)
                {
                    break;
                }
            }
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }


        Ok(statements)
    }
    pub fn get_var_type(&mut self)->Result<VarType,ParsingError>
    {
        let var_type_token = self.current_token().ok_or(ParsingError::EndOfInput)?.clone();
        self.advance();
         self.parse_type_with_parameters(&self.previous().clone())
    }
    pub fn parse_all(&mut self)->Result<ProgramUnit, ParsingError>
    {
        self.parse_program()
    }
    fn parse_statement(&mut self) ->Result<Box<Stmt>,ParsingError>
    {

        if self.check_keyword(Keyword::Print)
        {
            self.advance();
            return self.parse_print();
        }
        else if self.check_keyword(Keyword::If)
        {
            return self.parse_if_statement();
        }
        else if self.match_keyword(Keyword::Do)
        {
           if self.check_keyword(Keyword::While)
           {
               return self.parse_while_loop_stmt();
           }
           return self.parse_do_for_stmt();
        }
        else if self.match_keyword(Keyword::Exit)
        {
            return Ok(Box::new(Stmt::Exit));
        }



        self.parse_assigment()
    }

    fn parse_do_for_stmt(&mut self) -> Result<Box<Stmt>, ParsingError> {

        if !self.match_tokens(&[TokenType::Identifier(String::from(""))])
        {
            return Err(ParsingError::UnexpectedToken(self.current_token().unwrap().clone()));
        }
        let var_name = self.previous().clone().lexeme;


        self.consume(TokenType::Assign, "Expected '=' after variable name".to_string())?;

        let start_expr = self.parse_expression()?;

        self.consume(TokenType::Comma, "Expected ',' after assigment".to_string())?;

        let end_expr = self.parse_expression()?;

        let step = if self.match_tokens(&[TokenType::Comma])
        {
            Some(*self.parse_expression()?)
        }
        else
        {
            None
        };


        let mut stmts = Vec::new();
        while !self.check_keyword(Keyword::End)
        {
            if self.is_at_end()
            {
                return Err(ParsingError::SyntaxError(self.previous().clone(),"Expected 'END DO' to close DO loop".to_string()));
            }
            let stmt = self.parse_statement()?;
            stmts.push(*stmt);
        }

        self.consume(TokenType::Keyword(Keyword::End), "Expected 'END' to close DO loop".to_string())?;
        self.consume(TokenType::Keyword(Keyword::Do), "Expected 'DO' after 'END'".to_string())?;

        let do_for_stmt = Stmt::DoFor
        {
            var_name,
            start: *start_expr,
            end: *end_expr,
            step,
            statements: stmts,
        };

        Ok(Box::new(do_for_stmt))
    }
    fn parse_while_loop_stmt(&mut self)->Result<Box<Stmt>,ParsingError>
    {
        self.consume(TokenType::Keyword(Keyword::While),"Expected 'WHILE' after 'DO'".to_string())?;
        self.consume(TokenType::LeftParen,"Expected '(' after 'WHILE'".to_string())?;
        let condition = self.parse_expression()?;
        self.consume(TokenType::RightParen,"Expected ')' after condition".to_string())?;


        let mut stmts = Vec::new();

        while !self.check_keyword(Keyword::End)
        {
            let stmt = self.parse_statement()?;
            stmts.push(*stmt);
        }

        self.consume(TokenType::Keyword(Keyword::End),"Expected 'END' to close DO WHILE loop".to_string())?;
        self.consume(TokenType::Keyword(Keyword::Do),"Expected 'DO' after 'END'".to_string())?;


        let do_while_stmt = Stmt::DoWhile{cond:*condition,statements:stmts};
        Ok(Box::new(do_while_stmt))
    }
    fn parse_assigment(&mut self)->Result<Box<Stmt>,ParsingError>
    {
        if !self.match_tokens(&[TokenType::Identifier(String::from(""))])
        {
            return Err(ParsingError::UnexpectedToken(self.current_token().unwrap().clone()));
        }
        let var_name = self.previous().clone();
        self.consume(TokenType::Assign,"Expected '=' after variable name".to_string())?;
        let expr = self.parse_expression()?;
        let assignment = Stmt::Assignment{var_name:var_name.lexeme,expr};
        Ok(Box::new(assignment))
    }
    fn parse_print(&mut self)->Result<Box<Stmt>,ParsingError>
    {
        let expr = self.parse_expression()?;
        let print_stmt = Stmt::Print{expr};
         Ok(Box::new(print_stmt))
    }
    fn parse_program(&mut self) -> Result<ProgramUnit, ParsingError>
    {
        self.consume(TokenType::Keyword(Keyword::Program), "Expected 'PROGRAM'".to_string())?;

        if !matches!(self.current_token().unwrap().token_type, TokenType::Identifier(_))
        {
            return Err(ParsingError::UnexpectedToken(self.current_token().unwrap().clone()));
        }

        self.advance();
        let program_name = self.previous().lexeme.clone();
        let mut program = Program::new(program_name.clone(), Vec::new(),Vec::new());


        while !self.is_at_end()
        {
            if let Some(token) = self.current_token()
            {

                if matches!(token.token_type, TokenType::Keyword(Keyword::End))
                {
                    break;
                }
                if self.is_declaration_keyword()
                {
                    let declarations = self.parse_declaration()?;
                    for declaration in declarations
                    {
                        program.declarations.push(declaration);
                    }
                    continue;
                }
            }
            program.stmts.push(*self.parse_statement()?);
        }

        self.consume(TokenType::Keyword(Keyword::End), "Expected 'END' to close program".to_string())?;
        self.consume(TokenType::Keyword(Keyword::Program), "Expected 'PROGRAM' after 'END'".to_string())?;
        self.consume(TokenType::Identifier(program_name), "Expected program name after 'END PROGRAM'".to_string())?;

        Ok(ProgramUnit::Program {program})
    }
    fn parse_type_with_parameters(&mut self,token:&Token)->Result<VarType,ParsingError>
    {
        match &token.token_type
        {
            TokenType::Keyword(Keyword::Integer) |
            TokenType::Keyword(Keyword::Real) |
            TokenType::Keyword(Keyword::Logical) =>
                {
                    let var_type = resolve_simple_type(&token.lexeme).ok_or(ParsingError::UnexpectedToken(token.clone()))?;
                    Ok(var_type)
                }
            TokenType::Keyword(Keyword::Character) =>
                {
                    let mut len = 1;
                    if self.match_tokens(&[TokenType::LeftParen])
                    {
                        if !self.match_tokens(&[TokenType::Int(0)])
                        {
                            return Err(ParsingError::UnexpectedToken(self.current_token().unwrap().clone()));
                        }
                        let len_token = self.previous().clone();
                        if let TokenType::Int(value) = len_token.token_type
                        {
                            len = value as usize;
                        }
                        self.consume(TokenType::RightParen,"Expected ')' after character length".to_string())?;
                    }
                    Ok(VarType::Character{len})
                }
            TokenType::Keyword(Keyword::Complex) =>
                {
                    Ok(VarType::Complex)
                }
            _ => Err(ParsingError::UnexpectedToken(token.clone()))
        }
    }



    fn parse_if_statement(&mut self) -> Result<Box<Stmt>, ParsingError>
    {
        self.consume(TokenType::Keyword(Keyword::If), "Expected 'IF'".to_string())?;
        self.consume(TokenType::LeftParen, "Expected '(' after 'IF'".to_string())?;
        let condition = self.parse_expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after condition".to_string())?;
        self.consume(TokenType::Keyword(Keyword::Then), "Expected 'THEN' after condition".to_string())?;

        let mut stmts = Vec::new();

        while !self.check_keyword(Keyword::Else) &&
            !self.check_keyword(Keyword::ElseIf) &&
            !(self.check_keyword(Keyword::End) && self.peek_next_is_keyword(Keyword::If))
        {
            let stmt = self.parse_statement()?;
            stmts.push(*stmt);
        }

        let init_if = If::new(*condition, stmts);
        let mut else_ifs = Vec::new();
        let mut else_last = None;

        while self.match_keyword(Keyword::ElseIf)
        {
            self.consume(TokenType::LeftParen, "Expected '(' after 'ELSE IF'".to_string())?;
            let elif_condition = self.parse_expression()?;
            self.consume(TokenType::RightParen, "Expected ')' after condition".to_string())?;
            self.consume(TokenType::Keyword(Keyword::Then), "Expected 'THEN' after condition".to_string())?;

            let mut elif_stmts = Vec::new();

            while !self.check_keyword(Keyword::Else) &&
                !(self.check_keyword(Keyword::End) && self.peek_next_is_keyword(Keyword::If))
            {
                let stmt = self.parse_statement()?;
                elif_stmts.push(*stmt);
            }

            else_ifs.push(If::new(*elif_condition, elif_stmts));
        }

        if self.check_keyword(Keyword::Else)
        {
            else_last = Some(self.parse_else()?);
        }
        self.consume(TokenType::Keyword(Keyword::End), "Expected 'END'".to_string())?;
        self.consume(TokenType::Keyword(Keyword::If), "Expected 'IF'".to_string())?;
        let if_stmt = Stmt::If { init_if, else_ifs, else_last };
        Ok(Box::new(if_stmt))
    }

    fn parse_else(&mut self) -> Result<If, ParsingError>
    {
        self.consume(TokenType::Keyword(Keyword::Else), "Expected 'ELSE'".to_string())?;

        let mut stmts = Vec::new();
        while !self.check_keyword(Keyword::End)
        {
            let stmt = self.parse_statement()?;
            stmts.push(*stmt);
        }

        let dummy_condition = Expr::Literal { value: Literal::Logical(true) };
        Ok(If::new(dummy_condition, stmts))
    }
    fn parse_declaration(&mut self)->Result<Vec<Declaration>,ParsingError>
    {
        if self.is_declaration_keyword()
        {
            let var_type = self.get_var_type()?;
            if self.match_tokens(&[TokenType::Comma])
            {
                if self.match_keyword(Keyword::Parameter)
                {
                    return Ok(vec![self.parse_parameter_declaration(var_type)?]);
                }

            }
           return self.parse_variable_declaration(var_type);
        }

        Err(ParsingError::EndOfInput)
    }
    fn parse_variable_declaration(&mut self,var_type:VarType)->Result<Vec<Declaration>,ParsingError>
    {

        let has_double_colon = self.match_tokens(&[TokenType::ColonColon]);

        let mut var_names = Vec::new();
        loop
        {
            if !self.match_tokens(&[TokenType::Identifier(String::from(""))])
            {
                return Err(ParsingError::UnexpectedToken(self.current_token().unwrap().clone()));
            }
            let var_name = self.previous().clone();
            let initial_value = if self.match_tokens(&[TokenType::Assign])
            {
                Some(*self.parse_expression()?)
            }
            else
            {
                None
            };


            var_names.push((var_name.lexeme, initial_value.clone()));
            if !self.match_tokens(&[TokenType::Comma])
            {
                break;
            }
        }

        let variables = var_names.iter().map(|tup| Declaration::Variable
        {
            name: tup.0.clone(),
            var_type: var_type.clone(),
            initial_value: tup.1.clone(),
        }).collect();
        Ok(variables)
    }
    fn consume_keyword(&mut self,attribute:Keyword)
    {
        if self.check_keyword(attribute)
        {
            self.advance();
        }

    }
    fn parse_parameter_declaration(&mut self,var_type: VarType)->Result<Declaration,ParsingError>
    {

        let has_double_colon = self.match_tokens(&[TokenType::ColonColon]);
        if !self.match_tokens(&[TokenType::Identifier(String::from(""))])
        {
            return Err(ParsingError::UnexpectedToken(self.current_token().unwrap().clone()));
        }
        let var_name =self.previous().lexeme.clone();

        self.consume(TokenType::Assign,"Expected '=' in parameter declaration".to_string())?;
        let value = self.parse_expression()?;
        let v = value.as_ref().clone();
        Ok(Declaration::Parameter {name:var_name,var_type,value: v })
    }
    pub(crate) fn parse_expression(&mut self) -> Result<Box<Expr>, ParsingError> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<Box<Expr>, ParsingError>
    {
        let mut node = self.parse_logical_and()?;

        while self.match_tokens(&[TokenType::Or])
        {
            let operator = self.previous().clone();
            let right = self.parse_logical_and()?;
            let left = node;
            let expr = Expr::BinaryOp { left, right, op: operator.token_type };
            node = Box::new(expr);
        }

        Ok(node)
    }

    fn parse_logical_and(&mut self) -> Result<Box<Expr>, ParsingError>
    {
        let mut node = self.parse_comparison()?;

        while self.match_tokens(&[TokenType::And])
        {
            let operator = self.previous().clone();
            let right = self.parse_comparison()?;
            let left = node;
            let expr = Expr::BinaryOp { left, right, op: operator.token_type };
            node = Box::new(expr);
        }

        Ok(node)
    }


    fn parse_comparison(&mut self) -> Result<Box<Expr>, ParsingError>
    {
        let mut node = self.parse_addition()?;
        while self.match_tokens(&[TokenType::Eq, TokenType::Ne, TokenType::Lt, TokenType::Le, TokenType::Gt, TokenType::Ge])
        {
            let operator = self.previous().clone();
            let right = self.parse_addition()?;
            let left = node;
            let expr = Expr::BinaryOp { left, right, op: operator.token_type };
            node = Box::new(expr);
        }
        Ok(node)
    }

    fn parse_addition(&mut self) -> Result<Box<Expr>, ParsingError>
    {
        let mut node = self.term()?;

        while self.match_tokens(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            let left = node;
            let expr = Expr::BinaryOp { left, right, op: operator.token_type };
            node = Box::new(expr);
        }

        Ok(node)
    }
    fn term(&mut self)->Result<Box<Expr>,ParsingError>
    {
        let mut node = self.factor()?;
        while self.match_tokens(&[TokenType::Star, TokenType::Slash])
        {
            let operator = self.previous().clone();
            let right = self.factor()?;
            let left = node;
            let expr = Expr::BinaryOp{left,right,op:operator.token_type};
            node = Box::new(expr);
        }
        Ok(node)
    }

    fn factor(&mut self)->Result<Box<Expr>,ParsingError>
    {
        if self.match_tokens(&[TokenType::Minus, TokenType::Plus,TokenType::Not])
        {
            let operator = self.previous().clone();
            let right = self.factor()?;
            let expr = Expr::UnaryOp{op:operator,expr:right};
            return Ok(Box::new(expr));
        }
        self.primary()
    }

    fn primary(&mut self)->Result<Box<Expr>,ParsingError>
    {
        let mut token =  self.current_token().ok_or(ParsingError::EndOfInput)?.clone();
        match token.token_type
        {
            TokenType::Int(value) =>
                {

                    self.advance();
                    let literal = Expr::Literal { value: Literal::Int(value) };
                     Ok(Box::new(literal))
                }
            TokenType::Real(value) =>
                {
                    self.advance();
                    let literal = Expr::Literal { value: Literal::Real(value)};
                     Ok(Box::new(literal))
                }
            TokenType::Double(value) =>
                {
                    self.advance();
                    let literal = Expr::Literal { value: Literal::Double(value) };
                     Ok(Box::new(literal))
                }
            TokenType::Keyword(Keyword::True) =>
                {
                    self.advance();
                    let literal = Expr::Literal { value: Literal::Logical(true) };
                    Ok(Box::new(literal))
                }
            TokenType::Keyword(Keyword::False) =>
                {
                    self.advance();
                    let literal = Expr::Literal { value: Literal::Logical(false) };
                     Ok(Box::new(literal))
                }
            TokenType::Character(value) =>
                {
                    self.advance();
                    let literal = Expr::Literal { value: Literal::Character(value) };
                     Ok(Box::new(literal))
                }
            TokenType::LeftParen =>
                {
                    self.advance();
                    let expr = self.parse_expression()?;
                    if !self.match_tokens(&[TokenType::RightParen])
                    {
                        return Err(ParsingError::UnexpectedToken(self.current_token().unwrap().clone()));
                    }
                    let grouping = Expr::Grouping { expr };
                    Ok(Box::new(grouping))
                }
            TokenType::Identifier(name) =>
                {
                    self.advance();
                    let variable = Expr::Variable { name: name.clone() };
                    Ok(Box::new(variable))
                }
            _ =>
                {

                    Err(ParsingError::UnexpectedToken(token.clone()))
                }
        }
    }
}






