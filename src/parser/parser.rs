use crate::lexer::token::{Keyword, Token, TokenType};
use std::fmt;
use crate::parser::ast::{Expr, Literal, Stmt, VarType};
use crate::parser::type_resolver::{resolve_simple_type,};

pub struct Parser
{
    tokens: Vec<Token>,
    current: usize,
}

pub enum ParsingError
{
    UnexpectedToken(Token),
    SyntaxError(Token,String),
    EndOfInput,
}

impl std::fmt::Display for ParsingError
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>)->fmt::Result
    {
        match self
        {
            ParsingError::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            ParsingError::SyntaxError(token,msg) => write!(f, "error at {} message:{} ",token.line ,msg),
            ParsingError::EndOfInput => write!(f, "Unexpected end of input"),
        }
    }
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
    fn parse_statement(&mut self) ->Result<Box<Stmt>,ParsingError>
    {

        if  self.match_tokens(&[TokenType::Keyword(Keyword::If)])
        {
            //todo!("if statement parsing not implemented yet");
        }
        else if self.match_tokens(&[TokenType::Keyword(Keyword::Print)])
        {
            self.parse_print()?;
        }

        self.parse_declaration()
    }

    fn parse_declaration(&mut self)->Result<Box<Stmt>,ParsingError>
    {
        self.parse_assigment().or_else(|_|self.parse_variable_declaration())
    }
    fn parse_variable_declaration(&mut self)->Result<Box<Stmt>,ParsingError>
    {

        let  var_type = self.current_token().ok_or(ParsingError::EndOfInput)?.clone();
        self.advance();
        let var_type = self.parse_type_with_parameters(&var_type)?;

         self.match_tokens(&[TokenType::ColonColon]);

        if !self.match_tokens(&[TokenType::Identifier(String::from(""))])
        {
            return Err(ParsingError::UnexpectedToken(self.current_token().unwrap().clone()));
        }
        let var_name = self.previous().clone();

        let declaration = Stmt::VarDeclare{ var_type: var_type, name: var_name.lexeme };
        Ok(Box::new(declaration))
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
            _ => Err(ParsingError::UnexpectedToken(token.clone()))
        }
    }
    fn parse_expression(&mut self)->Result<Box<Expr>,ParsingError>
    {
        let mut node = self.parse_comparison()?;
        while self.match_tokens(&[TokenType::Plus, TokenType::Minus])
        {
            let operator = self.previous().clone();
            let right = self.parse_comparison()?;
            let expr = Expr::BinaryOp{left:node,right,op:operator.token_type};
            node = Box::new(expr);

        }
        Ok(node)
    }
    fn parse_comparison(&mut self)->Result<Box<Expr>,ParsingError>
    {
        let mut node = self.term()?;
        while self.match_tokens(&[TokenType::Eq, TokenType::Ne,TokenType::Lt,TokenType::Le,TokenType::Gt,TokenType::Ge])
        {
            let operator = self.previous().clone();
            let right = self.term()?;
            let left = node;
            let expr = Expr::BinaryOp{left,right,op:operator.token_type};
            node = Box::new(expr);
        }
        Ok(node)
    }
    fn term(&mut self)->Result<Box<Expr>,ParsingError>
    {
        let mut node = self.factor()?;
        while self.match_tokens(&[TokenType::Star, TokenType::Slash,TokenType::Percent])
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
        if self.match_tokens(&[TokenType::Minus, TokenType::Plus])
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
                    let literal = Expr::Literal { value: Literal::Real(value) };
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
                    let literal = Expr::Literal { value: Literal::Int(1) };
                    Ok(Box::new(literal))
                }
            TokenType::Keyword(Keyword::False) =>
                {
                    self.advance();
                    let literal = Expr::Literal { value: Literal::Int(0) };
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






