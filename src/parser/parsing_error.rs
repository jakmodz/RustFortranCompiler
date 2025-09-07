use std::fmt;
use crate::lexer::token::Token;

#[derive(Debug)]
pub enum ParsingError
{
    UnexpectedToken(Token),
    SyntaxError(Token,String),
    StatementAfterDeclaration(Token),
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
            ParsingError::StatementAfterDeclaration(token) => write!(f, "Statement found after declaration at line {}", token.line),
        }
    }
}
