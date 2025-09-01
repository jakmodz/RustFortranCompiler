use crate::lexer::token::{Token,TokenType,Keyword};
use std::collections::HashMap;
use std::f32::consts::E;

pub enum LexerError
{
    UnexpectedCharacter(char, usize),
    UnterminatedString(usize),
    InvalidNumber(String, usize),
}

impl std::fmt::Display for LexerError
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        match self
        {
            LexerError::UnexpectedCharacter(c, line) =>
                write!(f, "Unexpected character '{}' at line {}", c, line),
            LexerError::UnterminatedString(line) =>
                write!(f, "Unterminated string at line {}", line),
            LexerError::InvalidNumber(num_str, line) =>
                write!(f, "Invalid number '{}' at line {}", num_str, line),
        }
    }
}


pub struct Lexer
{
    pub source: Vec<char>,
    keywords: HashMap<String, Keyword>,
    start:usize,
    current:usize,
    line:usize,
}

impl Lexer
{
    pub fn new(source: String) -> Self
    {

        let mut keywords = HashMap::new();
        keywords.insert("program".to_string(), Keyword::Program);
        keywords.insert("end".to_string(), Keyword::End);
        keywords.insert("module".to_string(), Keyword::Module);
        keywords.insert("use".to_string(), Keyword::Use);
        keywords.insert("contains".to_string(), Keyword::Contains);
        keywords.insert("subroutine".to_string(), Keyword::Subroutine);
        keywords.insert("function".to_string(), Keyword::Function);
        keywords.insert("return".to_string(), Keyword::Return);
        keywords.insert("integer".to_string(), Keyword::Integer);
        keywords.insert("real".to_string(), Keyword::Real);
        keywords.insert("doubleprecision".to_string(), Keyword::DoublePrecision);
        keywords.insert("complex".to_string(), Keyword::Complex);
        keywords.insert("logical".to_string(), Keyword::Logical);
        keywords.insert("character".to_string(), Keyword::Character);
        keywords.insert("parameter".to_string(), Keyword::Parameter);
        keywords.insert("dimension".to_string(), Keyword::Dimension);
        keywords.insert("allocatable".to_string(), Keyword::Allocatable);
        keywords.insert("pointer".to_string(), Keyword::Pointer);
        keywords.insert("target".to_string(), Keyword::Target);
        keywords.insert("intent".to_string(), Keyword::Intent);
        keywords.insert("in".to_string(), Keyword::In);
        keywords.insert("out".to_string(), Keyword::Out);
        keywords.insert("inout".to_string(), Keyword::Inout);
        keywords.insert("optional".to_string(), Keyword::Optional);
        keywords.insert("if".to_string(), Keyword::If);
        keywords.insert("then".to_string(), Keyword::Then);
        keywords.insert("else".to_string(), Keyword::Else);
        keywords.insert("elseif".to_string(), Keyword::ElseIf);
        Self {
            source: source.chars().collect(),
            keywords,
            start:0,
            current:0,
            line:1
        }
    }

    fn advance(&mut self) -> char
    {
        self.current += 1;
        self.source[self.current - 1]
    }
    fn current_char(&self) ->char
    {
        self.source.get(self.current).copied().unwrap_or('\0')
    }
    fn is_at_end(&self) -> bool
    {
        self.current >= self.source.len()
    }

    pub fn tokenize(&mut self) ->Result<Vec<Token>, LexerError>
    {
        let mut  tokens = Vec::new();

        while !self.is_at_end()
        {
            let token = self.next_token()?;
            tokens.push(token);

        }

        tokens.push(Token::new(TokenType::Eof, "".to_string(), self.line));
        Ok(tokens)
    }
    fn string(&mut self) -> Result<Token,LexerError>
    {
        //skip opening quote
        let opening_c = self.advance();

        let mut  str =String::new();

        while self.current_char() != opening_c  && !self.is_at_end()
        {
            let c = self.advance();

            if c == '"' || c == '\''
            {
                self.advance();
                str.push(self.advance());
            }
            else
            {
                str.push(c);
            }
        }

        if self.is_at_end()
        {
            return Err(LexerError::UnterminatedString(self.line));
        }
        //skip closing quote
        self.advance();


        Ok(Token::new(TokenType::Character(str.clone()),str, self.line))
    }
    fn number(&mut self) -> Result<Token,LexerError>
    {
        let mut num_str = String::new();
        while self.current_char().is_ascii_digit()
        {
            num_str.push(self.advance());
        }

        if self.current_char() == '.'
        {
            num_str.push(self.advance());
            while self.current_char().is_ascii_digit()
            {
                num_str.push(self.advance());
            }
            if let Some('e') | Some('E') = self.peek()
            {
                num_str.push(self.advance());
                if let Some('+') | Some('-') = self.peek()
                {
                    num_str.push(self.advance());
                }
                while self.current_char().is_ascii_digit()
                {
                    num_str.push(self.advance());
                }
            }
            match num_str.parse::<f64>()
            {
                Ok(n) => Ok(Token::new(TokenType::Double(n), num_str, self.line)),
                Err(_) => Err(LexerError::InvalidNumber(num_str, self.line)),
            }
        }
        else if let Some('e') | Some('E') = self.peek()
        {
            num_str.push(self.advance());
            if let Some('+') | Some('-') = self.peek()
            {
                num_str.push(self.advance());
            }
            while self.current_char().is_ascii_digit()
            {
                num_str.push(self.advance());
            }
            match num_str.parse::<f64>()
            {
                Ok(n) => Ok(Token::new(TokenType::Double(n), num_str, self.line)),
                Err(_) => Err(LexerError::InvalidNumber(num_str, self.line)),
            }
        }
        else
        {
            match num_str.parse::<i64>()
            {
                Ok(n) => Ok(Token::new(TokenType::Int(n), num_str, self.line)),
                Err(_) => Err(LexerError::InvalidNumber(num_str, self.line)),
            }
        }
    }

    fn identifier(&mut self)->Result<Token,LexerError>
    {
        let mut str = String::new();
        while self.current_char().is_ascii_alphanumeric() || self.current_char() == '_'
        {
            str.push(self.advance());
        }
        if let Some(keyword) = self.keywords.get(&str.to_lowercase())
        {
            Ok(Token::new(TokenType::Keyword(keyword.clone()), str, self.line))
        }
        else
        {
            Ok(Token::new(TokenType::Identifier(str.clone()), str, self.line))
        }
    }

    fn peek(&self)-> Option<char>
    {
        self.source.get(self.current + 1).copied()
    }
    fn ignore_comment(&mut self)
    {
        while self.current_char() != '\n' && !self.is_at_end()
        {
            self.advance();
        }
    }
    fn next_token(&mut self)-> Result<Token, LexerError>
    {
        self.start = self.current;
        let c = self.current_char();
        match c
        {

            '"' | '\''=>
            {
                self.string()
            }
            ' ' | '\r' | '\t' =>
            {
                self.advance();
                self.next_token()
            },
            '\n' =>
            {
                self.line += 1;
                self.advance();
                self.next_token()
            },
            '!' =>
            {
                self.ignore_comment();
                self.next_token()
            }
            '['=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::LeftBracket, "[".to_string(), self.line))
                }
            ']'=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::RightBracket, "]".to_string(), self.line))
                }
            ';'=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::Semicolon, ";".to_string(), self.line))
                }
            ':'=>
                {

                    if let Some(':') = self.peek()
                    {
                        self.advance();
                        self.advance();
                        return Ok(Token::new(TokenType::ColonColon, "::".to_string(), self.line));
                    }
                    self.advance();
                    Ok(Token::new(TokenType::Colon, ":".to_string(), self.line))
                }
            ','=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::Comma, ",".to_string(), self.line))
                }
            '%'=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::Percent, "%".to_string(), self.line))
                }
            '('=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::LeftParen, "(".to_string(), self.line))
                }
            ')'=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::RightParen, ")".to_string(), self.line))
                }
            '+'=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::Plus, "+".to_string(), self.line))
                }
            '-'=>
                {
                    self.advance();
                    Ok(Token::new(TokenType::Minus, "-".to_string(), self.line))
                }
            '*'=>
                {
                    if let Some('*') = self.peek()
                    {
                        self.advance();
                        self.advance();
                        return Ok(Token::new(TokenType::StarStar, "**".to_string(), self.line));
                    }
                    self.advance();
                    Ok(Token::new(TokenType::Star, "*".to_string(), self.line))
                }
            '/'=>
                {
                    if let Some('=') = self.peek()
                    {
                        self.advance();
                        self.advance();
                        return Ok(Token::new(TokenType::Ne, "/=".to_string(), self.line));
                    }
                    self.advance();
                    Ok(Token::new(TokenType::Slash, "/".to_string(), self.line))
                },
            '='=>
                {
                    if let Some('=') = self.peek()
                    {
                        self.advance();
                        self.advance();
                        return Ok(Token::new(TokenType::Eq, "==".to_string(), self.line));
                    }
                    self.advance();
                    Ok(Token::new(TokenType::Assign, "=".to_string(), self.line))
                }
            '<'=>
                {
                    if let Some('=') = self.peek()
                    {
                        self.advance();
                        self.advance();
                        return Ok(Token::new(TokenType::Le, "<=".to_string(), self.line));
                    }
                    self.advance();
                    Ok(Token::new(TokenType::Lt, "<".to_string(), self.line))
                }
            '>'=>
                {
                    if let Some('=') = self.peek()
                    {
                        self.advance();
                        self.advance();
                        return Ok(Token::new(TokenType::Ge, ">=".to_string(), self.line));
                    }
                    self.advance();
                    Ok(Token::new(TokenType::Gt, ">".to_string(), self.line))
                }
            '.'=>
                {
                    let mut op = String::new();
                    op.push(self.advance());
                    while let Some(c) = self.peek()
                    {
                        if c.is_alphabetic()
                        {
                            op.push(self.advance());
                        } else if c == '.'
                        {
                            op.push(self.advance());
                            break;
                        } else {
                            break;
                        }
                    }
                    match op.to_lowercase().as_str()
                    {
                        ".and." => Ok(Token::new(TokenType::And, op, self.line)),
                        ".or." => Ok(Token::new(TokenType::Or, op, self.line)),
                        ".not." => Ok(Token::new(TokenType::Not, op, self.line)),
                        ".eqv." => Ok(Token::new(TokenType::Eqv, op, self.line)),
                        ".neqv." => Ok(Token::new(TokenType::Neqv, op, self.line)),
                        _ => Err(LexerError::UnexpectedCharacter(c, self.line)),
                    }
                }
            _ =>
                {
                    if c.is_ascii_digit()
                    {
                        return self.number();
                    }
                    if c.is_ascii_alphabetic() || c == '_'
                    {
                        return self.identifier();
                    }
                    Err(LexerError::UnexpectedCharacter(c, self.line))
                }
        }
    }
}