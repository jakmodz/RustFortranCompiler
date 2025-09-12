use crate::lexer::token::{Token,TokenType,Keyword};
use std::collections::HashMap;

#[derive(Debug)]
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
        keywords.insert("PROGRAM".to_string(), Keyword::Program);
        keywords.insert("PRINT".to_string(), Keyword::Print);
        keywords.insert("END".to_string(), Keyword::End);
        keywords.insert("EXIT".to_string(), Keyword::Exit);
        keywords.insert("MODULE".to_string(), Keyword::Module);
        keywords.insert("USE".to_string(), Keyword::Use);
        keywords.insert("CONTAINS".to_string(), Keyword::Contains);
        keywords.insert("SUBROUTINE".to_string(), Keyword::Subroutine);
        keywords.insert("FUNCTION".to_string(), Keyword::Function);
        keywords.insert("RETURN".to_string(), Keyword::Return);
        keywords.insert("INTEGER".to_string(), Keyword::Integer);
        keywords.insert("REAL".to_string(), Keyword::Real);
        keywords.insert("DOUBLEPRECISION".to_string(), Keyword::DoublePrecision);
        keywords.insert("COMPLEX".to_string(), Keyword::Complex);
        keywords.insert("LOGICAL".to_string(), Keyword::Logical);
        keywords.insert("CHARACTER".to_string(), Keyword::Character);
        keywords.insert("PARAMETER".to_string(), Keyword::Parameter);
        keywords.insert("DIMENSION".to_string(), Keyword::Dimension);
        keywords.insert("ALLOCATABLE".to_string(), Keyword::Allocatable);
        keywords.insert("POINTER".to_string(), Keyword::Pointer);
        keywords.insert("TARGET".to_string(), Keyword::Target);
        keywords.insert("INTENT".to_string(), Keyword::Intent);
        keywords.insert("IN".to_string(), Keyword::In);
        keywords.insert("OUT".to_string(), Keyword::Out);
        keywords.insert("INOUT".to_string(), Keyword::Inout);
        keywords.insert("OPTIONAL".to_string(), Keyword::Optional);
        keywords.insert("IF".to_string(), Keyword::If);
        keywords.insert("THEN".to_string(), Keyword::Then);
        keywords.insert("ELSE".to_string(), Keyword::Else);
        keywords.insert("ELSEIF".to_string(), Keyword::ElseIf);
        keywords.insert("DO".to_string(), Keyword::Do);
        keywords.insert("WHILE".to_string(), Keyword::While);
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
        while self.current_char().is_ascii_digit() {
            num_str.push(self.advance());
        }

        if self.current_char() == '.' {
            num_str.push(self.advance());
            while self.current_char().is_ascii_digit() {
                num_str.push(self.advance());
            }
        }

        if self.current_char() == 'e' || self.current_char() == 'E' {
            num_str.push(self.advance());
            if self.current_char() == '+' || self.current_char() == '-' {
                num_str.push(self.advance());
            }
            let mut has_exp_digits = false;
            while self.current_char().is_ascii_digit() {
                has_exp_digits = true;
                num_str.push(self.advance());
            }
            if !has_exp_digits {
                return Err(LexerError::InvalidNumber(num_str, self.line));
            }
        }

        if num_str.contains('.') || num_str.contains('e') || num_str.contains('E') {
            match num_str.parse::<f64>() {
                Ok(n) => Ok(Token::new(TokenType::Double(n), num_str, self.line)),
                Err(_) => Err(LexerError::InvalidNumber(num_str, self.line)),
            }
        } else {
            match num_str.parse::<i64>() {
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
        let upper_str = str.to_ascii_uppercase();
        if let Some(keyword) = self.keywords.get(&upper_str)
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
            ' ' | '\r' | '\t'  =>
            {
                self.advance();
                self.next_token()
            },
            '\0' =>
            {
                Ok(Token::new(TokenType::Eof, "".to_string(), self.line))
            }
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
                        }
                        else if c == '.'
                        {
                            op.push(self.advance());
                            op.push(self.advance());
                            break;
                        }
                        else
                        {
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
                        ".true." =>Ok(Token::new(TokenType::Keyword(Keyword::True),op,self.line)),
                        ".false." =>Ok(Token::new(TokenType::Keyword(Keyword::False),op,self.line)),
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