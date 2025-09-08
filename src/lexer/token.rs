use std::fmt;

#[derive(Debug, Clone)]
pub enum TokenType
{
    //literal
    Character(String),
    Real(f32),
    Double(f64),
    Int(i64),

    Identifier(String),
    Keyword(Keyword),

    // Operators
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    StarStar,       // **

    // Comparison
    Eq,             // ==
    Ne,             // /=
    Lt,             // <
    Le,             // <=
    Gt,             // >
    Ge,             // >=

    // Logical operators
    And,            // .AND.
    Or,             // .OR.
    Not,            // .NOT.
    Eqv,            // .EQV.
    Neqv,           // .NEQV.

    // Delimiters
    LeftParen,      // (
    RightParen,     // )
    LeftBracket,    // [
    RightBracket,   // ]
    Comma,          // ,
    Colon,          // :
    ColonColon,     // ::
    Semicolon,      // ;
    Percent,        // %


    // Assignment
    Assign,         // =

    // Special
    Eof,
}
impl TokenType
{
    /// Returns the lexeme for operator and delimiter types, or a descriptive string for others.
    pub fn lexeme(&self) -> &'static str
    {
        match self
        {
            TokenType::Character(_) => "Character",
            TokenType::Real(_) => "Real",
            TokenType::Double(_) => "Double",
            TokenType::Int(_) => "Int",
            TokenType::Identifier(_) => "Identifier",
            TokenType::Keyword(_) => "Keyword",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Star => "*",
            TokenType::Slash => "/",
            TokenType::StarStar => "**",
            TokenType::Eq => "==",
            TokenType::Ne => "/=",
            TokenType::Lt => "<",
            TokenType::Le => "<=",
            TokenType::Gt => ">",
            TokenType::Ge => ">=",
            TokenType::And => ".AND.",
            TokenType::Or => ".OR.",
            TokenType::Not => ".NOT.",
            TokenType::Eqv => ".EQV.",
            TokenType::Neqv => ".NEQV.",
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBracket => "[",
            TokenType::RightBracket => "]",
            TokenType::Comma => ",",
            TokenType::Colon => ":",
            TokenType::ColonColon => "::",
            TokenType::Semicolon => ";",
            TokenType::Percent => "%",
            TokenType::Assign => "=",
            TokenType::Eof => "EOF",
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Keyword
{
    Program, End, Module, Use, Contains,
    Subroutine, Function, Return,
    Integer, Real, DoublePrecision, Complex, Logical, Character,
    Parameter, Dimension, Allocatable, Pointer, Target,
    Intent, In, Out, Inout, Optional,
    If, Then, Else, ElseIf, EndIf,
    Do, EndDo, While, Exit, Cycle,
    Select, Case, Default, EndSelect,
    Where, Elsewhere, EndWhere,
    Type, EndType, Class,
    Interface, EndInterface,
    Abstract, Procedure,
    Public, Private, Protected,
    Save, External, Intrinsic,
    Common, Block, Data,
    Goto, Continue, Stop, Pause,
    Read, Write, Print, Open, Close, Rewind, Backspace, Endfile,
    Format, Namelist,
    Implicit, None,
    True, False,
}
#[derive(Debug, Clone)]
pub struct Token
{
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}


impl Token
{
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self
    {
        Self { token_type, lexeme, line }
    }
}
impl fmt::Display for Token
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "{:?} {} at line {}", self.token_type, self.lexeme, self.line)
    }
}
