use std::fmt::Formatter;
use crate::lexer::token::{Token, TokenType};
#[derive(Debug, Clone, PartialEq)]
pub enum FormatDescriptor 
{
    Integer { width: Option<u32>, minimum_digits: Option<u32> }, 
    FixedReal { width: u32, decimal_places: u32 },              
    ScientificReal { width: u32, decimal_places: u32 },         
    Character { width: Option<u32> },                           
    Skip { spaces: u32 },                                       
    Literal(String),                                            
    Repeat { count: u32, descriptors: Vec<FormatDescriptor> },  
}


#[derive(Debug, Clone, PartialEq)]
pub enum FormatSpec 
{
    ListDirected,                          // *
    Formatted(Vec<FormatDescriptor>),      // '(I5,F8.2)'
}
#[derive(Debug,Clone)]
pub enum Literal
{
    Int(i64),
    Real(f32),
    Double(f64),
    Character(String),
    Logical(bool)
}
#[derive(Debug,Clone)]
pub enum VarType
{
    Integer,
    Real,
    Logical,
    Complex,
    Character{len:usize}

}
#[derive(Debug, Clone)]
pub enum Declaration
{
    Variable 
    {
        name: String,
        var_type: VarType,
        initial_value: Option<Expr>,
    },
    Parameter
    {
        name: String,
        var_type: VarType,
        value: Expr,
    }
}
#[derive(Debug,Clone)]
pub enum Expr
{
    Literal{value:Literal},
    BinaryOp{left: Box<Expr>, op: TokenType, right: Box<Expr>},
    UnaryOp{op:Token,expr:Box<Expr>},
    Grouping{expr:Box<Expr>},

    Variable{name:String},
}
#[derive(Debug,Clone)]
pub enum Stmt
{
    Assignment{var_name:String,expr:Box<Expr>},

    Print{format_descriptor: FormatSpec,exprs:Vec<Box<Expr>>},
    Read{format_descriptor: FormatSpec,var_name:String},
    
    If{init_if:If,else_ifs: Vec<If>, else_last: Option<If>},
    DoWhile{cond:Expr,statements: Vec<Stmt>},
    DoFor{var_name:String,start:Expr,end:Expr,step:Option<Expr>,statements: Vec<Stmt>},
    DoInfinite{statements: Vec<Stmt>},
    Exit,

}
#[derive(Debug,Clone)]
pub struct If
{
    pub cond:Expr,
    pub statements: Vec<Stmt>,
}


impl If
{
    pub fn new(cond:Expr,statements:Vec<Stmt>)->If
    {
        Self
        {
            cond,
            statements
        }
    }
}
