use std::fmt::Formatter;
use crate::lexer::token::{Token, TokenType};

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

    Print{expr:Box<Expr>},
    If{cond:Box<Expr>,then:Box<Stmt>},

}

