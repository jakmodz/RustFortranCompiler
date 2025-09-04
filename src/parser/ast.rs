use std::fmt::Formatter;
use crate::lexer::token::{Token, TokenType};

#[derive(Debug,Clone)]
pub enum Literal
{
    Int(i64),
    Real(f32),
    Double(f64),
    Character(String),
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
    VarDeclare{ var_type:VarType,name:String},
    Assignment{var_name:String,expr:Box<Expr>},

    Print{expr:Box<Expr>},


}
pub struct Program
{
    pub stmts: Vec<Stmt>,
    pub name: String
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