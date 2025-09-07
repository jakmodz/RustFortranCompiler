use crate::parser::ast::{Declaration, Stmt};
use crate::parser::ast::*;
#[derive(Debug,Clone)]
pub struct Program
{
    pub name: String,
    pub declarations: Vec<Declaration>,
    pub stmts: Vec<Stmt>,
    
}
impl Program
{
    pub fn new(name:String,stmts:Vec<Stmt>,declarations: Vec<Declaration>) -> Program
    {
        Self
        {
            stmts,
            name,
            declarations
        }
    }
}

#[derive(Debug,Clone)]
pub enum ProgramUnit
{
     Program
     {
         program:Program
     },
}