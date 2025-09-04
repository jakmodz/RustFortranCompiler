mod lexer;
mod parser;
mod compiler;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::{Parser};
use std::env;
use std::fs;
use crate::compiler::compiler::Compiler;
use crate::parser::ast::{Expr,Literal};

fn run_file(_file_path: &str)
{
    let file = fs::read_to_string("test.f90").expect("Unable to read file");
    let mut lexer =  Lexer::new(file);
    let tokens =match   lexer.tokenize()
    {
        Ok(t) => t,
        Err(e) =>
        {
            eprintln!("Error during tokenization: {}", e);
            return;
        }
    };

    let mut parser = Parser::new(tokens);

    match parser.parse()
    {
        Ok(expr) => println!("Ast: {:#?}", expr),
        Err(e) => eprintln!("Error during parsing: {}", e),
    }
    let c = Compiler::new("warkusz").unwrap();
    let expr = Expr::Literal {
        value: Literal::Int(42)
    };

}


fn main()
{
    todo!("parsing main program");
    let args: Vec<String> = env::args().collect();
    // if args.len() != 2
    // {
    //     eprintln!("Usage: {} <file_path>", args[0]);
    //     std::process::exit(64);
    // }
    run_file("dd");

}
