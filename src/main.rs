mod lexer;
mod parser;
mod compiler;
mod Common;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::{Parser};
use std::env;
use std::fs;
use crate::compiler::compiler::{Compiler,generate_object_file};
use crate::parser::ast::{Expr,Literal};
const PATH: &str = "warkusz.obj";
fn run_file(_file_path: &str)
{
    let file = fs::read_to_string("test.f90").expect("Unable to read file");
    let mut lexer = Lexer::new(file);
    let tokens = match lexer.tokenize()
    {
        Ok(t) => t,
        Err(e) =>
            {
                eprintln!("Error during tokenization: {}", e);
                return;
            }
    };

    let mut parser = Parser::new(tokens);

    let pr = parser.parse_all().unwrap();
    println!("{:#?}", pr);
    let mut c = Compiler::new("warkusz").unwrap();
    c.compile(vec![pr]);
    generate_object_file(c,PATH).unwrap()

}
fn main()
{
    //todo!(check if it compiles to binary and run it);
    let args: Vec<String> = env::args().collect();
    // if args.len() != 2
    // {
    //     eprintln!("Usage: {} <file_path>", args[0]);
    //     std::process::exit(64);
    // }
    run_file("dd");

}

