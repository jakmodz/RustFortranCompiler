mod lexer;
mod parser;
mod compiler;
mod Common;
mod tests;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::{Parser};
use std::env;
use std::fs;
use crate::compiler::compiler::{Compiler,generate_object_file};
use crate::parser::ast::{Expr,Literal};

#[cfg(target_os = "linux")]
const PATH: &str = "warkusz.o";
#[cfg(target_os = "windows")]
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
    match c.compile(vec![pr])
    {
        Ok(_) => println!("Compilation successful"),
        Err(e) =>
            {
                eprintln!("{}", e);
                return;
            }
    }

    generate_object_file(c,PATH).unwrap()

}
fn main()
{
    let args: Vec<String> = env::args().collect();
    // if args.len() != 2
    // {
    //     eprintln!("Usage: {} <file_path>", args[0]);
    //     std::process::exit(64);
    // }
    run_file("dd");

}

