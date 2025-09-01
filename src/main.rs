mod lexer;
mod parser;
mod Compiler;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use std::env;
use std::fs;
fn run_file(_file_path: &str)
{
    let file = fs::read_to_string(_file_path).expect("Unable to read file");
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
}


fn main()
{
    let args: Vec<String> = env::args().collect();
    if args.len() != 2
    {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(64);
    }
    run_file(&args[1]);

}
