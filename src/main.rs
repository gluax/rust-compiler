#[macro_use]
extern crate lazy_static;

mod token;
pub use self::token::{Token, TokenType};

mod lexer;
pub use self::lexer::Lexer;

#[allow(dead_code)]
mod parser;
pub use self::parser::Parser;

use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let lex = Lexer::new(&line.unwrap());
        let parser = Parser::new(lex);
        println!("parser {:?}", parser.lex);
    }
}
