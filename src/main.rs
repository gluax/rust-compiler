#[macro_use]
extern crate lazy_static;

mod token;
pub use self::token::{Token, TokenType};

mod lexer;
pub use self::lexer::Lexer;

mod parser;
pub use self::parser::Parser;

use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let lex = &mut Lexer::new(&line.unwrap());
        let mut tok = lex.next_token();
        let mut parser = Parser::new(lex);
        while tok.t_type != TokenType::EOF {
            println!("{:?}", tok);
            tok = lex.next_token();
        }
    }
}
