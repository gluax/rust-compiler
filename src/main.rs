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
    // let stdin = io::stdin();
    // for line in stdin.lock().lines() {
    //     let lex = &mut Lexer::new(&line.unwrap());
    //     let mut tok = lex.next_token();
    //     while tok.t_type != TokenType::EOF {
    //         println!("{:?}", tok);
    //         tok = lex.next_token();
    //     }
    //     println!("{:?}", tok);
    // }

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let lex = Lexer::new(&line.unwrap());
        let mut parser = Parser::new(lex);
        println!("parser {:?}", parser.lex);
        println!("{}", parser.parse_program());
    }
}
