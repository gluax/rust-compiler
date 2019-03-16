#[macro_use]
extern crate lazy_static;

mod lexer;
pub use self::lexer::{Lexer, TokenType};

use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let lex = &mut Lexer::new(&line.unwrap());
        let mut tok = lex.next_token();
        while tok.t_type != TokenType::EOF {
            println!("{:?}", tok);
            tok = lex.next_token();
        }
    }
}
