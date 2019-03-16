#[macro_use]
extern crate lazy_static;

mod lexer;
pub use self::lexer::Lexer;

fn main() {
    let lex = &mut Lexer::new("=");
    println!("{:?}", lex.next_token());
}
