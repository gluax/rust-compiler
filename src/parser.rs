use std::collections::HashMap;

mod token;
pub use self::token::{Token, TokenType};

mod lexer;
pub use self::lexer::Lexer;

mod ast;
pub use self::ast::Expression;

const INT: u8 = iota!();
const LOWEST: u8 = INT + 1;
const EQUALS: u8 = LOWEST + 1;
const LESSGREATER: u8 = EQUALS + 1;
const SUM: u8 = LESSGREATER + 1;
const PRODUCT: u8 = SUM + 1;
const PREFIX: u8 = PRODUCT + 1;
const CALL: u8 = PREFIX + 1;

lazy_static! {
    static ref PRECEDENCE: HashMap<TokenType, u8> = {
        let mut m = HashMap::new();
        m.insert(TokenType::EQ, EQUALS);
        m.insert(TokenType::NOTEQ, EQUALS);
        m.insert(TokenType::LT, LESSGREATER);
        m.insert(TokenType::GT, LESSGREATER);
        m.insert(TokenType::PLUS, SUM);
        m.insert(TokenType::MINUS, SUM);
        m.insert(TokenType::SLASH, PRODUCT);
        m.insert(TokenType::ASTERISK, PRODUCT);
        m.insert(TokenType::LPAREN, CALL);
        m
    };
}

type prefixParserFn = fn() -> Expression;
type infixParserFn = fn(Expression) -> Expression;

struct Parser {
    lex: Lexer,
    cur_tok: Token,
    peek_tok: Token,
    prefixParseFns: HashMap<TokenType, String>,
    infixParseFns: HashMap<TokenType, String>,
}
