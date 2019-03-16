use std::collections::HashMap;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub enum TokenType {
    // Misc
    ILLEGAL,
    EOF,

    // Idents + literals
    IDENT,
    INT,

    // OPs
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOTEQ,

    // Delims
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub t_type: TokenType,
    pub literal: String,
}

// macro_rules! hashmap {
//     ($( $key: expr => $val: expr ),*) => {{
//          let mut map = ::std::collections::HashMap::new();
//          $( map.insert($key, $val); )*
//          map
//     }}
// }

lazy_static! {
    static ref HASHMAP: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenType::FUNCTION);
        m.insert("let", TokenType::LET);
        m.insert("", TokenType::TRUE);
        m
    };
}

// static mut Keywords: HashMap<&str, TokenType> = hashmap![
//     "fn" => TokenType::FUNCTION,
//     "let" => TokenType::LET,
//     "true" => TokenType::TRUE,
//     "false" => TokenType::FALSE,
//     "if" => TokenType::IF,
//     "else" => TokenType::ELSE,
//     "return" => TokenType::RETURN
// ];
