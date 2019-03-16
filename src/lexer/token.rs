use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq)]
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

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenType::FUNCTION);
        m.insert("let", TokenType::LET);
        m.insert("true", TokenType::TRUE);
        m.insert("false", TokenType::FALSE);
        m.insert("if", TokenType::IF);
        m.insert("else", TokenType::ELSE);
        m.insert("return", TokenType::RETURN);
        m
    };
}

pub fn lookup_ident(ident: &str) -> TokenType {
    println!("ident: {}", ident);
    match KEYWORDS.get(ident) {
        Some(t_type) => *t_type,
        None => TokenType::ILLEGAL,
    }
}
