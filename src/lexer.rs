mod token;
pub use self::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    pos: usize,
    read_pos: usize,
    ch: u8,
}

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn new_token(token_type: TokenType, ch: u8) -> Token {
    Token {
        t_type: token_type,
        literal: (ch as char).to_string(),
    }
}

#[allow(dead_code)]
impl Lexer {
    fn peek_char(&mut self) -> char {
        if self.read_pos >= self.input.len() {
            0 as char
        } else {
            let chars: Vec<char> = self.input.chars().collect();
            *chars.get(self.read_pos).unwrap_or(&' ')
        }
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = 0;
        } else {
            let chars: Vec<char> = self.input.chars().collect();
            self.ch = *chars.get(self.read_pos).unwrap_or(&' ') as u8;
        }
        self.pos = self.read_pos;
        self.read_pos = self.read_pos + 1;
    }

    fn skip_whitespace(&mut self) {
        let mut ch = self.ch as char;
        while ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
            self.read_char();
            ch = self.ch as char;
        }
    }

    pub fn new(input: &str) -> Self {
        let mut lex = Lexer {
            input: input.to_string(),
            pos: 0,
            read_pos: 0,
            ch: 0,
        };
        lex.read_char();
        lex
    }

    fn read_ident(&mut self) -> String {
        let pos = self.pos;
        while is_letter(self.ch as char) {
            self.read_char();
        }
        self.input.chars().skip(pos).take(self.pos - pos).collect()
    }

    fn read_num(&mut self) -> String {
        let pos = self.pos;
        while is_letter(self.ch as char) {
            self.read_char();
        }
        self.input.chars().skip(pos).take(self.pos - pos).collect()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch as char {
            '=' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = ch.to_string() + &self.ch.to_string();
                    Token {
                        t_type: TokenType::EQ,
                        literal: literal,
                    }
                } else {
                    new_token(TokenType::ASSIGN, self.ch)
                }
            }
            '+' => new_token(TokenType::PLUS, self.ch),
            '-' => new_token(TokenType::PLUS, self.ch),
            '!' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = ch.to_string() + &self.ch.to_string();
                    Token {
                        t_type: TokenType::NOTEQ,
                        literal: literal,
                    }
                } else {
                    new_token(TokenType::BANG, self.ch)
                }
            }
            '*' => new_token(TokenType::ASTERISK, self.ch),
            '/' => new_token(TokenType::SLASH, self.ch),
            '<' => new_token(TokenType::LT, self.ch),
            '>' => new_token(TokenType::GT, self.ch),
            ';' => new_token(TokenType::SEMICOLON, self.ch),
            ',' => new_token(TokenType::COMMA, self.ch),
            '{' => new_token(TokenType::LBRACE, self.ch),
            '}' => new_token(TokenType::RBRACE, self.ch),
            '(' => new_token(TokenType::LPAREN, self.ch),
            ')' => new_token(TokenType::RPAREN, self.ch),
            '\0' => new_token(TokenType::EOF, 0),
            _ => {
                if is_letter(self.ch as char) {
                    let lit = self.read_ident();
                    Token {
                        t_type: token::lookup_ident(&lit),
                        literal: lit,
                    }
                } else if (self.ch as char).is_digit(10) {
                    Token {
                        t_type: TokenType::INT,
                        literal: self.read_num(),
                    }
                } else {
                    new_token(TokenType::ILLEGAL, self.ch)
                }
            }
        };

        self.read_char();
        tok
    }
}
