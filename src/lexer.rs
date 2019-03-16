mod token;
pub use self::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    pos: usize,
    read_pos: usize,
    ch: char,
}

#[allow(dead_code)]
fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}
#[allow(dead_code)]
fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn new_token(token_type: TokenType, ch: char) -> Token {
    Token {
        t_type: token_type,
        literal: ch.to_string(),
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
            self.ch = 0 as char;
        } else {
            let chars: Vec<char> = self.input.chars().collect();
            self.ch = *chars.get(self.read_pos).unwrap_or(&' ');
        }
        self.pos = self.read_pos;
        self.read_pos = self.read_pos + 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    pub fn new(input: &str) -> Self {
        let mut lex = Lexer {
            input: input.to_string(),
            pos: 0,
            read_pos: 0,
            ch: 0 as char,
        };
        lex.read_char();
        lex
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.ch {
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
            _ => new_token(TokenType::ILLEGAL, self.ch),
        }
    }
}
