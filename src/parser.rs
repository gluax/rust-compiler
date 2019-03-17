use std::collections::HashMap;

mod token;
pub use self::token::{Token, TokenType};

mod lexer;
pub use self::lexer::Lexer;

mod ast;
pub use self::ast::{Expression, Program, Statement};

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

pub struct Parser {
    lex: Lexer,
    cur_tok: Token,
    peek_tok: Token,
    prefixParseFns: HashMap<TokenType, String>,
    infixParseFns: HashMap<TokenType, String>,
}

impl Parser {
    fn next_token(&mut self) {
        self.cur_tok = self.peek_tok;
        self.peek_tok = self.lex.next_token();
    }

    fn cur_token_is(&mut self, t_type: TokenType) -> bool {
        self.cur_tok.t_type == t_type
    }

    fn peek_token_is(&mut self, t_type: TokenType) -> bool {
        self.peek_tok.t_type == t_type
    }

    fn expect_peek(&mut self, t_type: TokenType) -> bool {
        if self.peek_token_is(t_type) {
            self.next_token();
            true
        } else {
            // self.peek_error();
            false
        }
    }

    // pub fn errors(self) ->
    // peek_error(t_type: TokenType)
    // fn no_prefix_parse_fn_error(t_type: TokenType)

    fn parse_expression(&mut self, i32) -> Expression {
        let prefix = self.prefixParsefns.get(self.cur_tok.t_type);
    }
    
    fn parse_let_statement(&mut self) -> Option<Statement> {
        let tok = self.cur_tok;
        if !self.expect_peek(TokenType::IDENT) {
            None
        }

        let name = Identifier {
            tok: self.cur_tok,
            val: self.cur_tok.literal,
        };
        if !self.expect_peek(TokenType::ASSIGN) {
            None
        }

        self.next_token();

        let val = self.parse_expression(LOWEST);
        if !self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(LetStatement {
            tok: self.cur_tok,
            name: name,
            val: val,
        })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {}

    fn parse_expression_statement(&mut self) -> Option<Statement> {}

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_tok.t_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut stmts: Vec<Statement> = Vec::new();
        while !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            match stmt {
                Some(val) => stmts.push(val),
                None => {}
            }

            self.next_token();
        }

        Program { statements: stmts }
    }

    pub fn new(lex: Lexer) -> Self {
        let mut pfs: HashMap<TokenType, prefixParserFn> = HashMap::new();
        let mut ifs: HashMap<TokenType, infixParserFn> = HashMap::new();

        Parser {
            lex: lex,
            prefixParseFns: pfs,
            infixParseFns: ifs,
        }
    }
}
