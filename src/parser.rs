use std::collections::HashMap;

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

mod ast;

const INT: u8 = 0;
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

type PrefixParseFn = fn() -> ast::Expression;
type InfixParseFn = fn(ast::Expression) -> ast::Expression;

pub struct Parser {
    lex: Lexer,
    cur_tok: Token,
    peek_tok: Token,
    prefix_parse_fns: HashMap<TokenType, String>,
    infix_parse_fns: HashMap<TokenType, String>,
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
    fn parse_block_statement(&mut self) -> ast::BlockStatement {
        let tok = self.cur_tok;
        let mut stmts: Vec<ast::Statement> = Vec::new();
        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            match stmt {
                Some(val) => stmts.push(stmt),
                None => {}
            }
        }

        ast::BlockStatement {
            tok: tok,
            statements: stmts,
        }
    }

    fn peek_precedence(&mut self) -> u8 {
        let precedence = PRECEDENCE.get(self.peek_tok.t_type);

        match precedence {
            Some(val) => val,
            None => LOWEST,
        }
    }

    fn cur_precedence(&mut self) -> u8 {
        let precedence = PRECEDENCE.get(self.cur_tok.t_type);

        match precedence {
            Some(val) => val,
            None => LOWEST,
        }
    }

    fn parse_ident(&mut self) -> ast::Expression {
        ast::Identifer {
            tok: self.cur_tok,
            val: self.cur_tok.literal,
        }
    }

    fn parse_bool(&mut self) -> ast::Expression {
        ast::Boolean {
            tok: self.cur_tok,
            val: self.cur_token_is(TokenType::True),
        }
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        let tok = self.cur_tok;
        if !self.expect_peek(TokenType::LPAREN) {
            None
        }

        self.next_token();
        let cond = self.parse_expression(LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            None
        }

        if !self.expect_peek(TokenType::LPAREN) {
            None
        }

        let conseq = self.parse_block_statement();
        let mut alt;
        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();

            if self.expect_peek(TokenType::LBRACE) {
                None
            }
            alt = self.parse_block_statement();
        }

        Some(ast::IfExpression {
            tok: tok,
            cond: cond,
            conseq: conseq,
            alt: alt,
        })
    }

    fn parse_function_params(&mut self) -> Option<Vec<ast::Identifer>> {
        let idents: Vec<ast::Identifer> = Vec::new();

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            Some(idents)
        }

        self.next_token();
        idents.push(ast::Identifer {
            tok: self.cur_tok,
            val: self.cur_tok.literal,
        });

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            idents.push(ast::Identifer {
                tok: self.cur_tok,
                val: self.cur_tok.literal,
            });
        }

        if self.expect_peek(TokenType::RPAREN) {
            None
        }

        Some(idents)
    }

    fn parse_function_literal(&mut self) -> Option<ast::Expression> {
        let tok = self.cur_tok();
        if !self.expect_peek(TokenType::LPAREN) {
            None
        }

        let params = self.parse_function_params();
        match params {
            Some(val) => {}
            None => None,
        }

        if !self.expect_peek(TokenType::LBRACE) {
            None
        }

        Some(ast::Function {
            tok: tok,
            params: params.unwrap(),
            body: self.parse_block_statement(),
        })
    }

    fn parse_call_args(&mut self) -> Option<Vec<ast::Expression>> {
        let args: Vec<ast::Expression> = Vec::new();
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            Some(args)
        }

        self.next_token();
        args.push(self.parse_expression(LOWEST));

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(LOWEST));
        }

        if !self.expect_peek(TokenType::RPAREN) {
            None
        }

        Some(args)
    }

    fn parse_call_expression(&mut self, func: ast::Expression) -> ast::Expression {
        ast::CallExpression {
            tok: self.cur_tok,
            func: func,
            args: self.parse_call_args(),
        }
    }

    fn register_prefix(&mut self, t_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(t_type, func);
    }

    fn register_infix(&mut self, t_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(t_type, func);
    }

    fn parse_int_literal(&mut self) -> Option<ast::Expression> {
        let int = self.cur_tok.literal.parse::<i64>();
        match int {
            Some(val) => Some(ast::Integer {
                tok: self.cur_tok,
                val: val,
            }),
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<ast::Expression> {
        let prefix = self.prefixParsefns.get(self.cur_tok.t_type);

        match prefix {
            Some(val) => {}
            None => {
                //self.no_prefix_parse_fn_error(self.cur_tok.t_type);
                None
            }
        }

        let left_exp = prefix.unwrap()();

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix = self.infixParseFns.get(self.peek_token.t_type);
            match infix {
                Some(val) => {}
                None => infix,
            }

            self.next_token();
        }

        left_exp
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let tok = self.cur_tok;
        if !self.expect_peek(TokenType::IDENT) {
            None
        }

        let name = ast::Identifer {
            tok: self.cur_tok,
            val: self.cur_tok.literal,
        };
        if !self.expect_peek(TokenType::ASSIGN) {
            None
        }

        self.next_token();

        let val = self.parse_expression(LOWEST).unwrap();
        if !self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(ast::LetStatement {
            tok: self.cur_tok,
            name: name,
            val: val,
        })
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let tok = self.cur_tok;
        self.next_token();
        let exp = self.parse_expression(LOWEST).unwrap();

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(ast::ReturnStatement {
            tok: tok,
            return_val: exp,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<ast::ExpressionStatement> {
        // let prefix = self.prefixParsefns.get(self.cur_tok.t_type);
        let stmt = ast::ExpressionStatement {
            tok: self.cur_tok,
            expression: self.parse_expression(LOWEST),
        };

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_tok.t_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_prefix_expression(&mut self) -> ast::Expression {
        let tok = self.cur_tok;
        self.next_token();

        ast::PrefixExpression {
            tok: tok,
            op: tok.literal,
            right: self.parse_expression(PREFIX).unwrap(),
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> ast::Expression {
        let tok = self.cur_tok;
        let precedence = self.cur_precedence();
        self.next_token();

        ast::InfixExpression {
            tok: tok,
            op: tok.literal,
            left: left,
            right: self.parse_expression(precedence).unwrap(),
        }
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut stmts: Vec<ast::Statement> = Vec::new();
        while !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            match stmt {
                Some(val) => stmts.push(val),
                None => {}
            }

            self.next_token();
        }

        ast::Program { statements: stmts }
    }

    pub fn new(lex: Lexer) -> Self {
        let mut pfs: HashMap<TokenType, PrefixParseFn> = HashMap::new();
        let mut ifs: HashMap<TokenType, InfixParseFn> = HashMap::new();

        Parser {
            lex: lex,
            prefix_parse_fns: pfs,
            infix_parse_fns: ifs,
        }
    }
}
