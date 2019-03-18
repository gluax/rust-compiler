use std::collections::HashMap;

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[allow(dead_code)]
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

type PrefixParseFn = fn(&mut Parser) -> Box<ast::Expression>;
type InfixParseFn = fn(Box<ast::Expression>) -> Box<ast::Expression>;

pub struct Parser {
    pub lex: Lexer,
    cur_tok: Token,
    peek_tok: Token,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

fn parse_ident(par: &mut Parser) -> Box<ast::Expression> {
    Box::new(ast::Identifer {
        tok: par.cur_tok.clone(),
        val: par.cur_tok.clone().literal,
    })
}

impl Parser {
    fn next_token(&mut self) {
        self.cur_tok = self.peek_tok.clone();
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
        let tok = self.cur_tok.clone();
        let mut stmts: Vec<Box<ast::Statement>> = Vec::new();
        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            stmts.push(stmt.unwrap());
            // match stmt {
            //     Some(val) => stmts.push(val
            //     None => {}
            // }
        }

        ast::BlockStatement {
            tok: tok,
            statements: stmts,
        }
    }

    fn peek_precedence(&mut self) -> u8 {
        let precedence = PRECEDENCE.get(&self.peek_tok.t_type);

        match precedence {
            Some(val) => *val,
            None => LOWEST,
        }
    }

    fn cur_precedence(&mut self) -> u8 {
        let precedence = PRECEDENCE.get(&self.cur_tok.t_type);

        match precedence {
            Some(val) => *val,
            None => LOWEST,
        }
    }

    fn parse_bool(&mut self) -> Box<ast::Expression> {
        Box::new(ast::Boolean {
            tok: self.cur_tok.clone(),
            val: self.cur_token_is(TokenType::TRUE),
        })
    }

    fn parse_if_expression(&mut self) -> Option<Box<ast::Expression>> {
        let tok = self.cur_tok.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        self.next_token();
        let cond = self.parse_expression(LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        let conseq = self.parse_block_statement();
        let mut alt = None;
        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();

            if self.expect_peek(TokenType::LBRACE) {
                return None;
            }
            alt = Some(self.parse_block_statement());
        }

        Some(Box::new(ast::IfExpression {
            tok: tok,
            cond: cond.unwrap(),
            conseq: conseq,
            alt: alt,
        }))
    }

    fn parse_function_params(&mut self) -> Option<Vec<ast::Identifer>> {
        let mut idents: Vec<ast::Identifer> = Vec::new();

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(idents);
        }

        self.next_token();
        idents.push(ast::Identifer {
            tok: self.cur_tok.clone(),
            val: self.cur_tok.clone().literal,
        });

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            idents.push(ast::Identifer {
                tok: self.cur_tok.clone(),
                val: self.cur_tok.clone().literal,
            });
        }

        if self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        Some(idents)
    }

    fn parse_function_literal(&mut self) -> Option<Box<ast::Expression>> {
        let tok = self.cur_tok.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        let params_opt = self.parse_function_params();
        let params = if params_opt.is_some() {
            params_opt.unwrap()
        } else {
            return None;
        };

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        Some(Box::new(ast::Function {
            tok: tok,
            params: params,
            body: self.parse_block_statement(),
        }))
    }

    fn parse_call_args(&mut self) -> Option<Vec<Box<ast::Expression>>> {
        let mut args: Vec<Box<ast::Expression>> = Vec::new();
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        let mut expression_opt = self.parse_expression(LOWEST);
        let mut arg;
        if expression_opt.is_some() {
            arg = expression_opt.unwrap()
        } else {
            return None;
        };
        args.push(arg);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            expression_opt = self.parse_expression(LOWEST);
            if expression_opt.is_some() {
                arg = expression_opt.unwrap()
            } else {
                return None;
            };
            args.push(arg);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        Some(args)
    }

    fn parse_call_expression(&mut self, func: Box<ast::Expression>) -> Box<ast::Expression> {
        Box::new(ast::CallExpression {
            tok: self.cur_tok.clone(),
            func: func,
            args: self.parse_call_args(),
        })
    }

    fn register_prefix(&mut self, t_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(t_type, func);
    }

    fn register_infix(&mut self, t_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(t_type, func);
    }

    fn parse_int_literal(&mut self) -> Option<Box<ast::Expression>> {
        let int_res = self.cur_tok.literal.parse::<i64>();
        let int;
        if int_res.is_ok() {
            int = int_res.unwrap();
        } else {
            return None;
        };

        Some(Box::new(ast::Integer {
            tok: self.cur_tok.clone(),
            val: int,
        }))
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<Box<ast::Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_tok.t_type);

        let mut left_exp;
        if prefix.is_some() {
            left_exp = prefix.unwrap()(self);
        } else {
            return None;
        }

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix = self.infix_parse_fns.get(&self.peek_tok.t_type);
            let ifunc;
            if infix.is_some() {
                ifunc = infix.unwrap();
            } else {
                return Some(left_exp);
            }

            // self.next_token();
            left_exp = ifunc(left_exp);
        }

        Some(left_exp)
    }

    fn parse_let_statement(&mut self) -> Option<Box<ast::Statement>> {
        let tok = self.cur_tok.clone();
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        let name = ast::Identifer {
            tok: tok.clone(),
            val: tok.clone().literal,
        };
        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        self.next_token();

        let val = self.parse_expression(LOWEST).unwrap();
        if !self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(ast::LetStatement {
            tok: tok,
            name: name,
            val: Some(val),
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<ast::Statement>> {
        let tok = self.cur_tok.clone();
        self.next_token();
        let exp = self.parse_expression(LOWEST).unwrap();

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(ast::ReturnStatement {
            tok: tok,
            return_val: Some(exp),
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Box<ast::Statement>> {
        // let prefix = self.prefixParsefns.get(self.cur_tok.t_type);
        let stmt = ast::ExpressionStatement {
            tok: self.cur_tok.clone(),
            expression: self.parse_expression(LOWEST).unwrap(),
        };

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    fn parse_statement(&mut self) -> Option<Box<ast::Statement>> {
        match self.cur_tok.t_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_prefix_expression(&mut self) -> Box<ast::Expression> {
        let tok = self.cur_tok.clone();
        self.next_token();

        Box::new(ast::PrefixExpression {
            tok: tok.clone(),
            op: tok.clone().literal,
            right: self.parse_expression(PREFIX).unwrap(),
        })
    }

    fn parse_infix_expression(&mut self, left: Box<ast::Expression>) -> Box<ast::Expression> {
        let tok = self.cur_tok.clone();
        let precedence = self.cur_precedence();
        self.next_token();

        Box::new(ast::InfixExpression {
            tok: tok.clone(),
            op: tok.clone().literal,
            left: left,
            right: self.parse_expression(precedence).unwrap(),
        })
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut stmts: Vec<Box<ast::Statement>> = Vec::new();
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
        let pfs: HashMap<TokenType, PrefixParseFn> = HashMap::new();
        let ifs: HashMap<TokenType, InfixParseFn> = HashMap::new();

        let mut par = Parser {
            lex: lex,
            prefix_parse_fns: pfs,
            infix_parse_fns: ifs,
            cur_tok: Token {
                t_type: TokenType::DUMMY,
                literal: "".to_string(),
            },
            peek_tok: Token {
                t_type: TokenType::DUMMY,
                literal: "".to_string(),
            },
        };

        par.register_prefix(TokenType::IDENT, parse_ident);
        // par.register_prefix(TokenType::INT, par.parse_int_literal);
        // par.register_prefix(TokenType::BANG, par.parse_prefix_expression);
        // par.register_prefix(TokenType::MINUS, par.parse_prefix_expression);
        // par.register_prefix(TokenType::TRUE, par.parse_bool);
        // par.register_prefix(TokenType::FALSE, par.parse_bool);
        // // par.register_prefix(TokenType::LPAREN, par.);
        // par.register_prefix(TokenType::IF, par.parse_if_expression);
        // par.register_prefix(TokenType::FUNCTION, par.parse_function_literal);

        // par.register_infix(TokenType::PLUS, par.parse_infix_expression);
        // par.register_infix(TokenType::MINUS, par.parse_infix_expression);
        // par.register_infix(TokenType::SLASH, par.parse_infix_expression);
        // par.register_infix(TokenType::ASTERISK, par.parse_infix_expression);
        // par.register_infix(TokenType::EQ, par.parse_infix_expression);
        // par.register_infix(TokenType::NOTEQ, par.parse_infix_expression);
        // par.register_infix(TokenType::LT, par.parse_infix_expression);
        // par.register_infix(TokenType::GT, par.parse_infix_expression);

        par.next_token();
        par.next_token();

        par
    }
}
