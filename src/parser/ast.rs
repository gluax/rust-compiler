use std::fmt;

use crate::token::Token;

pub trait Node: fmt::Display {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for stmt in &self.statements {
            out.push(stmt.to_string());
        }
        write!(f, "{}", out.join(" "))
    }
}

pub struct LetStatement {
    pub tok: Token,
    pub name: Identifer,
    pub val: Option<Box<Expression>>,
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.val {
            Some(val) => write!(f, "{} {} = {};", self.token_literal(), self.name, val),
            None => write!(f, "{} {} = ;", self.token_literal(), self.name),
        }
    }
}

pub struct ReturnStatement {
    pub tok: Token,
    pub return_val: Option<Box<Expression>>,
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.return_val {
            Some(val) => write!(f, "{} {};", self.token_literal(), val),
            None => write!(f, "{} ;", self.token_literal()),
        }
    }
}

pub struct ExpressionStatement {
    pub tok: Token,
    pub expression: Box<Expression>,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

pub struct BlockStatement {
    pub tok: Token,
    pub statements: Vec<Box<Statement>>,
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for stmt in &self.statements {
            out.push(stmt.to_string());
        }
        write!(f, "{}", out.join(" "))
    }
}

pub struct Identifer {
    pub tok: Token,
    pub val: String,
}

impl Expression for Identifer {
    fn expression_node(&self) {}
}

impl Node for Identifer {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for Identifer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

pub struct Boolean {
    pub tok: Token,
    pub val: bool,
}

impl Expression for Boolean {
    fn expression_node(&self) {}
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

pub struct Integer {
    pub tok: Token,
    pub val: i64,
}

impl Expression for Integer {
    fn expression_node(&self) {}
}

impl Node for Integer {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

pub struct PrefixExpression {
    pub tok: Token,
    pub op: String,
    pub right: Box<Expression>,
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.op, self.right)
    }
}

pub struct InfixExpression {
    pub tok: Token,
    pub op: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

pub struct IfExpression {
    pub tok: Token,
    pub cond: Box<Expression>,
    pub conseq: BlockStatement,
    pub alt: Option<BlockStatement>,
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} {})", self.cond, self.conseq)
    }
}

pub struct Function {
    pub tok: Token,
    pub params: Vec<Identifer>,
    pub body: BlockStatement,
}

impl Expression for Function {
    fn expression_node(&self) {}
}

impl Node for Function {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for param in &self.params {
            out.push(param.to_string());
        }

        write!(
            f,
            "{} ({}) {}",
            self.token_literal(),
            out.join(", "),
            self.body
        )
    }
}

pub struct CallExpression {
    pub tok: Token,
    pub func: Box<Expression>,
    pub args: Option<Vec<Box<Expression>>>,
}

impl Expression for CallExpression {
    fn expression_node(&self) {}
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.tok.clone().literal
    }
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let out: Vec<String> = Vec::new();
        // for arg in self.args.unwrap().iter() {
        //     out.push(arg.to_string());
        // }
        write!(f, "{} ({})", self.func, out.join(", "))
    }
}
