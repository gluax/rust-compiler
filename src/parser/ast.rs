use std::fmt;

use crate::token::Token;

trait Node: fmt::Display {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    statements: Vec<Statement>,
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
        for stmt in self.statements {
            out.push(stmt.to_string());
        }
        write!(f, "{}", out)
    }
}

pub struct LetStatement {
    tok: Token,
    name: Identifer,
    val: Option<Expression>,
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.val {
            Some(val) => write!(f, "{} {} = {};", self.token_literal(), self.name, val),
            None => write!(f, "{} {} = ;", self.token_literal(), self.name),
        }
    }
}

pub struct ReturnStatement {
    tok: Token,
    return_val: Option<Expression>,
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.return_val {
            Some(val) => write!(f, "{} {};", self.token_literal(), val),
            None => write!(f, "{} ;", self.token_literal()),
        }
    }
}

pub struct ExpressionStatement {
    tok: Token,
    expression: Expression,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

pub struct BlockStatement {
    tok: Token,
    statements: Vec<Statement>,
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for stmt in self.statements {
            out.push(stmt.to_string());
        }
        write!(f, "{}", out)
    }
}

pub struct Identifer {
    tok: Token,
    val: String,
}

impl Expression for Identifer {
    fn expression_node(&self) {}
}

impl Node for Identifer {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for Identifer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

pub struct Boolean {
    tok: Token,
    val: bool,
}

impl Expression for Boolean {
    fn expression_node(&self) {}
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal)
    }
}

pub struct Integer {
    tok: Token,
    val: i64,
}

impl Expression for Integer {
    fn expression_node(&self) {}
}

impl Node for Integer {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

pub struct PrefixExpression {
    tok: Token,
    op: String,
    right: Expression,
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.op, self.right)
    }
}

pub struct InfixExpression {
    tok: Token,
    op: String,
    left: Expression,
    right: Expression,
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

pub struct IfExpression {
    tok: Token,
    cond: Expression,
    conseq: BlockStatement,
    alt: BlockStatement,
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} {})", self.cond, self.coneq)
    }
}

pub struct Function {
    tok: Token,
    params: Vec<Identifer>,
    body: BlockStatement,
}

impl Expression for Function {
    fn expression_node(&self) {}
}

impl Node for Function {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for param in self.params {
            out.push(param.to_string());
        }

        write!(
            f,
            "{} ({}) {}",
            self.token_literal,
            out.join(", "),
            self.body
        )
    }
}

pub struct CallExpression {
    tok: Token,
    func: Expression,
    args: Option<Vec<Expression>>,
}

impl Expression for CallExpression {
    fn expression_node(&self) {}
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.tok.literal
    }
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for arg in self.args {
            out.push(arg.to_string());
        }
        write!(f, "{} ({})", self.func, out.join(", "))
    }
}
