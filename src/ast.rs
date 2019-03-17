use std::fmt;

mod token;
pub use self::token::{Token, TokenType};

trait Node: Display {
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
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }

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

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.val {
            Some(val) => write!(f, "{} {} = {};", self.token_literal(), self.name, val),
            None => write!(f, "{} {} = ;", self.token_literal(), self.name),
        }
    }
}

struct ReturnStatement {
    tok: Token,
    return_val: Option<Expression>,
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.return_val {
            Some(val) => write!(f, "{} {};", self.token_literal(), val),
            None => write!(f, "{} ;", self.token_literal()),
        }
    }
}

struct ExpressionStatement {
    tok: Token,
    expression: Expression,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

struct BlockStatement {
    tok: Token,
    statements: Vec<Statement>,
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for stmt in self.statements {
            out.push(stmt.to_string());
        }
        write!(f, "{}", out)
    }
}

struct Identifer {
    tok: Token,
    val: String,
}

impl Expression for Identifer {
    fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

struct Boolean {
    tok: Token,
    val: Bool,
}

impl Expression for Boolean {
    fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal)
    }
}

struct Integer {
    tok: Token,
    val: i64,
}

impl Expression for Integer {
    fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

struct PrefixExpression {
    tok: Token,
    op: String,
    right: Expression,
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.op, self.right)
    }
}

struct InfixExpression {
    tok: Token,
    op: String,
    left: Expression,
    right: Expression,
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

struct IfExpression {
    tok: Token,
    cond: Expression,
    conseq: BlockStatement,
    alt: Option<BlockStatement>,
}

impl Expression for IfExpression {
    fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} {})", self.cond, self.coneq)
    }
}

struct Function {
    tok: Token,
    params: Vec<Identifer>,
    body: BlockStatement,
}

impl Expression for Function {
    fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for param in self.params {
            out.push(param.to_string());
        }

        write!(f, "{} ({}) {}", self.token_literal, out.join(, ), self.body)
    }
}

struct CallExpression {
    tok: Token,
    func: Expression,
    args: Vec<Expression>,
}

impl Expression for CallExpression {
    fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.tok.literal
    }

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = Vec::new();
        for arg in self.args {
            out.push(arg.to_string());
        }
        write!(f, "{} ({})", self.func, out.join(", "))
    }
}
