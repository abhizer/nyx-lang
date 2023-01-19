use std::{
    cmp::Ordering,
    fmt::Display,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub},
};

use crate::{BinOpToken, Delimiter, InnerSymbol, Symbol, Token, TokenKind};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PrefixOp {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InflixOp {
    Plus,
    Minus,
    EqEq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    Product,
    Divide,
    Modulo,
    Power,
    And,
    Or,
}

impl Display for InflixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            InflixOp::Plus => "+",
            InflixOp::Minus => "-",
            InflixOp::EqEq => "==",
            InflixOp::Ne => "!=",
            InflixOp::Le => "<=",
            InflixOp::Ge => ">=",
            InflixOp::Lt => "<",
            InflixOp::Gt => ">",
            InflixOp::Product => "*",
            InflixOp::Divide => "/",
            InflixOp::Modulo => "%",
            InflixOp::Power => "^",
            InflixOp::And => "&",
            InflixOp::Or => "|",
        };
        write!(f, "{str}")
    }
}

impl TryFrom<Token> for InflixOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::Lt => Ok(InflixOp::Lt),
            TokenKind::Le => Ok(InflixOp::Le),
            TokenKind::Gt => Ok(InflixOp::Gt),
            TokenKind::Ge => Ok(InflixOp::Ge),
            TokenKind::EqEq => Ok(InflixOp::EqEq),
            TokenKind::Ne => Ok(InflixOp::Ne),
            TokenKind::And => Ok(InflixOp::And),
            TokenKind::Or => Ok(InflixOp::Or),
            TokenKind::BinOp(tok) | TokenKind::BinOpEq(tok) => match tok {
                BinOpToken::Plus => Ok(InflixOp::Plus),
                BinOpToken::Minus => Ok(InflixOp::Minus),
                BinOpToken::Star => Ok(InflixOp::Product),
                BinOpToken::Slash => Ok(InflixOp::Divide),
                BinOpToken::Percent => Ok(InflixOp::Modulo),
                BinOpToken::Caret => Ok(InflixOp::Power),
                BinOpToken::And => Ok(InflixOp::And),
                BinOpToken::Or => Ok(InflixOp::Or),
            },
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(x) => write!(f, "{x}"),
            Literal::Float(x) => write!(f, "{x}"),
            Literal::Bool(x) => write!(f, "{x}"),
            Literal::Str(x) => {
                let x = x.replace('"', "");
                write!(f, "{x}")
            }
        }
    }
}

impl Add for Literal {
    type Output = Literal;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Literal::Int(x), Literal::Int(y)) => Literal::Int(x + y),
            (Literal::Float(x), Literal::Float(y)) => Literal::Float(x + y),
            (Literal::Str(x), Literal::Str(y)) => Literal::Str(format!("{}{}", x, y)),
            _ => unreachable!(),
        }
    }
}

impl Sub for Literal {
    type Output = Literal;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Literal::Int(x), Literal::Int(y)) => Literal::Int(x - y),
            (Literal::Float(x), Literal::Float(y)) => Literal::Float(x - y),
            _ => unreachable!(),
        }
    }
}

impl BitOr for Literal {
    type Output = Literal;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Literal::Bool(x), Literal::Bool(y)) => Literal::Bool(x | y),
            _ => unreachable!(),
        }
    }
}

impl BitAnd for Literal {
    type Output = Literal;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Literal::Bool(x), Literal::Bool(y)) => Literal::Bool(x & y),
            _ => unreachable!(),
        }
    }
}

impl PartialOrd for Literal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Literal::Int(x), Literal::Int(y)) => Some(x.cmp(y)),
            (Literal::Float(x), Literal::Float(y)) => x.partial_cmp(y),
            _ => unreachable!(),
        }
    }
}

impl BitXor for Literal {
    type Output = Literal;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Literal::Int(x), Literal::Int(y)) => Literal::Int(x.pow(y as u32)),
            (Literal::Float(x), Literal::Float(y)) => Literal::Float(x.powf(y)),
            (Literal::Bool(x), Literal::Bool(y)) => Literal::Bool(x ^ y),
            _ => unreachable!(),
        }
    }
}

impl Mul for Literal {
    type Output = Literal;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Literal::Int(x), Literal::Int(y)) => Literal::Int(x * y),
            (Literal::Float(x), Literal::Float(y)) => Literal::Float(x * y),
            _ => unreachable!(),
        }
    }
}

impl Div for Literal {
    type Output = Literal;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Literal::Int(x), Literal::Int(y)) => Literal::Int(x / y),
            (Literal::Float(x), Literal::Float(y)) => Literal::Float(x / y),
            _ => unreachable!(),
        }
    }
}

impl Rem for Literal {
    type Output = Literal;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Literal::Int(x), Literal::Int(y)) => Literal::Int(x % y),
            (Literal::Float(x), Literal::Float(y)) => Literal::Float(x % y),
            _ => unreachable!(),
        }
    }
}

impl Into<TypeInfo> for &Literal {
    fn into(self) -> TypeInfo {
        match self {
            Literal::Int(_) => TypeInfo {
                ty: InnerSymbol::new("i64"),
            },
            Literal::Float(_) => TypeInfo {
                ty: InnerSymbol::new("f64"),
            },
            Literal::Bool(_) => TypeInfo {
                ty: InnerSymbol::new("bool"),
            },
            Literal::Str(_) => TypeInfo {
                ty: InnerSymbol::new("string"),
            },
        }
    }
}

impl Into<TypeInfo> for Literal {
    fn into(self) -> TypeInfo {
        match self {
            Literal::Int(_) => TypeInfo {
                ty: InnerSymbol::new("i64"),
            },
            Literal::Float(_) => TypeInfo {
                ty: InnerSymbol::new("f64"),
            },
            Literal::Bool(_) => TypeInfo {
                ty: InnerSymbol::new("bool"),
            },
            Literal::Str(_) => TypeInfo {
                ty: InnerSymbol::new("string"),
            },
        }
    }
}

impl From<i64> for Literal {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<f64> for Literal {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Self::Str(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Symbol),
    Literal(Literal),
    Reassignment {
        identifier: InnerSymbol,
        expression: Box<Expression>,
    },
    Prefix {
        operator: PrefixOp,
        expr: Box<Expression>,
    },
    Inflix {
        operator: InflixOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Box<Expression>,
        alternate: Option<Box<Expression>>,
    },
    Call {
        identifier: Symbol,
        arguments: Vec<Expression>,
    },
    Continue,
    Break(Box<Expression>),
    Loop(Box<Expression>),
    Block(Program),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(Let),
    Return(Expression),
    Expr(Expression),
    Function {
        parameters: Vec<FunctionParameters>,
        name: InnerSymbol,
        ret_type: Option<TypeInfo>,
        body: Expression,
    },
}

#[derive(Debug, PartialEq)]
pub struct Reassignment {
    pub expression: Expression,
    pub identifier: InnerSymbol,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Let {
    pub mutable: bool,
    pub expression: Expression,
    pub identifier: InnerSymbol,
    pub ty: Option<TypeInfo>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameters {
    pub ident: InnerSymbol,
    pub ty: TypeInfo,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeInfo {
    pub ty: InnerSymbol,
}

impl Default for TypeInfo {
    fn default() -> Self {
        Self {
            ty: InnerSymbol::new("none"),
        }
    }
}

pub type Program = Vec<Statement>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Modulo,
    Power,
    Prefix,
    Call,
}

impl Token {
    pub fn get_precedence(&self) -> Precedence {
        match self.kind {
            TokenKind::EqEq => Precedence::Equals,
            TokenKind::Eq => Precedence::Equals,
            TokenKind::Lt => Precedence::LessGreater,
            TokenKind::Le => Precedence::LessGreater,
            TokenKind::Gt => Precedence::LessGreater,
            TokenKind::Ge => Precedence::LessGreater,
            TokenKind::BinOp(token) | TokenKind::BinOpEq(token) => match token {
                BinOpToken::Plus => Precedence::Sum,
                BinOpToken::Minus => Precedence::Sum,
                BinOpToken::Star => Precedence::Product,
                BinOpToken::Slash => Precedence::Product,
                BinOpToken::Percent => Precedence::Modulo,
                BinOpToken::Caret => Precedence::Power,
                BinOpToken::And => Precedence::Lowest,
                BinOpToken::Or => Precedence::Lowest,
            },
            TokenKind::OpenDelim(Delimiter::Parenthesis) => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}
