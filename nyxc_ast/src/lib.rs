use std::ops::Deref;

use keywords::{check_keyword, Kw};

pub mod ast;
pub mod diagnostics;
pub mod keywords;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    Parenthesis,
    Brace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpToken {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Expression operators
    Eq,                  // =
    Lt,                  // <
    Le,                  // <=
    Gt,                  // >
    Ge,                  // >=
    EqEq,                // ==
    Ne,                  // !=
    And,                 // &
    Or,                  // |
    Not,                 // !
    BinOp(BinOpToken),   // x + y
    BinOpEq(BinOpToken), // Something like x += y

    // Structural Symbols
    Dot,                   // .
    RArrow,                // ->
    SemiColon,             // ;
    Colon,                 // :
    Comma,                 // ,
    Pound,                 // #
    OpenDelim(Delimiter),  // { or (
    CloseDelim(Delimiter), // } or )

    // Literals
    Literal(Lit),

    // Identifiers
    Ident(Symbol),

    None,

    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
}

impl Default for Token {
    fn default() -> Self {
        Self::dummy()
    }
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Self { kind }
    }

    pub fn glue(&self, joint: &Token) -> Option<Token> {
        let kind = match self.kind {
            TokenKind::Eq => match joint.kind {
                TokenKind::Eq => TokenKind::EqEq, // ==
                _ => return None,
            },
            TokenKind::Lt => match joint.kind {
                TokenKind::Eq => TokenKind::Le, // <=
                _ => return None,
            },
            TokenKind::Gt => match joint.kind {
                TokenKind::Eq => TokenKind::Ge, // >=
                _ => return None,
            },
            TokenKind::Not => match joint.kind {
                TokenKind::Eq => TokenKind::Ne, // !=
                _ => return None,
            },
            TokenKind::BinOp(op) => {
                match joint.kind {
                    TokenKind::Gt if op == BinOpToken::Minus => TokenKind::RArrow, // ->
                    _ => return None,
                }
            }
            _ => return None,
        };
        Some(Token::new(kind))
    }

    pub fn dummy() -> Self {
        Self {
            kind: TokenKind::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Symbol,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LitKind {
    Bool,
    Int,
    Float,
    Str { terminated: bool },
}

impl LitKind {
    pub fn default_symbol(&self) -> Symbol {
        match self {
            LitKind::Bool => Symbol::new("true"),
            LitKind::Int => Symbol::new("42"),
            LitKind::Float => Symbol::new("42.0"),
            LitKind::Str { .. } => Symbol::new("string literal expected"),
        }
    }
}

impl Lit {
    pub fn new(kind: LitKind, symbol: Symbol) -> Self {
        Self { kind, symbol }
    }
}

pub type TokenTree = Vec<Token>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InnerSymbol {
    inner: String,
}

impl InnerSymbol {
    pub fn underscore() -> Self {
        Self {
            inner: "_".to_owned(),
        }
    }
}

impl InnerSymbol {
    pub fn new(s: &str) -> Self {
        Self {
            inner: s.to_owned(),
        }
    }
}

impl Deref for InnerSymbol {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    Keyword(Kw),
    Symbol(InnerSymbol),
}

impl Symbol {
    pub fn new(str: &str) -> Symbol {
        match check_keyword(str) {
            Some(kw) => Symbol::Keyword(kw),
            None => Symbol::Symbol(InnerSymbol {
                inner: str.to_owned(),
            }),
        }
    }

    pub fn get_keyword(&self) -> Option<&Kw> {
        match self {
            Symbol::Keyword(kw) => Some(kw),
            Symbol::Symbol(_) => None,
        }
    }

    pub fn get_identifier(&self) -> Option<&str> {
        match self {
            Symbol::Keyword(_) => None,
            Symbol::Symbol(i) => Some(i.deref()),
        }
    }
}
