#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Comment,
    Whitespace,
    Ident,
    Literal { kind: LiteralKind },
    SemiColon,
    Colon,
    Comma,
    Dot,
    Eq,
    Bang,
    Minus,
    Plus,
    Pound,
    Caret,
    And,
    Or,
    Star,
    Slash,
    Lt,
    Gt,
    Percent,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Unknown,
    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    Int,
    Float,
    Str { terminated: bool },
}
