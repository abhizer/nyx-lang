use std::fmt::Display;

use phf::phf_map;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kw {
    Fn,
    True,
    False,
    Let,
    If,
    Else,
    Return,
    For,
    Break,
    Continue,
    Loop,
    Underscore,
    Mut,
}

impl Display for Kw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            Kw::Fn => "fn",
            Kw::True => "true",
            Kw::False => "false",
            Kw::Let => "let",
            Kw::If => "if",
            Kw::Else => "else",
            Kw::Return => "return",
            Kw::For => "for",
            Kw::Break => "break",
            Kw::Continue => "continue",
            Kw::Loop => "loop",
            Kw::Underscore => "_",
            Kw::Mut => "mut",
        };
        write!(f, "{val}")
    }
}

pub const KEYWORD: phf::Map<&'static str, Kw> = phf_map! {
    "fn" => Kw::Fn,
    "true" => Kw::True,
    "false" => Kw::False,
    "let" => Kw::Let,
    "if" => Kw::If,
    "else" => Kw::Else,
    "return" => Kw::Return,
    "for" => Kw::For,
    "break" => Kw::Break,
    "continue" => Kw::Continue,
    "loop" => Kw::Loop,
    "_" => Kw::Underscore,
    "mut" => Kw::Mut,
};

pub fn check_keyword(arg: &str) -> Option<Kw> {
    KEYWORD.get(arg).cloned()
}
