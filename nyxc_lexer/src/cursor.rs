use std::str::Chars;

#[derive(Debug)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    len_remaining: usize,
    prev: char,
}

const EOF: char = '\0';

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            len_remaining: input.len(),
            prev: EOF,
        }
    }

    pub fn prev(&self) -> char {
        self.prev
    }

    pub fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF)
    }

    pub fn second(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap_or(EOF)
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn pos_within_token(&self) -> usize {
        self.len_remaining - self.chars.as_str().len()
    }

    pub fn reset_pos_within_token(&mut self) {
        self.len_remaining = self.chars.as_str().len();
    }

    pub fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.prev = c;
        Some(c)
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }
}
