pub mod cursor;
pub mod tokens;

use cursor::Cursor;

use tokens::{LiteralKind, Token, TokenKind};

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

impl Cursor<'_> {
    pub fn advance_token(&mut self) -> Token {
        let first_char = match self.bump() {
            Some(c) => c,
            None => return Token::new(TokenKind::Eof, 0),
        };

        let token_kind = match first_char {
            // Slash, or comment
            '/' => match self.first() {
                '/' => self.comment(),
                _ => TokenKind::Slash,
            },

            // Whitespaces
            c if c.is_whitespace() => self.whitespace(),

            c if is_id_start(c) => self.ident(),

            _c @ '0'..='9' => {
                let literal_kind = self.number();
                TokenKind::Literal { kind: literal_kind }
            }

            // One symbol tokens
            ';' => TokenKind::SemiColon,
            ':' => TokenKind::Colon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '*' => TokenKind::Star,
            '#' => TokenKind::Pound,
            '=' => TokenKind::Eq,
            '>' => TokenKind::Gt,
            '<' => TokenKind::Lt,
            '-' => TokenKind::Minus,
            '!' => TokenKind::Bang,
            '+' => TokenKind::Plus,
            '%' => TokenKind::Percent,
            '&' => TokenKind::And,
            '|' => TokenKind::Or,
            '^' => TokenKind::Caret,

            // String Literal
            '"' => {
                let terminated = self.double_quoted_string();
                let kind = LiteralKind::Str { terminated };
                TokenKind::Literal { kind }
            }

            _ => TokenKind::Unknown,
        };

        let res = Token::new(token_kind, self.pos_within_token());
        self.reset_pos_within_token();
        res
    }

    fn comment(&mut self) -> TokenKind {
        self.bump();
        self.eat_while(|c| c != '\n');

        TokenKind::Comment
    }

    fn whitespace(&mut self) -> TokenKind {
        self.eat_while(|c| c.is_whitespace());
        TokenKind::Whitespace
    }

    fn number(&mut self) -> LiteralKind {
        self.eat_decimal_digits();

        match self.first() {
            '.' => {
                self.bump();
                if self.first().is_ascii_digit() {
                    self.eat_decimal_digits();
                }
                LiteralKind::Float
            }
            _ => LiteralKind::Int,
        }
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_identifier(&mut self) {
        if !is_id_start(self.first()) {
            return;
        }

        self.bump();

        self.eat_while(is_id_continue);
    }

    fn double_quoted_string(&mut self) -> bool {
        while let Some(c) = self.bump() {
            match c {
                '"' => {
                    return true;
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    self.bump();
                }
                _ => {}
            }
        }
        false
    }

    fn ident(&mut self) -> TokenKind {
        self.eat_while(is_id_continue);
        TokenKind::Ident
    }
}

pub fn is_id_start(c: char) -> bool {
    c == '_' || unicode_ident::is_xid_start(c)
}

pub fn is_id_continue(c: char) -> bool {
    unicode_ident::is_xid_continue(c)
}

pub fn is_ident(string: &str) -> bool {
    let mut chars = string.chars();
    if let Some(start) = chars.next() {
        is_id_start(start) && chars.all(is_id_continue)
    } else {
        false
    }
}

#[cfg(test)]
mod test {

    use crate::{tokenize, tokens::Token};

    #[test]
    fn identifier() {
        let input = "let";
        let expected = Some(Token::new(crate::tokens::TokenKind::Ident, 3));
        let got = tokenize(input).next();
        assert_eq!(got, expected);
    }

    #[test]
    fn str_literal_terminated() {
        let input = "\"input\"";
        let expected = Some(Token::new(
            crate::tokens::TokenKind::Literal {
                kind: crate::tokens::LiteralKind::Str { terminated: true },
            },
            7,
        ));
        let got = tokenize(input).next();
        assert_eq!(got, expected);
    }

    #[test]
    fn str_literal_not_terminated() {
        let input = "\"input";
        let expected = Some(Token::new(
            crate::tokens::TokenKind::Literal {
                kind: crate::tokens::LiteralKind::Str { terminated: false },
            },
            6,
        ));
        let got = tokenize(input).next();
        assert_eq!(got, expected);
    }

    #[test]
    fn float_literal() {
        let input = "012312.30123124";
        let expected = Some(Token::new(
            crate::tokens::TokenKind::Literal {
                kind: crate::tokens::LiteralKind::Float,
            },
            15,
        ));
        let got = tokenize(input).next();
        assert_eq!(got, expected);
    }

    #[test]
    fn int_literal() {
        let input = "1231";
        let expected = Some(Token::new(
            crate::tokens::TokenKind::Literal {
                kind: crate::tokens::LiteralKind::Int,
            },
            4,
        ));
        let got = tokenize(input).next();
        assert_eq!(got, expected);
    }
}
