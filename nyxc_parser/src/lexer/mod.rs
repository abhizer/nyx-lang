use nyxc_ast::{
    diagnostics::{DiagnosticLevel, Diagnostics},
    keywords::Kw,
    BinOpToken, Delimiter, Lit, LitKind, Symbol, Token, TokenKind, TokenTree,
};
use nyxc_lexer::cursor::Cursor;

#[derive(Debug)]
pub struct StringReader<'a> {
    src: &'a str,
    pos: usize,
    cursor: Cursor<'a>,
    open_braces: Vec<Delimiter>,
    diagnostics: Vec<Diagnostics>,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub string_reader: StringReader<'a>,
    /// Next token returned by string_reader but not yet handled by the Lexer
    pub token: Token,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let string_reader = StringReader::new(src, 0);
        let token = Token::dummy();
        Self {
            string_reader,
            token,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.token = self.string_reader.next_token().0;
        let mut buf = TokenTree::new();

        'outer: loop {
            match self.token.kind {
                TokenKind::Eof => {
                    buf.push(std::mem::take(&mut self.token));
                    break 'outer;
                }
                _ => {
                    let next_token = 'inner: loop {
                        let (next_token, preceded_by_whitespace) = self.string_reader.next_token();
                        if !preceded_by_whitespace {
                            if let Some(glued) = self.token.glue(&next_token) {
                                self.token = glued;
                            } else {
                                break 'inner (next_token);
                            }
                        } else {
                            break 'inner (next_token);
                        }
                    };
                    if let TokenKind::Ident(ref symbol) = self.token.kind {
                        if let Symbol::Keyword(kw) = symbol {
                            if [Kw::True, Kw::False].contains(kw) {
                                // change the token to a literal
                                self.token = Token::new(TokenKind::Literal(Lit::new(
                                    LitKind::Bool,
                                    symbol.to_owned(),
                                )));
                            }
                        }
                    }

                    buf.push(std::mem::replace(&mut self.token, next_token));
                }
            }
        }

        buf
    }

    pub fn diagnostics(&self) -> &[Diagnostics] {
        self.string_reader.diagnostics()
    }
}

impl<'a> StringReader<'a> {
    pub fn new(src: &'a str, pos: usize) -> Self {
        Self {
            src,
            pos,
            cursor: Cursor::new(src),
            open_braces: vec![],
            diagnostics: vec![],
        }
    }

    /// Returns a token and a bool representing weather it was preceded by a whitespace or not
    pub fn next_token(&mut self) -> (Token, bool) {
        let mut preceded_by_whitespace = false;

        loop {
            let token = self.cursor.advance_token();
            let start = self.pos;
            self.pos += token.len as usize;

            let kind = match token.kind {
                nyxc_lexer::tokens::TokenKind::Comment => continue,
                nyxc_lexer::tokens::TokenKind::Whitespace => {
                    preceded_by_whitespace = true;
                    continue;
                }
                nyxc_lexer::tokens::TokenKind::Ident => {
                    let ident = &self.src[start..self.pos];
                    let symbol = Symbol::new(ident);

                    TokenKind::Ident(symbol)
                }
                nyxc_lexer::tokens::TokenKind::Literal { kind } => TokenKind::Literal({
                    let kind = match kind {
                        nyxc_lexer::tokens::LiteralKind::Int => LitKind::Int,
                        nyxc_lexer::tokens::LiteralKind::Float => LitKind::Float,
                        nyxc_lexer::tokens::LiteralKind::Str { terminated } => {
                            LitKind::Str { terminated }
                        }
                    };
                    let ident = &self.src[start..self.pos];
                    let symbol = Symbol::new(ident);
                    Lit::new(kind, symbol)
                }),
                nyxc_lexer::tokens::TokenKind::SemiColon => TokenKind::SemiColon,
                nyxc_lexer::tokens::TokenKind::Colon => TokenKind::Colon,
                nyxc_lexer::tokens::TokenKind::Comma => TokenKind::Comma,
                nyxc_lexer::tokens::TokenKind::Dot => TokenKind::Dot,
                nyxc_lexer::tokens::TokenKind::Eq => TokenKind::Eq,
                nyxc_lexer::tokens::TokenKind::Bang => TokenKind::Not,
                nyxc_lexer::tokens::TokenKind::Minus => TokenKind::BinOp(BinOpToken::Minus),
                nyxc_lexer::tokens::TokenKind::Plus => TokenKind::BinOp(BinOpToken::Plus),
                nyxc_lexer::tokens::TokenKind::Pound => TokenKind::Pound,
                nyxc_lexer::tokens::TokenKind::Caret => TokenKind::BinOp(BinOpToken::Caret),
                nyxc_lexer::tokens::TokenKind::And => TokenKind::BinOp(BinOpToken::And),
                nyxc_lexer::tokens::TokenKind::Or => TokenKind::BinOp(BinOpToken::Or),
                nyxc_lexer::tokens::TokenKind::Star => TokenKind::BinOp(BinOpToken::Star),
                nyxc_lexer::tokens::TokenKind::Slash => TokenKind::BinOp(BinOpToken::Slash),
                nyxc_lexer::tokens::TokenKind::Lt => TokenKind::Lt,
                nyxc_lexer::tokens::TokenKind::Gt => TokenKind::Gt,
                nyxc_lexer::tokens::TokenKind::Percent => TokenKind::BinOp(BinOpToken::Percent),
                nyxc_lexer::tokens::TokenKind::OpenParen => {
                    self.open_braces.push(Delimiter::Parenthesis);
                    TokenKind::OpenDelim(Delimiter::Parenthesis)
                }
                nyxc_lexer::tokens::TokenKind::CloseParen => {
                    let last_brace = self.open_braces.last();
                    match last_brace {
                        Some(del) if del == &Delimiter::Parenthesis => {
                            self.open_braces.pop();
                        }
                        None => {
                            // Found Unexpected Closing Parenthesis
                            let expected_token = Token::new(TokenKind::SemiColon);
                            let got_token =
                                Some(Token::new(TokenKind::CloseDelim(Delimiter::Parenthesis)));
                            let level = DiagnosticLevel::Error;
                            let diagnostics =
                                Diagnostics::new(expected_token, got_token, level, None);

                            self.diagnostics.push(diagnostics);
                        }
                        Some(other) => {
                            let expected_token = Token::new(TokenKind::CloseDelim(*other));
                            let got_token =
                                Some(Token::new(TokenKind::CloseDelim(Delimiter::Parenthesis)));
                            let level = DiagnosticLevel::Error;
                            let diagnostics =
                                Diagnostics::new(expected_token, got_token, level, None);
                            self.diagnostics.push(diagnostics);
                        }
                    };
                    TokenKind::CloseDelim(Delimiter::Parenthesis)
                }
                nyxc_lexer::tokens::TokenKind::OpenBrace => {
                    self.open_braces.push(Delimiter::Brace);
                    TokenKind::OpenDelim(Delimiter::Brace)
                }
                nyxc_lexer::tokens::TokenKind::CloseBrace => {
                    let last_brace = self.open_braces.last();
                    match last_brace {
                        Some(del) if del == &Delimiter::Brace => {
                            self.open_braces.pop();
                        }
                        None => {
                            // Found Unexpected Closing Brace
                            let expected_token = Token::new(TokenKind::SemiColon);
                            let got_token =
                                Some(Token::new(TokenKind::CloseDelim(Delimiter::Brace)));
                            let level = DiagnosticLevel::Error;
                            let diagnostics =
                                Diagnostics::new(expected_token, got_token, level, None);

                            self.diagnostics.push(diagnostics);
                        }
                        Some(other) => {
                            let expected_token = Token::new(TokenKind::CloseDelim(*other));
                            let got_token =
                                Some(Token::new(TokenKind::CloseDelim(Delimiter::Brace)));
                            let level = DiagnosticLevel::Error;
                            let diagnostics =
                                Diagnostics::new(expected_token, got_token, level, None);
                            self.diagnostics.push(diagnostics);
                        }
                    };
                    TokenKind::CloseDelim(Delimiter::Brace)
                }
                nyxc_lexer::tokens::TokenKind::Unknown => {
                    let diagnostics = Diagnostics::new(
                        Token::dummy(),
                        Some(Token::new(TokenKind::Ident(Symbol::new(
                            &self.src[start..self.pos],
                        )))),
                        DiagnosticLevel::Error,
                        None,
                    );
                    self.diagnostics.push(diagnostics);
                    preceded_by_whitespace = true;
                    continue;
                }
                nyxc_lexer::tokens::TokenKind::Eof => {
                    self.open_braces.iter().for_each(|brace| {
                        let diagnostics = Diagnostics::new(
                            Token::new(TokenKind::CloseDelim(*brace)),
                            Some(Token::new(TokenKind::Eof)),
                            DiagnosticLevel::Error,
                            None,
                        );
                        self.diagnostics.push(diagnostics);
                    });
                    TokenKind::Eof
                }
            };

            return (Token::new(kind), preceded_by_whitespace);
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostics] {
        &self.diagnostics[..]
    }
}

#[cfg(test)]
mod tests {
    use nyxc_ast::{keywords::Kw, Lit, LitKind, Symbol, Token, TokenKind};

    use super::Lexer;

    #[test]
    fn lex_let() {
        let mut lexer = Lexer::new("let x = 2;");
        let got = lexer.tokenize();
        let expected = vec![
            Token::new(TokenKind::Ident(Symbol::new("let"))),
            Token::new(TokenKind::Ident(Symbol::new("x"))),
            Token::new(TokenKind::Eq),
            Token::new(TokenKind::Literal(Lit::new(LitKind::Int, Symbol::new("2")))),
            Token::new(TokenKind::SemiColon),
            Token::new(TokenKind::Eof),
        ];
        eprintln!("{:?}", lexer.diagnostics());
        assert_eq!(expected, got)
    }

    #[test]
    fn lex_return() {
        let mut lexer = Lexer::new("return;");
        let got = lexer.tokenize();
        let expected = vec![
            Token::new(TokenKind::Ident(Symbol::new("return"))),
            Token::new(TokenKind::SemiColon),
            Token::new(TokenKind::Eof),
        ];
        assert_eq!(expected, got);
    }

    #[test]
    fn lex_bool_true() {
        let mut lexer = Lexer::new("true;");
        let got = lexer.tokenize();
        let expected = vec![
            Token::new(TokenKind::Literal(Lit {
                kind: LitKind::Bool,
                symbol: Symbol::Keyword(Kw::True),
            })),
            Token::new(TokenKind::SemiColon),
            Token::new(TokenKind::Eof),
        ];
        assert_eq!(expected, got);
    }

    #[test]
    fn lex_bool_false() {
        let mut lexer = Lexer::new("false;");
        let got = lexer.tokenize();
        let expected = vec![
            Token::new(TokenKind::Literal(Lit {
                kind: LitKind::Bool,
                symbol: Symbol::Keyword(Kw::False),
            })),
            Token::new(TokenKind::SemiColon),
            Token::new(TokenKind::Eof),
        ];
        assert_eq!(expected, got);
    }

    #[test]
    fn lex_rarrow() {
        let mut lexer = Lexer::new("->");
        let got = lexer.tokenize();
        let expected = vec![Token::new(TokenKind::RArrow), Token::new(TokenKind::Eof)];
        assert_eq!(expected, got);
    }
}
