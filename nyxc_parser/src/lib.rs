use lexer::Lexer;
use nyxc_ast::{
    ast::{
        Expression, FunctionParameters, InflixOp, Let, Precedence, PrefixOp, Program, Statement,
        TypeInfo,
    },
    diagnostics::{DiagnosticLevel, Diagnostics},
    keywords::Kw,
    BinOpToken, Delimiter, InnerSymbol, Lit, LitKind, Symbol, Token, TokenKind,
};
use std::vec::IntoIter;

pub mod lexer;

pub struct Parser {
    tokens_iter: IntoIter<Token>,
    tokens: Vec<Token>,
    token: Token,
    open_delim: Vec<Delimiter>,
    prev_token: Token,
    diagnostics: Vec<Diagnostics>,
}

impl Parser {
    pub fn new(src: &str) -> Self {
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize();
        let diagnostics = lexer.diagnostics().to_vec();

        Self {
            tokens_iter: tokens.clone().into_iter(),
            tokens,
            token: Token::dummy(),
            open_delim: vec![],
            prev_token: Token::dummy(),
            diagnostics,
        }
    }

    pub fn parse(&mut self) -> Result<Program, &[Diagnostics]> {
        if !self.diagnostics.is_empty() {
            return Err(&self.diagnostics);
        }

        self.bump();

        let mut program = Program::new();
        while self.token.kind != TokenKind::Eof {
            match self.parse_statement() {
                Ok(stmt) => program.push(stmt),
                Err(e) => {
                    self.diagnostics.push(e);
                    return Err(&self.diagnostics);
                }
            }
            self.bump();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement, Diagnostics> {
        match &self.token.kind {
            TokenKind::Ident(symbol) => match symbol {
                Symbol::Keyword(kw) => match kw {
                    Kw::Let => self.parse_let(),
                    Kw::Return => self.parse_return(),
                    Kw::Fn => self.parse_function(),
                    _ => self.parse_expression_statement(),
                },
                Symbol::Symbol(_) => self.parse_expression_statement(),
            },
            _ => self.parse_expression_statement(),
        }
    }

    fn peek_token(&self) -> Token {
        self.tokens_iter
            .clone()
            .next()
            .unwrap_or(Token::new(TokenKind::Eof))
            .to_owned()
    }

    fn bump(&mut self) -> &Token {
        let next_token = self
            .tokens_iter
            .next()
            .unwrap_or(Token::new(TokenKind::Eof));
        self.prev_token = std::mem::replace(&mut self.token, next_token);
        &self.token
    }

    fn optional_expect(&mut self, expected: &Token) -> Option<&Token> {
        let next_token = self.peek_token();
        if &next_token == expected {
            self.bump();
            Some(&self.token)
        } else {
            None
        }
    }

    fn expect_ident(&mut self) -> Result<InnerSymbol, Diagnostics> {
        let got = self.bump();
        let diagnostic_err = Diagnostics::new(
            Token::new(TokenKind::Ident(Symbol::new("var_name"))),
            Some(got.to_owned()),
            DiagnosticLevel::Error,
            None,
        );

        match &got.kind {
            TokenKind::Ident(symbol) => match symbol {
                Symbol::Keyword(_) => Err(diagnostic_err),
                Symbol::Symbol(identifier) => Ok(identifier.to_owned()),
            },
            _ => Err(diagnostic_err),
        }
    }

    fn expect_optional_type_info(&mut self) -> Result<Option<TypeInfo>, Diagnostics> {
        if self
            .optional_expect(&Token::new(TokenKind::Colon))
            .is_some()
        {
            let ty = self.expect_ident()?;
            return Ok(Some(TypeInfo { ty }));
        }

        Ok(None)
    }

    fn expect_type_info(&mut self) -> Result<TypeInfo, Diagnostics> {
        self.expect(Token::new(TokenKind::Colon))?;
        Ok(TypeInfo {
            ty: self.expect_ident()?,
        })
    }

    fn assert_literal(&mut self, lit_kind: LitKind) -> Result<Lit, Diagnostics> {
        match &self.token.kind {
            TokenKind::Literal(l) if l.kind == lit_kind => Ok(l.to_owned()),
            _ => Err(Diagnostics::new(
                Token::new(TokenKind::Literal(Lit {
                    kind: lit_kind,
                    symbol: lit_kind.default_symbol(),
                })),
                Some(self.token.to_owned()),
                DiagnosticLevel::Error,
                None,
            )),
        }
    }

    fn assert(&mut self, expected: Token) -> Result<&Token, Diagnostics> {
        if &self.token == &expected {
            Ok(&self.token)
        } else {
            Err(Diagnostics::new(
                expected.to_owned(),
                Some(self.token.to_owned()),
                DiagnosticLevel::Error,
                None,
            ))
        }
    }

    fn expect(&mut self, expected: Token) -> Result<&Token, Diagnostics> {
        let got = self.bump();
        if got == &expected {
            Ok(&got)
        } else {
            Err(Diagnostics::new(
                expected.to_owned(),
                Some(got.to_owned()),
                DiagnosticLevel::Error,
                None,
            ))
        }
    }

    fn expect_keyword(&mut self, keyword: Kw) -> Result<&Token, Diagnostics> {
        let symbol = Symbol::Keyword(keyword);
        let expected_keyword = Token::new(TokenKind::Ident(symbol));
        self.expect(expected_keyword)
    }

    fn parse_let(&mut self) -> Result<Statement, Diagnostics> {
        let mutable = self
            .optional_expect(&Token::new(TokenKind::Ident(Symbol::Keyword(Kw::Mut))))
            .is_some();

        let identifier = self.expect_ident()?.to_owned();

        let ty = self.expect_optional_type_info()?;

        self.expect(Token::new(TokenKind::Eq))?;

        self.bump();

        let expression = self.parse_expression(Precedence::Lowest)?;

        Ok(Statement::Let(Let {
            mutable,
            identifier,
            expression,
            ty,
        }))
    }

    fn parse_reassignment(&mut self, left: Expression) -> Result<Expression, Diagnostics> {
        let identifier = match left {
            Expression::Identifier(symbol) => match symbol {
                Symbol::Keyword(kw) => {
                    if kw != Kw::Underscore {
                        return Err(Diagnostics::new(
                            Token::new(TokenKind::Ident(Symbol::new("var_name"))),
                            Some(Token::new(TokenKind::Ident(symbol.to_owned()))),
                            DiagnosticLevel::Error,
                            Some(format!("found keyword: {kw}, can't assign to a keyword")),
                        ));
                    } else {
                        InnerSymbol::underscore()
                    }
                }
                Symbol::Symbol(inner) => inner,
            },
            _ => {
                return Err(Diagnostics::new(
                    Token::new(TokenKind::Ident(Symbol::new("var_name"))),
                    None,
                    DiagnosticLevel::Error,
                    Some(format!(
                        "can only assign to either a variable or the '_' keyword"
                    )),
                ));
            }
        };

        self.assert(Token::new(TokenKind::Eq))?;

        self.bump();

        let expression = Box::new(self.parse_expression(Precedence::Lowest)?);

        Ok(Expression::Reassignment {
            expression,
            identifier,
        })
    }

    fn parse_function(&mut self) -> Result<Statement, Diagnostics> {
        let name = self.expect_ident()?;
        let mut parameters = vec![];

        self.expect(Token::new(TokenKind::OpenDelim(Delimiter::Parenthesis)))?;
        let close_delim = Token::new(TokenKind::CloseDelim(Delimiter::Parenthesis));

        while self.peek_token().kind != close_delim.kind {
            if matches!(self.token.kind, TokenKind::Eof | TokenKind::SemiColon) {
                return Err(Diagnostics::new(
                    close_delim,
                    Some(self.token.to_owned()),
                    DiagnosticLevel::Error,
                    None,
                ));
            }

            let ident = self.expect_ident()?;
            let ty = self.expect_type_info()?;

            parameters.push(FunctionParameters { ident, ty });

            if self.peek_token().kind == TokenKind::Comma {
                self.bump();
                continue;
            }
        }

        self.bump();

        let ret_type = if self
            .optional_expect(&Token::new(TokenKind::RArrow))
            .is_some()
        {
            Some(TypeInfo {
                ty: self.expect_ident()?,
            })
        } else {
            None
        };

        self.expect(Token::new(TokenKind::OpenDelim(Delimiter::Brace)))?;
        let body = self.parse_blocks(Delimiter::Brace)?;

        Ok(Statement::Function {
            parameters,
            name,
            ret_type,
            body,
        })
    }

    fn parse_ident(&mut self) -> Result<InnerSymbol, Diagnostics> {
        Ok(self.expect_ident()?)
    }

    fn parse_int_literal(&mut self) -> Result<i64, Diagnostics> {
        let lit = self.assert_literal(LitKind::Int)?;
        let symbol = lit.symbol.get_identifier().expect("E1: Parser: Internal compiler error: parse_int_literal! This should never happen, please report if you see this!");
        symbol.parse::<i64>().map_err(|_| {
            Diagnostics::new(
                Token::dummy(),
                Some(self.token.clone()),
                DiagnosticLevel::Error,
                Some("couldn't parse the symbol as i64".to_owned()),
            )
        })
    }

    fn parse_float_literal(&mut self) -> Result<f64, Diagnostics> {
        let lit = self.assert_literal(LitKind::Float)?;
        let symbol = lit.symbol.get_identifier().expect("E2: Parser: Internal compiler error: parse_float_literal! This should never happen, please report if you see this!");
        symbol.parse::<f64>().map_err(|_| {
            Diagnostics::new(
                Token::dummy(),
                Some(self.token.clone()),
                DiagnosticLevel::Error,
                Some("couldn't parse the symbol as f64".to_owned()),
            )
        })
    }

    fn parse_bool_literal(&mut self) -> Result<bool, Diagnostics> {
        let lit = self.assert_literal(LitKind::Bool)?;
        let symbol = lit.symbol.get_keyword().expect("E3: Parser: Internal compiler error: parse_bool_literal! This should never happen, please report if you see this!");
        match symbol {
            Kw::True => Ok(true),
            Kw::False => Ok(false),
            _ => Err(Diagnostics::new(
                Token::dummy(),
                Some(self.token.clone()),
                DiagnosticLevel::Error,
                Some("couldn't parse the symbol as bool".to_owned()),
            )),
        }
    }

    fn parse_string_literal(&mut self) -> Result<String, Diagnostics> {
        let lit = self.assert_literal(LitKind::Str { terminated: true })?;
        let symbol = lit.symbol.get_identifier().expect("E4: Parser: Internal compiler error: parse_string_literal! This should never happen, please report if you see this!");
        Ok(symbol.to_owned())
    }

    fn parse_return(&mut self) -> Result<Statement, Diagnostics> {
        self.bump();
        let right = self.parse_expression(Precedence::Lowest)?;
        Ok(Statement::Return(right))
    }

    fn parse_prefix(&mut self) -> Result<Expression, Diagnostics> {
        let operator = match &self.token.kind {
            TokenKind::Not => PrefixOp::Bang,
            TokenKind::BinOp(BinOpToken::Minus) => PrefixOp::Minus,
            _ => {
                return Err(Diagnostics::new(
                    Token::dummy(),
                    Some(self.token.clone()),
                    DiagnosticLevel::Error,
                    Some("Expected a prefix token (! or -)".to_owned()),
                ));
            }
        };

        self.bump();
        let expr = Box::new(self.parse_expression(Precedence::Prefix)?);

        Ok(Expression::Prefix { operator, expr })
    }

    fn parse_inflix(&mut self, left: Expression) -> Result<Expression, Diagnostics> {
        let left = Box::new(left);

        let operator: InflixOp = match self.token.clone().try_into() {
            Ok(op) => op,
            Err(_) => unreachable!(),
        };

        let precedence = self.token.get_precedence();

        self.bump();

        let right = Box::new(self.parse_expression(precedence)?);
        Ok(Expression::Inflix {
            operator,
            left,
            right,
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, Diagnostics> {
        self.bump();

        let expr = self.parse_expression(Precedence::Lowest)?;
        self.assert(Token::new(TokenKind::CloseDelim(Delimiter::Parenthesis)))?;
        self.open_delim.pop();

        Ok(expr)
    }

    fn parse_blocks(&mut self, delim: Delimiter) -> Result<Expression, Diagnostics> {
        self.open_delim.push(delim);
        match delim {
            Delimiter::Parenthesis => self.parse_grouped_expression(),
            Delimiter::Brace => {
                let mut block = vec![];
                while !matches!(
                    self.token.kind,
                    TokenKind::CloseDelim(Delimiter::Brace) | TokenKind::Eof
                ) {
                    self.bump();

                    let ret = self.parse_statement()?;

                    if Statement::Expr(Expression::None) != ret {
                        block.push(ret);
                    }
                }

                self.open_delim.pop();

                if block.is_empty() {
                    Ok(Expression::None)
                } else {
                    Ok(Expression::Block(block))
                }
            }
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression, Diagnostics> {
        self.bump();

        // parse condition
        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.assert(Token::new(TokenKind::OpenDelim(Delimiter::Brace)))?;

        // parse consequence block
        let consequence = Box::new(self.parse_blocks(Delimiter::Brace)?);

        // parse alternate block
        let alternate = if self
            .optional_expect(&Token::new(TokenKind::Ident(Symbol::Keyword(Kw::Else))))
            .is_some()
        {
            self.bump();
            self.assert(Token::new(TokenKind::OpenDelim(Delimiter::Brace)))?;
            Some(Box::new(self.parse_blocks(Delimiter::Brace)?))
        } else {
            None
        };

        Ok(Expression::If {
            condition,
            consequence,
            alternate,
        })
    }

    fn parse_literal(&mut self, lit_kind: LitKind) -> Result<Expression, Diagnostics> {
        Ok(Expression::Literal(match lit_kind {
            LitKind::Bool => self.parse_bool_literal()?.into(),
            LitKind::Int => self.parse_int_literal()?.into(),
            LitKind::Float => self.parse_float_literal()?.into(),
            LitKind::Str { .. } => self.parse_string_literal()?.into(),
        }))
    }

    fn parse_loop(&mut self) -> Result<Expression, Diagnostics> {
        self.bump();

        self.assert(Token::new(TokenKind::OpenDelim(Delimiter::Brace)))?;

        let contents = Box::new(self.parse_blocks(Delimiter::Brace)?);
        Ok(Expression::Loop(contents))
    }

    fn parse_continue(&mut self) -> Result<Expression, Diagnostics> {
        self.bump();
        self.assert(Token::new(TokenKind::SemiColon))?;
        Ok(Expression::Continue)
    }

    fn parse_break(&mut self) -> Result<Expression, Diagnostics> {
        self.bump();

        let right = Box::new(self.parse_expression(Precedence::Lowest)?);
        Ok(Expression::Break(right))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Diagnostics> {
        let diagnostic_err = Diagnostics::new(
            Token::dummy(),
            Some(self.token.clone()),
            DiagnosticLevel::Error,
            Some(format!(
                "Unexpected token {:?} received after: {:?}",
                self.token, self.prev_token
            )),
        );

        let mut left = match &self.token.kind {
            TokenKind::Not | TokenKind::BinOp(BinOpToken::Minus) => self.parse_prefix()?,
            TokenKind::OpenDelim(delim) => self.parse_blocks(delim.clone())?,
            TokenKind::CloseDelim(delim) => {
                if self.open_delim.last() == Some(delim) {
                    return Ok(Expression::None);
                } else {
                    return Err(diagnostic_err);
                }
            }
            TokenKind::Literal(lit) => self.parse_literal(lit.kind.clone())?,
            TokenKind::Ident(symbol) => match symbol {
                Symbol::Keyword(kw) => match kw {
                    Kw::Underscore => {
                        Expression::Identifier(Symbol::Symbol(InnerSymbol::underscore()))
                    }
                    Kw::If => self.parse_if_expression()?,
                    Kw::Loop => self.parse_loop()?,
                    Kw::Continue => self.parse_continue()?,
                    Kw::Break => self.parse_break()?,
                    _ => return Err(diagnostic_err),
                },
                Symbol::Symbol(_) => Expression::Identifier(symbol.to_owned()),
            },
            TokenKind::Eof | TokenKind::SemiColon => {
                return Ok(Expression::None);
            }
            _ => return Err(diagnostic_err),
        };

        self.bump();

        while ![TokenKind::SemiColon, TokenKind::Eof].contains(&self.token.kind)
            && precedence < self.token.get_precedence()
        {
            left = match self.token.kind {
                TokenKind::Eq => self.parse_reassignment(left)?,
                TokenKind::Lt
                | TokenKind::Le
                | TokenKind::Gt
                | TokenKind::Ge
                | TokenKind::EqEq
                | TokenKind::Ne
                | TokenKind::And
                | TokenKind::BinOp(_)
                | TokenKind::BinOpEq(_)
                | TokenKind::Or => self.parse_inflix(left)?,
                TokenKind::OpenDelim(Delimiter::Parenthesis) => self.parse_call()?,
                _ => return Ok(left),
            };
        }

        Ok(left)
    }

    fn parse_call(&mut self) -> Result<Expression, Diagnostics> {
        // extract the name of the function being called
        let identifier = match &self.prev_token.kind {
            TokenKind::Ident(inner) => inner.to_owned(),
            _ => {
                return Err(Diagnostics::new(
                    Token::dummy(),
                    Some(self.token.clone()),
                    DiagnosticLevel::Error,
                    Some("unexpected '(' received".to_owned()),
                ))
            }
        };

        let mut arguments = vec![];
        let close_delim = Token::new(TokenKind::CloseDelim(Delimiter::Parenthesis));

        self.bump();
        while self.token.kind != close_delim.kind {
            if matches!(self.token.kind, TokenKind::Eof | TokenKind::SemiColon) {
                return Err(Diagnostics::new(
                    close_delim,
                    Some(self.token.clone()),
                    DiagnosticLevel::Error,
                    None,
                ));
            }

            let expr = self.parse_expression(Precedence::Lowest)?;
            arguments.push(expr);

            if self.token.kind == TokenKind::Comma {
                self.bump();
                continue;
            }
        }

        self.bump();

        Ok(Expression::Call {
            identifier,
            arguments,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Diagnostics> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        Ok(Statement::Expr(expr))
    }
}

#[cfg(test)]
mod tests {
    use nyxc_ast::{
        ast::{
            Expression, FunctionParameters, InflixOp, Let, Literal, PrefixOp, Statement, TypeInfo,
        },
        InnerSymbol, Symbol,
    };

    use crate::Parser;

    #[test]
    fn parse_prefix() {
        let input = "!true;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Prefix {
            operator: PrefixOp::Bang,
            expr: Box::new(Expression::Literal(Literal::Bool(true))),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_inflix() {
        let input = "2 + 3;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Inflix {
            operator: InflixOp::Plus,
            left: Box::new(Expression::Literal(Literal::Int(2))),
            right: Box::new(Expression::Literal(Literal::Int(3))),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_inflix_complex() {
        let input = "2 * 2^3 + 3;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Inflix {
            operator: InflixOp::Plus,
            left: Box::new(Expression::Inflix {
                operator: InflixOp::Product,
                left: Box::new(Expression::Literal(Literal::Int(2))),
                right: Box::new(Expression::Inflix {
                    operator: InflixOp::Power,
                    left: Box::new(Expression::Literal(Literal::Int(2))),
                    right: Box::new(Expression::Literal(Literal::Int(3))),
                }),
            }),
            right: Box::new(Expression::Literal(Literal::Int(3))),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_return() {
        let input = "return;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Return(Expression::None)];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_return_expr() {
        let input = "return 2 % 3;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Return(Expression::Inflix {
            operator: InflixOp::Modulo,
            left: Box::new(Expression::Literal(Literal::Int(2))),
            right: Box::new(Expression::Literal(Literal::Int(3))),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_grouped_expression() {
        let input = "(2 + 2) / 2;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Inflix {
            operator: InflixOp::Divide,
            left: Box::new(Expression::Inflix {
                operator: InflixOp::Plus,
                left: Box::new(Expression::Literal(Literal::Int(2))),
                right: Box::new(Expression::Literal(Literal::Int(2))),
            }),
            right: Box::new(Expression::Literal(Literal::Int(2))),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_let() {
        let input = "let x = 2;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Let(Let {
            mutable: false,
            expression: Expression::Literal(Literal::Int(2)),
            identifier: InnerSymbol::new("x"),
            ty: None,
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_let_mutable() {
        let input = "let mut x = 2;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Let(Let {
            mutable: true,
            expression: Expression::Literal(Literal::Int(2)),
            identifier: InnerSymbol::new("x"),
            ty: None,
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_let_ty() {
        let input = "let x: int = 2;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Let(Let {
            mutable: false,
            expression: Expression::Literal(Literal::Int(2)),
            identifier: InnerSymbol::new("x"),
            ty: Some(TypeInfo {
                ty: InnerSymbol::new("int"),
            }),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_let_mut_ty() {
        let input = "let mut x: int = 2;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Let(Let {
            mutable: true,
            expression: Expression::Literal(Literal::Int(2)),
            identifier: InnerSymbol::new("x"),
            ty: Some(TypeInfo {
                ty: InnerSymbol::new("int"),
            }),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_reassignment() {
        let input = "x = 2;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Reassignment {
            identifier: InnerSymbol::new("x"),
            expression: Box::new(Expression::Literal(Literal::Int(2))),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn underscore_reassignment() {
        let input = "_ = 2;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Reassignment {
            identifier: InnerSymbol::underscore(),
            expression: Box::new(Expression::Literal(Literal::Int(2))),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_blocks() {
        let input = "
            {
                let x = 2;
                x + 2;
            }
            ";
        let mut parser = Parser::new(input);
        let got = parser.parse().unwrap();

        let expected = vec![Statement::Expr(Expression::Block(vec![
            Statement::Let(Let {
                mutable: false,
                expression: Expression::Literal(Literal::Int(2)),
                identifier: InnerSymbol::new("x"),
                ty: None,
            }),
            Statement::Expr(Expression::Inflix {
                operator: InflixOp::Plus,
                left: Box::new(Expression::Identifier(Symbol::new("x"))),
                right: Box::new(Expression::Literal(Literal::Int(2))),
            }),
        ]))];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_blocks_empty() {
        let mut parser = Parser::new("{}");
        let got = parser.parse().unwrap();

        let expected = vec![Statement::Expr(Expression::None)];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_if() {
        let input = "
            if true {
                2
            };
            ";
        let mut parser = Parser::new(input);
        let got = parser.parse().unwrap();

        let expected = vec![Statement::Expr(Expression::If {
            condition: Box::new(Expression::Literal(Literal::Bool(true))),
            consequence: Box::new(Expression::Block(vec![Statement::Expr(
                Expression::Literal(Literal::Int(2)),
            )])),
            alternate: None,
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_if_else() {
        let input = "
            if true {
                2
            } else {
                3
            };
            ";
        let mut parser = Parser::new(input);
        let got = parser.parse().unwrap();

        let expected = vec![Statement::Expr(Expression::If {
            condition: Box::new(Expression::Literal(Literal::Bool(true))),
            consequence: Box::new(Expression::Block(vec![Statement::Expr(
                Expression::Literal(Literal::Int(2)),
            )])),
            alternate: Some(Box::new(Expression::Block(vec![Statement::Expr(
                Expression::Literal(Literal::Int(3)),
            )]))),
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_continue() {
        let input = "continue;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Continue)];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_break() {
        let input = "break;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Break(Box::new(
            Expression::None,
        )))];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_break_expr() {
        let input = "break 2;";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Break(Box::new(
            Expression::Literal(Literal::Int(2)),
        )))];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_loop() {
        let input = "loop {};";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Loop(Box::new(
            Expression::None,
        )))];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_loop_break() {
        let input = "loop {
             break 2;
        };";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Loop(Box::new(
            Expression::Block(vec![Statement::Expr(Expression::Break(Box::new(
                Expression::Literal(Literal::Int(2)),
            )))]),
        )))];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_loop_break_empty() {
        let input = "loop {
             break;
        };";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Loop(Box::new(
            Expression::Block(vec![Statement::Expr(Expression::Break(Box::new(
                Expression::None,
            )))]),
        )))];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_loop_continue() {
        let input = "loop {
             continue;
        };";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Loop(Box::new(
            Expression::Block(vec![Statement::Expr(Expression::Continue)]),
        )))];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_function_empty() {
        let input = "fn name() {}";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();

        let expected = vec![Statement::Function {
            parameters: vec![],
            name: InnerSymbol::new("name"),
            ret_type: None,
            body: Expression::None,
        }];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_function_single_param() {
        let input = "fn name(x: int) {}";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();

        let expected = vec![Statement::Function {
            parameters: vec![FunctionParameters {
                ident: InnerSymbol::new("x"),
                ty: TypeInfo {
                    ty: InnerSymbol::new("int"),
                },
            }],
            name: InnerSymbol::new("name"),
            ret_type: None,
            body: Expression::None,
        }];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_function_multiple_param() {
        let input = "fn name(x: int, y: float, z: str) {}";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();

        let expected = vec![Statement::Function {
            parameters: vec![
                FunctionParameters {
                    ident: InnerSymbol::new("x"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("int"),
                    },
                },
                FunctionParameters {
                    ident: InnerSymbol::new("y"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("float"),
                    },
                },
                FunctionParameters {
                    ident: InnerSymbol::new("z"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("str"),
                    },
                },
            ],
            name: InnerSymbol::new("name"),
            ret_type: None,
            body: Expression::None,
        }];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_function_multiple_param_returns() {
        let input = "fn name(x: int, y: float, z: str) -> int {}";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();

        let expected = vec![Statement::Function {
            parameters: vec![
                FunctionParameters {
                    ident: InnerSymbol::new("x"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("int"),
                    },
                },
                FunctionParameters {
                    ident: InnerSymbol::new("y"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("float"),
                    },
                },
                FunctionParameters {
                    ident: InnerSymbol::new("z"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("str"),
                    },
                },
            ],
            name: InnerSymbol::new("name"),
            ret_type: Some(TypeInfo {
                ty: InnerSymbol::new("int"),
            }),
            body: Expression::None,
        }];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_function_multiple_param_trailing_comma_returns() {
        let input = "fn name(x: int, y: float, z: str,) -> int {}";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();

        let expected = vec![Statement::Function {
            parameters: vec![
                FunctionParameters {
                    ident: InnerSymbol::new("x"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("int"),
                    },
                },
                FunctionParameters {
                    ident: InnerSymbol::new("y"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("float"),
                    },
                },
                FunctionParameters {
                    ident: InnerSymbol::new("z"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("str"),
                    },
                },
            ],
            name: InnerSymbol::new("name"),
            ret_type: Some(TypeInfo {
                ty: InnerSymbol::new("int"),
            }),
            body: Expression::None,
        }];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_function_multiple_param_returns_body() {
        let input = "fn name(x: int, y: float, z: str) -> int {
            x + y + z
        }";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();

        let expected = vec![Statement::Function {
            parameters: vec![
                FunctionParameters {
                    ident: InnerSymbol::new("x"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("int"),
                    },
                },
                FunctionParameters {
                    ident: InnerSymbol::new("y"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("float"),
                    },
                },
                FunctionParameters {
                    ident: InnerSymbol::new("z"),
                    ty: TypeInfo {
                        ty: InnerSymbol::new("str"),
                    },
                },
            ],
            name: InnerSymbol::new("name"),
            ret_type: Some(TypeInfo {
                ty: InnerSymbol::new("int"),
            }),
            body: Expression::Block(vec![Statement::Expr(Expression::Inflix {
                operator: InflixOp::Plus,
                left: Box::new(Expression::Inflix {
                    operator: InflixOp::Plus,
                    left: Box::new(Expression::Identifier(Symbol::Symbol(InnerSymbol::new(
                        "x",
                    )))),
                    right: Box::new(Expression::Identifier(Symbol::Symbol(InnerSymbol::new(
                        "y",
                    )))),
                }),
                right: Box::new(Expression::Identifier(Symbol::Symbol(InnerSymbol::new(
                    "z",
                )))),
            })]),
        }];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_parens_block() {
        let input = "(())";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::None)];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_blocks_embedded() {
        let input = "{{}}";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::None)];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_call() {
        let input = "name();";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Call {
            identifier: Symbol::Symbol(InnerSymbol::new("name")),
            arguments: vec![],
        })];

        assert_eq!(got, expected);
    }

    #[test]
    fn parse_call_args() {
        let input = "name(x, y);";
        let mut parser = Parser::new(input);

        let got = parser.parse().unwrap();
        let expected = vec![Statement::Expr(Expression::Call {
            identifier: Symbol::Symbol(InnerSymbol::new("name")),
            arguments: vec![
                Expression::Identifier(Symbol::Symbol(InnerSymbol::new("x"))),
                Expression::Identifier(Symbol::Symbol(InnerSymbol::new("y"))),
            ],
        })];

        assert_eq!(got, expected);
    }
}
