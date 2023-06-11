use std::iter::Peekable;

use thin_vec::ThinVec;
use tracing::trace;

use crate::ast::{Expr, ExprKind, Fn, Ident, Stmt, StmtKind};
use crate::lexer::token::TokenKind;
use crate::lexer::{token::Token, Lexer};
use crate::lexer::{LexError, LexErrorKind};

#[derive(Debug)]
pub enum ParseError {
    LexError(LexError),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::LexError(error) => error.to_string(),
            }
        )
    }
}

pub struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(input: &'src str) -> Self {
        dbg!(Lexer::new(input).collect::<Vec<_>>());

        Self {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, ParseError> {
        trace!("Parsing program");

        let mut statements = Vec::new();

        while !self.is_at_end().unwrap() {
            let fn_decl = self.parse_fn().map_err(ParseError::LexError)?;

            statements.push(fn_decl);
        }

        Ok(statements)
    }

    fn peek(&mut self) -> Result<Option<&Token>, LexError> {
        if let Some(token) = self.lexer.peek() {
            let next_token_kind = token
                .as_ref()
                .map(|token| token.kind)
                .map_err(|err| err.clone())?;

            match next_token_kind {
                TokenKind::Comment => {
                    self.advance()?;

                    return self.peek();
                }
                _ => {}
            }
        }

        if let Some(token) = self.lexer.peek() {
            let token = token.as_ref().map_err(|err| err.clone())?;

            Ok(Some(token))
        } else {
            Ok(None)
        }
    }

    fn is_at_end(&mut self) -> Result<bool, LexError> {
        Ok(self.peek()?.is_none())
    }

    fn advance(&mut self) -> Result<Token, LexError> {
        let token = self.lexer.next().ok_or(LexError {
            kind: LexErrorKind::Unknown,
            span: 0..1,
        })?;
        let token = token?;

        match token.kind {
            TokenKind::Comment => self.advance(),
            _ => Ok(token),
        }
    }

    fn check(&mut self, kind: TokenKind) -> Result<bool, LexError> {
        Ok(self
            .peek()?
            .map(|token| token.kind == kind)
            .unwrap_or(false))
    }

    fn check_and_consume(&mut self, kind: TokenKind) -> Result<bool, LexError> {
        if self.check(kind)? {
            self.advance()?;

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<Token, LexError> {
        if self.check(kind)? {
            self.advance()
        } else {
            Err(LexError {
                kind: LexErrorKind::Unknown,
                span: 0..1,
            })
        }
    }

    fn parse_fn(&mut self) -> Result<Stmt, LexError> {
        trace!("Parsing function declaration");

        match self.peek()? {
            Some(peeked) if peeked.kind == TokenKind::Ident && peeked.lexeme == "pub" => {
                self.advance()?;
            }
            _ => {}
        }

        let fn_keyword = self.advance()?;
        assert_eq!(fn_keyword.kind, TokenKind::Ident);
        assert_eq!(fn_keyword.lexeme, "fn");

        let name = self.advance()?;
        assert_eq!(name.kind, TokenKind::Ident);

        self.consume(TokenKind::OpenParen, "Expected '('.")?;
        self.consume(TokenKind::CloseParen, "Expected ')'.")?;

        self.consume(TokenKind::OpenBrace, "Expected '{'.")?;

        let mut body = ThinVec::new();

        loop {
            if self.check(TokenKind::CloseBrace)? {
                break;
            }

            let fn_call = self.parse_call_expr()?;

            body.push(Stmt {
                kind: StmtKind::Expr(fn_call),
            });
        }

        self.consume(TokenKind::CloseBrace, "Expected '}'.")?;

        Ok(Stmt {
            kind: StmtKind::Fn(Box::new(Fn {
                name: name.lexeme.into(),
                body,
            })),
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, LexError> {
        trace!("Parsing expression");

        if self.check(TokenKind::String)? {
            let token = self.advance()?;

            Ok(Expr {
                kind: ExprKind::Literal(token.lexeme),
            })
        } else {
            todo!()
        }
    }

    fn parse_call_expr(&mut self) -> Result<Expr, LexError> {
        trace!("Parsing call expression");

        let callee = self.advance()?;
        assert_eq!(callee.kind, TokenKind::Ident);

        self.consume(TokenKind::OpenParen, "Expected '('.")?;

        let mut args = ThinVec::new();

        if !self.check(TokenKind::CloseParen)? {
            loop {
                args.push(self.parse_expr()?);

                if !self.check_and_consume(TokenKind::Comma)? {
                    break;
                }
            }
        }

        self.consume(TokenKind::CloseParen, "Expected ')'.")?;

        let callee = Expr {
            kind: ExprKind::Variable {
                name: Ident(callee.lexeme),
            },
        };

        Ok(Expr {
            kind: ExprKind::Call {
                fun: Box::new(callee),
                args: args.into_iter().map(Box::new).collect(),
            },
        })
    }
}
