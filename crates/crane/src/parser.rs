mod error;

pub use error::*;

use std::iter::Peekable;

use thin_vec::ThinVec;
use tracing::trace;

use crate::ast::{Expr, ExprKind, Fn, Ident, Item, ItemKind, Stmt, StmtKind};
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::Lexer;

pub type ParseResult<T> = Result<T, ParseError>;

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

    pub fn parse(mut self) -> ParseResult<Vec<Item>> {
        trace!("Parsing program");

        let mut items = Vec::new();

        while !self.is_at_end().unwrap() {
            let item = self.parse_fn_item()?;

            items.push(item);
        }

        Ok(items)
    }

    fn peek(&mut self) -> ParseResult<Option<&Token>> {
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

    fn is_at_end(&mut self) -> ParseResult<bool> {
        Ok(self.peek()?.is_none())
    }

    fn advance(&mut self) -> ParseResult<Token> {
        let token = self.lexer.next().ok_or(ParseError {
            kind: ParseErrorKind::AdvancedPastEndOfInput,
            span: (0..0).into(),
        })?;
        let token = token?;

        match token.kind {
            TokenKind::Comment => self.advance(),
            _ => Ok(token),
        }
    }

    fn check(&mut self, kind: TokenKind) -> ParseResult<bool> {
        Ok(self
            .peek()?
            .map(|token| token.kind == kind)
            .unwrap_or(false))
    }

    fn check_and_consume(&mut self, kind: TokenKind) -> ParseResult<bool> {
        if self.check(kind)? {
            self.advance()?;

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> ParseResult<Token> {
        if self.check(kind)? {
            self.advance()
        } else {
            Err(ParseError {
                kind: ParseErrorKind::Error(message.to_string()),
                span: self
                    .peek()
                    .map(|token| {
                        token
                            .map(|token| token.span.clone())
                            .unwrap_or((0..0).into())
                    })
                    .unwrap_or((0..0).into()),
            })
        }
    }

    fn parse_fn_item(&mut self) -> ParseResult<Item> {
        trace!("Parsing function declaration");

        match self.peek()? {
            Some(peeked) if peeked.kind == TokenKind::Ident && peeked.lexeme == "pub" => {
                self.advance()?;
            }
            _ => {}
        }

        let fn_keyword = self.consume(TokenKind::Ident, "Expected 'fn'.")?;
        if fn_keyword.lexeme != "fn" {
            return Err(ParseError {
                kind: ParseErrorKind::Error("Expected 'fn'.".to_string()),
                span: fn_keyword.span,
            });
        }

        let name = self.consume(TokenKind::Ident, "Expected a function name.")?;

        self.consume(TokenKind::OpenParen, "Expected '('.")?;
        self.consume(TokenKind::CloseParen, "Expected ')'.")?;

        self.consume(TokenKind::OpenBrace, "Expected '{'.")?;

        let mut body = ThinVec::new();

        loop {
            if self.check(TokenKind::CloseBrace)? {
                break;
            }

            let fn_call = self.parse_call_expr()?;

            let span = fn_call.span;

            body.push(Stmt {
                kind: StmtKind::Expr(fn_call),
                span,
            });
        }

        self.consume(TokenKind::CloseBrace, "Expected '}'.")?;

        Ok(Item {
            kind: ItemKind::Fn(Box::new(Fn { body })),
            name: Ident(name.lexeme.into()),
        })
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        trace!("Parsing expression");

        if self.check(TokenKind::String)? {
            let token = self.advance()?;

            Ok(Expr {
                kind: ExprKind::Literal(token.lexeme),
                span: token.span,
            })
        } else {
            todo!()
        }
    }

    fn parse_call_expr(&mut self) -> ParseResult<Expr> {
        trace!("Parsing call expression");

        let callee = self.consume(TokenKind::Ident, "Expected a function name.")?;

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
            span: callee.span,
        };

        let span = callee.span.clone();

        Ok(Expr {
            kind: ExprKind::Call {
                fun: Box::new(callee),
                args: args.into_iter().map(Box::new).collect(),
            },
            span,
        })
    }
}
