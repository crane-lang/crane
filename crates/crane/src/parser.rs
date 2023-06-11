use std::iter::Peekable;

use crate::ast::{Fn, Stmt, StmtKind};
use crate::lexer::token::TokenKind;
use crate::lexer::{token::Token, Lexer};

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(Option<usize>, String),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::SyntaxError(position, reason) => match position {
                    Some(position) =>
                        format!("Syntax error at position {}: {}", position + 1, reason),
                    None => format!("Syntax error: {}", reason),
                },
            }
        )
    }
}

pub struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();

        while !self.is_at_end().unwrap() {
            let fn_decl = self.parse_fn().unwrap();

            statements.push(fn_decl);
        }

        Ok(statements)
    }

    fn peek(&mut self) -> Result<Option<&Token>, ()> {
        if let Some(token) = self.lexer.peek() {
            let next_token_kind = token.as_ref().map(|token| token.kind).map_err(|err| *err)?;

            match next_token_kind {
                TokenKind::Comment => {
                    self.advance()?;

                    return self.peek();
                }
                _ => {}
            }
        }

        if let Some(token) = self.lexer.peek() {
            let token = token.as_ref().map_err(|err| *err)?;

            Ok(Some(token))
        } else {
            Ok(None)
        }
    }

    fn is_at_end(&mut self) -> Result<bool, ()> {
        Ok(self.peek()?.is_none())
    }

    fn advance(&mut self) -> Result<Token, ()> {
        let token = self.lexer.next().ok_or(())?;
        let token = token?;

        match token.kind {
            TokenKind::Comment => self.advance(),
            _ => Ok(token),
        }
    }

    fn parse_fn(&mut self) -> Result<Stmt, ()> {
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

        let open_paren = self.advance()?;
        assert_eq!(open_paren.kind, TokenKind::OpenParen);

        let close_paren = self.advance()?;
        assert_eq!(close_paren.kind, TokenKind::CloseParen);

        let open_brace = self.advance()?;
        assert_eq!(open_brace.kind, TokenKind::OpenBrace);

        let close_brace = self.advance()?;
        assert_eq!(close_brace.kind, TokenKind::CloseBrace);

        Ok(Stmt {
            kind: StmtKind::Fn(Box::new(Fn {
                name: name.lexeme.into(),
            })),
        })
    }
}
