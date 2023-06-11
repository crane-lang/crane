use std::iter::Peekable;

use crate::ast::{Expr, Stmt};
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

    pub fn parse(self) -> Result<Vec<Stmt>, ParseError> {
        let tokens: Vec<Token> = self
            .lexer
            .collect::<Result<Vec<_>, _>>()
            .map_err(|err| ParseError::SyntaxError(Some(1), "Failed to lex".to_string()))?;

        let (token, rest) = tokens
            .split_first()
            .ok_or(ParseError::SyntaxError(Some(1), "No token".to_string()))?;

        let token = dbg!(token);
        let rest = dbg!(rest);

        let statements = Vec::new();

        Ok(statements)
    }
}
