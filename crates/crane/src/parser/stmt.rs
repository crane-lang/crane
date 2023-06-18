use crate::ast::{Stmt, StmtKind};
use crate::lexer::token::Token;
use crate::lexer::LexError;
use crate::parser::{ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    pub fn parse_stmt(&mut self) -> ParseResult<Option<Stmt>> {
        if let Some(expr) = self.parse_expr()? {
            let span = expr.span;

            return Ok(Some(Stmt {
                kind: StmtKind::Expr(Box::new(expr)),
                span,
            }));
        }

        Ok(None)
    }
}
