use crate::ast::{Stmt, StmtKind, DUMMY_SPAN};
use crate::lexer::token::Token;
use crate::lexer::LexError;
use crate::parser::{ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    pub fn parse_stmt(&mut self) -> ParseResult<Option<Stmt>> {
        if let Some(expr) = self.parse_expr()? {
            return Ok(Some(Stmt {
                kind: StmtKind::Expr(expr),
                // TODO: Use a real span.
                span: DUMMY_SPAN,
            }));
        }

        Ok(None)
    }
}
