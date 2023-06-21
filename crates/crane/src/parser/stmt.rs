use crate::ast::{keywords, Local, LocalKind, Stmt, StmtKind};
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::LexError;
use crate::parser::{ParseError, ParseErrorKind, ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    /// Parses a [`Stmt`].
    #[tracing::instrument(skip(self))]
    pub fn parse_stmt(&mut self) -> ParseResult<Option<Stmt>> {
        if self.consume_keyword(keywords::LET) {
            let local = self.parse_local()?;

            let span = local.span;

            return Ok(Some(Stmt {
                kind: StmtKind::Local(Box::new(local)),
                span,
            }));
        }

        if let Some(expr) = self.parse_expr()? {
            let span = expr.span;

            return Ok(Some(Stmt {
                kind: StmtKind::Expr(Box::new(expr)),
                span,
            }));
        }

        Ok(None)
    }

    #[tracing::instrument(skip(self))]
    fn parse_local(&mut self) -> ParseResult<Local> {
        let name = self.parse_ident()?;

        self.consume(TokenKind::Equal);

        let init = self.parse_expr()?.ok_or_else(|| ParseError {
            kind: ParseErrorKind::Error(format!(
                "Expected an initializer for this `{}` binding.",
                keywords::LET
            )),
            span: self.token.span,
        })?;

        let span = name.span;

        Ok(Local {
            kind: LocalKind::Init(Box::new(init)),
            name,
            span,
            ty: None,
        })
    }
}
