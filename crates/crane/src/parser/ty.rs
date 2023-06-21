use crate::ast::{Ty, TyKind};
use crate::lexer::token::Token;
use crate::lexer::LexError;
use crate::parser::{ParseError, ParseErrorKind, ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    /// Parses a [`Ty`].
    #[tracing::instrument(skip(self))]
    pub fn parse_ty(&mut self) -> ParseResult<Ty> {
        if self.check_path() {
            let path = self.parse_path()?;

            let span = path.span.clone();

            return Ok(Ty {
                kind: TyKind::Path(path),
                span,
            });
        }

        Err(ParseError {
            kind: ParseErrorKind::Error("Expected a type.".to_string()),
            span: self.token.span,
        })
    }
}
