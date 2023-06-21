use crate::ast::{Ty, TyKind};
use crate::lexer::token::Token;
use crate::lexer::LexError;
use crate::parser::{ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    /// Parses a [`Ty`].
    #[tracing::instrument(skip(self))]
    pub fn parse_ty(&mut self) -> ParseResult<Ty> {
        let path = self.parse_path()?;

        let span = path.span.clone();

        Ok(Ty {
            kind: TyKind::Path(path),
            span,
        })
    }
}
