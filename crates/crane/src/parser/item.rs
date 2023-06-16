use thin_vec::ThinVec;
use tracing::trace;

use crate::ast::{Fn, FnParam, Ident, Item, ItemKind};
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::LexError;
use crate::parser::{ParseResult, Parser};

type ItemInfo = (Ident, ItemKind);

mod keywords {
    use smol_str::SmolStr;

    use crate::ast::{Ident, DUMMY_SPAN};

    pub const PUB: Ident = Ident {
        name: SmolStr::new_inline("pub"),
        span: DUMMY_SPAN,
    };

    pub const FN: Ident = Ident {
        name: SmolStr::new_inline("fn"),
        span: DUMMY_SPAN,
    };
}

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    /// Parses an [`Item`].
    pub fn parse_item(&mut self) -> ParseResult<Option<Item>> {
        trace!("parse_item");

        self.consume_keyword(keywords::PUB);

        Ok(self
            .parse_item_kind()?
            .map(|(name, kind)| Item { name, kind }))
    }

    fn parse_item_kind(&mut self) -> ParseResult<Option<ItemInfo>> {
        trace!("parse_item_kind");

        if self.consume_keyword(keywords::FN) {
            let (name, fun) = self.parse_fn()?;

            return Ok(Some((name, ItemKind::Fn(Box::new(fun)))));
        }

        Ok(None)
    }

    fn parse_fn(&mut self) -> ParseResult<(Ident, Fn)> {
        trace!("parse_fn");

        let ident = self.parse_ident()?;

        self.consume(TokenKind::OpenParen);

        let mut params = ThinVec::new();

        if !self.check(TokenKind::CloseParen) {
            loop {
                let param_name = self.parse_ident()?;

                self.consume(TokenKind::Colon);

                let ty_annotation = self.parse_ident()?;

                let span = param_name.span;

                params.push(FnParam {
                    name: param_name,
                    ty: ty_annotation,
                    span,
                });

                if !self.consume(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::CloseParen);

        self.consume(TokenKind::OpenBrace);

        let mut body = ThinVec::new();

        while let Some(stmt) = self.parse_stmt()? {
            body.push(stmt);
        }

        self.consume(TokenKind::CloseBrace);

        Ok((ident, Fn { params, body }))
    }
}
