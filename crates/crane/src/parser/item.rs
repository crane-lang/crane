use crate::ast::{Ident, Item, ItemKind};
use crate::lexer::token::TokenKind;
use crate::lexer::{token::Token, LexError};
use crate::parser::{ParseResult, Parser};

type ItemInfo = (Ident, ItemKind);

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    /// Parses an [`Item`].
    pub fn parse_item(&mut self) -> ParseResult<Item> {
        if self.check(TokenKind::Ident) {}

        self.parse_item_kind()?;

        Ok(todo!())
    }

    fn parse_item_kind(&mut self) -> ParseResult<Option<ItemInfo>> {
        Ok(None)
    }
}
