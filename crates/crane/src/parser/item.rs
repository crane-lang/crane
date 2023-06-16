use crate::ast::Item;
use crate::lexer::{token::Token, LexError};
use crate::parser::{ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    /// Parses an [`Item`].
    pub fn parse_item(&mut self) -> ParseResult<Item> {
        todo!()
    }
}
