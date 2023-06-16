use crate::ast::Expr;
use crate::lexer::{token::Token, LexError};
use crate::parser::{ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        todo!()
    }
}
