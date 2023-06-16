use crate::ast::{Expr, ExprKind, DUMMY_SPAN};
use crate::lexer::token::Token;
use crate::lexer::LexError;
use crate::parser::{ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    pub fn parse_expr(&mut self) -> ParseResult<Option<Expr>> {
        Ok(Some(Expr {
            kind: ExprKind::Call { fun: (), args: () },
            // TODO: Use a real span.
            span: DUMMY_SPAN,
        }));

        Ok(None)
    }

    // fn parse_call_expr(&mut self) -> ParseResult<>
}
