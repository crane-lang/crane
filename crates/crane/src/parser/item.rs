use thin_vec::ThinVec;
use tracing::trace;

use crate::ast::{Fn, Ident, Item, ItemKind};
use crate::lexer::token::TokenKind;
use crate::lexer::{token::Token, LexError};
use crate::parser::{ParseResult, Parser};

type ItemInfo = (Ident, ItemKind);

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    /// Parses an [`Item`].
    pub fn parse_item(&mut self) -> ParseResult<Option<Item>> {
        trace!("parse_item");

        self.consume_keyword(Ident("pub".into()));

        Ok(self
            .parse_item_kind()?
            .map(|(name, kind)| Item { name, kind }))
    }

    fn parse_item_kind(&mut self) -> ParseResult<Option<ItemInfo>> {
        trace!("parse_item_kind");

        if self.consume_keyword(Ident("fn".into())) {
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
            }
        }

        self.consume(TokenKind::CloseParen);

        Ok((
            ident,
            Fn {
                params,
                body: ThinVec::new(),
            },
        ))
    }
}

// fn parse_fn_item(&mut self) -> ParseResult<Item> {
//     trace!("Parsing function declaration");

//     if !self.check(TokenKind::CloseParen)? {
//         loop {
//             let param_name =
//                 self.consume(TokenKind::Ident, "Expected a function parameter name.")?;

//             self.consume(TokenKind::Colon, "Expected a ':'.")?;

//             let ty_annotation =
//                 self.consume(TokenKind::Ident, "Expected a type annotation.")?;

//             params.push(FnParam {
//                 name: Ident(param_name.lexeme.into()),
//                 ty: Ident(ty_annotation.lexeme.into()),
//                 span: param_name.span,
//             });

//             if !self.check_and_consume(TokenKind::Comma)? {
//                 break;
//             }
//         }
//     }

//     self.consume(
//         TokenKind::CloseParen,
//         "Expected a ')' to close the parameter list.",
//     )?;

//     self.consume(TokenKind::OpenBrace, "Expected '{'.")?;

//     let mut body = ThinVec::new();

//     loop {
//         if self.check(TokenKind::CloseBrace)? {
//             break;
//         }

//         let fn_call = self.parse_call_expr(None)?;

//         let span = fn_call.span;

//         body.push(Stmt {
//             kind: StmtKind::Expr(fn_call),
//             span,
//         });
//     }

//     self.consume(TokenKind::CloseBrace, "Expected '}'.")?;

//     Ok(Item {
//         kind: ItemKind::Fn(Box::new(Fn { params, body })),
//         name: Ident(name.lexeme.into()),
//     })
// }
