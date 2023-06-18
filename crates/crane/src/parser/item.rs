use thin_vec::ThinVec;
use tracing::trace;

use crate::ast::{
    keywords, FieldDecl, Fn, FnParam, Ident, Item, ItemKind, StructDecl, UnionDecl, Variant,
    VariantData,
};
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::LexError;
use crate::parser::{ParseResult, Parser};

type ItemInfo = (Ident, ItemKind);

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

        if self.consume_keyword(keywords::STRUCT) {
            let (name, struct_decl) = self.parse_struct_decl()?;

            return Ok(Some((name, ItemKind::Struct(struct_decl))));
        }

        if self.consume_keyword(keywords::UNION) {
            let (name, union_decl) = self.parse_union_decl()?;

            return Ok(Some((name, ItemKind::Union(union_decl))));
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

        let return_ty = if self.consume(TokenKind::RightArrow) {
            Some(self.parse_ident()?)
        } else {
            None
        };

        self.consume(TokenKind::OpenBrace);

        let mut body = ThinVec::new();

        while let Some(stmt) = self.parse_stmt()? {
            body.push(stmt);
        }

        self.consume(TokenKind::CloseBrace);

        Ok((
            ident,
            Fn {
                params,
                return_ty,
                body,
            },
        ))
    }

    fn parse_struct_decl(&mut self) -> ParseResult<(Ident, StructDecl)> {
        trace!("parse_struct_decl");

        let ident = self.parse_ident()?;

        self.consume(TokenKind::OpenBrace);

        let mut fields = ThinVec::new();

        if !self.check(TokenKind::CloseBrace) {
            loop {
                let field_name = self.parse_ident()?;

                self.consume(TokenKind::Colon);

                let ty_annotation = self.parse_ident()?;

                let span = field_name.span;

                fields.push(FieldDecl {
                    name: Some(field_name),
                    ty: ty_annotation,
                    span,
                });

                self.consume(TokenKind::Comma);

                if self.check(TokenKind::CloseBrace) {
                    break;
                }
            }
        }

        self.consume(TokenKind::CloseBrace);

        Ok((ident, StructDecl(VariantData::Struct(fields))))
    }

    fn parse_union_decl(&mut self) -> ParseResult<(Ident, UnionDecl)> {
        trace!("parse_union_decl");

        let ident = self.parse_ident()?;

        self.consume(TokenKind::OpenBrace);

        let mut variants = ThinVec::new();

        if !self.check(TokenKind::CloseBrace) {
            loop {
                let variant_name = self.parse_ident()?;

                let span = variant_name.span;

                variants.push(Variant {
                    name: variant_name,
                    data: VariantData::Unit,
                    span,
                });

                self.consume(TokenKind::Comma);

                if self.check(TokenKind::CloseBrace) {
                    break;
                }
            }
        }

        self.consume(TokenKind::CloseBrace);

        Ok((ident, UnionDecl { variants }))
    }
}
