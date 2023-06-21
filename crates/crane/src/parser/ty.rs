use thin_vec::ThinVec;

use crate::ast::{FnDecl, FnParam, FnReturnTy, FnTy, Ident, Ty, TyKind, DUMMY_SPAN};
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::LexError;
use crate::parser::{ParseError, ParseErrorKind, ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    /// Parses a [`Ty`].
    #[tracing::instrument(skip(self))]
    pub fn parse_ty(&mut self) -> ParseResult<Ty> {
        if let Some(ident) = self.token.ident() {
            if ident.name == "Fn" {
                let fn_ty = self.parse_fn_ty()?;

                let span = ident.span.to(self.prev_token.span);

                return Ok(Ty {
                    kind: TyKind::Fn(Box::new(fn_ty)),
                    span,
                });
            }
        }

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

    #[tracing::instrument(skip(self))]
    fn parse_fn_ty(&mut self) -> ParseResult<FnTy> {
        self.parse_ident()?;

        self.consume(TokenKind::OpenParen);

        let mut params = ThinVec::new();

        if !self.check_without_expect(TokenKind::CloseParen) {
            loop {
                let ty = self.parse_ty()?;

                let span = ty.span;

                params.push(FnParam {
                    name: Ident {
                        name: "".into(),
                        span: DUMMY_SPAN,
                    },
                    ty: Box::new(ty),
                    span,
                });

                if !self.consume(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::CloseParen);

        let return_ty = if self.consume(TokenKind::RightArrow) {
            let ty = self.parse_ty()?;

            FnReturnTy::Ty(Box::new(ty))
        } else {
            FnReturnTy::Unit
        };

        Ok(FnTy {
            decl: Box::new(FnDecl { params, return_ty }),
        })
    }
}
