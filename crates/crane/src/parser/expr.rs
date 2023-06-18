use thin_vec::ThinVec;


use crate::ast::{Expr, ExprKind, Literal, LiteralKind};
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::LexError;
use crate::parser::{ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    #[tracing::instrument(skip(self))]
    pub fn parse_expr(&mut self) -> ParseResult<Option<Expr>> {
        if self.check(TokenKind::String) {
            let string_literal = Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::String,
                    value: self.token.lexeme.clone(),
                }),
                span: self.token.span,
            };

            self.advance();

            return Ok(Some(string_literal));
        }

        if self.check(TokenKind::Integer) {
            let int_literal = Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Integer,
                    value: self.token.lexeme.clone(),
                }),
                span: self.token.span,
            };

            self.advance();

            return Ok(Some(int_literal));
        }

        if self.check_without_expect(TokenKind::Ident) {
            let ident = self.parse_ident()?;

            if self.check_without_expect(TokenKind::OpenParen) {
                let span = ident.span;

                let callee = Expr {
                    kind: ExprKind::Variable { name: ident },
                    span,
                };

                let args = self.parse_call_expr()?;

                return Ok(Some(Expr {
                    kind: ExprKind::Call {
                        fun: Box::new(callee),
                        args: args.into_iter().map(Box::new).collect(),
                    },
                    span,
                }));
            } else {
                let span = ident.span;

                return Ok(Some(Expr {
                    kind: ExprKind::Variable { name: ident },
                    span,
                }));
            }
        }

        Ok(None)
    }

    #[tracing::instrument(skip(self))]
    fn parse_call_expr(&mut self) -> ParseResult<ThinVec<Expr>> {
        self.consume(TokenKind::OpenParen);

        let mut args = ThinVec::new();

        if !self.check_without_expect(TokenKind::CloseParen) {
            loop {
                if let Some(param) = self.parse_expr()? {
                    args.push(param);
                }

                if self.check_without_expect(TokenKind::CloseBrace) {
                    break;
                }

                if !self.consume(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::CloseParen);

        Ok(args)
    }
}
