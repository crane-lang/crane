use thin_vec::ThinVec;

use crate::ast::{Expr, ExprKind, Literal, LiteralKind, Path, StructExpr, StructExprField};
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
            let path = self.parse_path()?;

            if self.check_without_expect(TokenKind::OpenParen) {
                let span = path.span;

                let callee = Expr {
                    kind: ExprKind::Variable(path),
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
            }

            if self.check_without_expect(TokenKind::OpenBrace) {
                let struct_expr = self.parse_struct_expr(&path)?;

                let span = path.span.to(self.prev_token.span);

                return Ok(Some(Expr {
                    kind: ExprKind::Struct(Box::new(struct_expr)),
                    span,
                }));
            }

            let span = path.span;

            return Ok(Some(Expr {
                kind: ExprKind::Variable(path),
                span,
            }));
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

                // TODO: Should this be a `CloseParen`?
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

    #[tracing::instrument(skip(self))]
    fn parse_struct_expr(&mut self, path: &Path) -> ParseResult<StructExpr> {
        self.consume(TokenKind::OpenBrace);

        let mut fields = ThinVec::new();

        if !self.check(TokenKind::CloseBrace) {
            loop {
                let field_name = self.parse_ident()?;

                self.consume(TokenKind::Colon);

                let expr = self.parse_expr()?.unwrap();

                let span = field_name.span.to(expr.span);

                fields.push(StructExprField {
                    name: field_name,
                    expr: Box::new(expr),
                    span,
                });

                if !self.consume(TokenKind::Comma) {
                    break;
                }

                if self.check_without_expect(TokenKind::CloseBrace) {
                    break;
                }
            }
        }

        self.consume(TokenKind::CloseBrace);

        Ok(StructExpr {
            path: path.clone(),
            fields,
        })
    }
}
