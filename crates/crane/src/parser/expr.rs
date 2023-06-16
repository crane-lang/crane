use thin_vec::ThinVec;
use tracing::trace;

use crate::ast::{Expr, ExprKind, Literal, LiteralKind, DUMMY_SPAN};
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::LexError;
use crate::parser::{ParseResult, Parser};

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    pub fn parse_expr(&mut self) -> ParseResult<Option<Expr>> {
        trace!("parse_expr");

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
                let callee = Expr {
                    kind: ExprKind::Variable { name: ident },
                    // TODO: This should be the `ident`'s span.
                    span: DUMMY_SPAN,
                };

                let args = self.parse_call_expr()?;

                return Ok(Some(Expr {
                    kind: ExprKind::Call {
                        fun: Box::new(callee),
                        args: args.into_iter().map(Box::new).collect(),
                    },
                    // TODO: What should this span be? The callee?
                    span: DUMMY_SPAN,
                }));
            } else {
                return Ok(Some(Expr {
                    kind: ExprKind::Variable { name: ident },
                    // TODO: This should be the `ident`'s span.
                    span: DUMMY_SPAN,
                }));
            }
        }

        Ok(None)
    }

    fn parse_call_expr(&mut self) -> ParseResult<ThinVec<Expr>> {
        trace!("parse_call_expr");

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

// fn parse_call_expr(&mut self, callee: Option<Token>) -> ParseResult<Expr> {
//     trace!("Parsing call expression");

//     let callee = if let Some(callee) = callee {
//         callee
//     } else {
//         self.consume(TokenKind::Ident, "Expected a function name.")?
//     };

//     self.consume(TokenKind::OpenParen, "Expected '('.")?;

//     let mut args = ThinVec::new();

//     if !self.check(TokenKind::CloseParen)? {
//         loop {
//             args.push(self.parse_expr()?);

//             if !self.check_and_consume(TokenKind::Comma)? {
//                 break;
//             }
//         }
//     }

//     self.consume(TokenKind::CloseParen, "Expected ')'.")?;

//     let callee = Expr {
//         kind: ExprKind::Variable {
//             name: Ident(callee.lexeme),
//         },
//         span: callee.span,
//     };

//     let span = callee.span.clone();

//     Ok(Expr {
//         kind: ExprKind::Call {
//             fun: Box::new(callee),
//             args: args.into_iter().map(Box::new).collect(),
//         },
//         span,
//     })
// }
