mod error;
mod expr;
mod item;

pub use error::*;

use std::iter::Peekable;

use thin_vec::ThinVec;
use tracing::trace;

use crate::ast::{
    Expr, ExprKind, Fn, FnParam, Ident, Item, ItemKind, Literal, LiteralKind, Stmt, StmtKind,
    DUMMY_SPAN,
};
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::{LexError, Lexer};

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<TokenStream: Iterator<Item = Result<Token, LexError>>> {
    /// The list of tokens.
    tokens: TokenStream,

    /// The list of lexing errors uncovered during parsing.
    lex_errors: Vec<LexError>,

    /// The current token.
    token: Token,

    /// The previous token.
    prev_token: Token,
}

impl<TokenStream> Parser<TokenStream>
where
    TokenStream: Iterator<Item = Result<Token, LexError>>,
{
    pub fn new(input: TokenStream) -> Self {
        let mut parser = Self {
            tokens: input,
            lex_errors: Vec::new(),
            token: Token::dummy(),
            prev_token: Token::dummy(),
        };

        // Advance the parser to the first token.
        parser.advance();

        parser
    }

    pub fn parse(mut self) -> ParseResult<Vec<Item>> {
        trace!("Parsing program");

        let mut items = Vec::new();

        while !self.is_at_end() {
            let item = self.parse_item()?;

            items.push(item);
        }

        Ok(items)
    }

    // fn peek(&mut self) -> ParseResult<Option<&Token>> {
    //     if let Some(token) = self.lexer.peek() {
    //         let next_token_kind = token
    //             .as_ref()
    //             .map(|token| token.kind)
    //             .map_err(|err| err.clone())?;

    //         match next_token_kind {
    //             TokenKind::Comment => {
    //                 self.advance()?;

    //                 return self.peek();
    //             }
    //             _ => {}
    //         }
    //     }

    //     if let Some(token) = self.lexer.peek() {
    //         let token = token.as_ref().map_err(|err| err.clone())?;

    //         Ok(Some(token))
    //     } else {
    //         Ok(None)
    //     }
    // }

    /// Returns whether the parser is at the end of the token stream.
    fn is_at_end(&mut self) -> bool {
        self.token.kind == TokenKind::Eof
    }

    /// Advances the parser to the next token.
    fn advance(&mut self) {
        let next_token: Option<Token>;

        loop {
            match self.tokens.next() {
                // Ignore any comment tokens.
                Some(Ok(Token {
                    kind: TokenKind::Comment,
                    ..
                })) => {
                    continue;
                }

                // Capture any lexing errors, as we come across them.
                Some(Err(lex_error)) => {
                    next_token = None;

                    self.lex_errors.push(lex_error);

                    break;
                }
                Some(Ok(token)) => {
                    next_token = Some(token);

                    break;
                }
                None => {
                    next_token = None;

                    break;
                }
            };
        }

        let next_token = next_token.unwrap_or(Token {
            kind: TokenKind::Eof,
            lexeme: "".into(),
            span: DUMMY_SPAN,
        });

        self.prev_token = std::mem::replace(&mut self.token, next_token);
    }

    /// Returns whether the next token is of the given [`TokenKind`].
    fn check(&mut self, kind: TokenKind) -> bool {
        self.token.kind == kind
    }

    /// Consumes the next token if it is of the given [`TokenKind`].
    ///
    /// Returns whether the token was present.
    pub fn consume(&mut self, kind: TokenKind) -> bool {
        let is_present = self.check(kind);
        if is_present {
            self.advance();
        }

        is_present
    }

    // pub fn consume_keyword(&mut self, keyword: Ident)

    // fn check(&mut self, kind: TokenKind) -> ParseResult<bool> {
    //     Ok(self
    //         .peek()?
    //         .map(|token| token.kind == kind)
    //         .unwrap_or(false))
    // }

    // fn check_and_consume(&mut self, kind: TokenKind) -> ParseResult<bool> {
    //     if self.check(kind)? {
    //         self.advance()?;

    //         Ok(true)
    //     } else {
    //         Ok(false)
    //     }
    // }

    // fn consume(&mut self, kind: TokenKind, message: &str) -> ParseResult<Token> {
    //     if self.check(kind)? {
    //         self.advance()
    //     } else {
    //         Err(ParseError {
    //             kind: ParseErrorKind::Error(message.to_string()),
    //             span: self
    //                 .peek()
    //                 .map(|token| {
    //                     token
    //                         .map(|token| token.span.clone())
    //                         .unwrap_or((0..0).into())
    //                 })
    //                 .unwrap_or((0..0).into()),
    //         })
    //     }
    // }

    // fn parse_fn_item(&mut self) -> ParseResult<Item> {
    //     trace!("Parsing function declaration");

    //     match self.peek()? {
    //         Some(peeked) if peeked.kind == TokenKind::Ident && peeked.lexeme == "pub" => {
    //             self.advance()?;
    //         }
    //         _ => {}
    //     }

    //     let fn_keyword = self.consume(TokenKind::Ident, "Expected 'fn'.")?;
    //     if fn_keyword.lexeme != "fn" {
    //         return Err(ParseError {
    //             kind: ParseErrorKind::Error("Expected 'fn'.".to_string()),
    //             span: fn_keyword.span,
    //         });
    //     }

    //     let name = self.consume(TokenKind::Ident, "Expected a function name.")?;

    //     self.consume(TokenKind::OpenParen, "Expected '('.")?;

    //     let mut params = ThinVec::new();

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

    // fn parse_expr(&mut self) -> ParseResult<Expr> {
    //     trace!("Parsing expression");

    //     if self.check(TokenKind::String)? {
    //         let token = self.advance()?;

    //         return Ok(Expr {
    //             kind: ExprKind::Literal(Literal {
    //                 kind: LiteralKind::String,
    //                 value: token.lexeme,
    //             }),
    //             span: token.span,
    //         });
    //     }

    //     if self.check(TokenKind::Integer)? {
    //         let token = self.advance()?;

    //         return Ok(Expr {
    //             kind: ExprKind::Literal(Literal {
    //                 kind: LiteralKind::Integer,
    //                 value: token.lexeme,
    //             }),
    //             span: token.span,
    //         });
    //     }

    //     if self.check(TokenKind::Ident)? {
    //         let token = self.advance()?;

    //         if self.check(TokenKind::OpenParen)? {
    //             return self.parse_call_expr(Some(token));
    //         } else {
    //             return Ok(Expr {
    //                 kind: ExprKind::Variable {
    //                     name: Ident(token.lexeme.into()),
    //                 },
    //                 span: token.span,
    //             });
    //         }
    //     }

    //     todo!()
    // }

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
}
