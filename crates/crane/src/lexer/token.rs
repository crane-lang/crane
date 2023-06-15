use logos::Logos;
use smol_str::SmolStr;

use crate::ast::Span;
use crate::lexer::LexErrorKind;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(error = LexErrorKind)]
pub enum TokenKind {
    /// `(`
    #[token("(")]
    OpenParen,

    /// `)`
    #[token(")")]
    CloseParen,

    /// `{`
    #[token("{")]
    OpenBrace,

    /// `}`
    #[token("}")]
    CloseBrace,

    /// `,`
    #[token(",")]
    Comma,

    /// An identifier.
    #[regex(r"[A-Za-z_]+")]
    Ident,

    /// A string literal.
    #[regex(r#""[^"]*""#)]
    String,

    /// An integer literal.
    #[regex(r#"[\d]+"#)]
    Integer,

    /// A comment.
    #[regex(r"//.*")]
    Comment,

    /// Any sequence of whitespace characters.
    #[regex(r"[ \n]+", logos::skip)]
    Whitespace,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: SmolStr,
    pub span: Span,
}
