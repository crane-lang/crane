use std::ops::Range;

use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
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

    /// An identifier.
    #[regex(r"[A-Za-z_]+")]
    Ident,

    /// A string literal.
    #[regex(r#""[^"]*""#)]
    String,

    /// A comment.
    #[regex(r"//.*")]
    Comment,

    /// Any sequence of whitespace characters.
    #[regex(r"[ \n]+", logos::skip)]
    Whitespace,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub span: Range<usize>,
}
