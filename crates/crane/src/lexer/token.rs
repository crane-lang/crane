use std::ops::Range;

use logos::Logos;
use smol_str::SmolStr;

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
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: SmolStr,
    pub span: Range<usize>,
}
