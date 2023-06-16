use logos::Logos;
use smol_str::SmolStr;

use crate::ast::{Span, DUMMY_SPAN};
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

    /// `:`
    #[token(":")]
    Colon,

    /// An identifier.
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
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

    /// The end of the file (EOF).
    #[end]
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: SmolStr,
    pub span: Span,
}

impl Token {
    /// A dummy [`Token`], to be thrown away later.
    pub fn dummy() -> Self {
        Self {
            kind: TokenKind::Comment,
            lexeme: SmolStr::default(),
            span: DUMMY_SPAN,
        }
    }
}
