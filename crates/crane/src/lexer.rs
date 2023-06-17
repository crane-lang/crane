mod error;

pub mod token;

pub use error::*;

use logos::Logos;

use crate::lexer::token::{Token, TokenKind};

pub struct Lexer<'src> {
    lexer: logos::Lexer<'src, TokenKind>,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            lexer: TokenKind::lexer(input),
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.lexer.next()?;
        let lexeme = self.lexer.slice();
        let span = self.lexer.span().into();

        Some(
            kind.map(|kind| Token {
                kind,
                lexeme: lexeme.into(),
                span,
            })
            .map_err(|kind| LexError { kind, span }),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> logos::Lexer<TokenKind> {
        TokenKind::lexer(input)
    }

    fn check(input: &str, token: TokenKind) {
        let mut lexer = TokenKind::lexer(input);

        assert_eq!(lexer.next(), Some(Ok(token)));
        assert_eq!(lexer.slice(), input);
    }

    #[test]
    fn lex_open_paren() {
        check("(", TokenKind::OpenParen)
    }

    #[test]
    fn lex_close_paren() {
        check(")", TokenKind::CloseParen)
    }

    #[test]
    fn lex_open_brace() {
        check("{", TokenKind::OpenBrace)
    }

    #[test]
    fn lex_close_brace() {
        check("}", TokenKind::CloseBrace)
    }

    #[test]
    fn lex_comment() {
        check("// This is a comment.", TokenKind::Comment)
    }

    #[test]
    fn lex_fn_declaration() {
        let mut lex = lex("fn foo() {}");

        assert_eq!(lex.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lex.span(), 0..2);
        assert_eq!(lex.slice(), "fn");

        assert_eq!(lex.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lex.span(), 3..6);
        assert_eq!(lex.slice(), "foo");

        assert_eq!(lex.next(), Some(Ok(TokenKind::OpenParen)));
        assert_eq!(lex.span(), 6..7);
        assert_eq!(lex.slice(), "(");

        assert_eq!(lex.next(), Some(Ok(TokenKind::CloseParen)));
        assert_eq!(lex.span(), 7..8);
        assert_eq!(lex.slice(), ")");

        assert_eq!(lex.next(), Some(Ok(TokenKind::OpenBrace)));
        assert_eq!(lex.span(), 9..10);
        assert_eq!(lex.slice(), "{");

        assert_eq!(lex.next(), Some(Ok(TokenKind::CloseBrace)));
        assert_eq!(lex.span(), 10..11);
        assert_eq!(lex.slice(), "}");
    }

    #[test]
    fn test_lexer() {
        insta::glob!("snapshot_inputs/*.crane", |path| {
            let source = std::fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&source);

            insta::assert_yaml_snapshot!(lexer.into_iter().collect::<Vec<_>>());
        })
    }
}
