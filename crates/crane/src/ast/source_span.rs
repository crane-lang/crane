use crate::ast::Span;

/// A [`Span`] with the source from where it originated.
pub struct SourceSpan {
    pub source: String,
    pub span: Span,
}

impl From<(&str, Span)> for SourceSpan {
    fn from(value: (&str, Span)) -> Self {
        Self {
            source: value.0.parse().unwrap(),
            span: value.1,
        }
    }
}

impl ariadne::Span for SourceSpan {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}
