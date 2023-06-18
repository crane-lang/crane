use serde::{Deserialize, Serialize};

/// A dummy [`Span`].
///
/// All of the positions are set to `0`.
pub const DUMMY_SPAN: Span = Span { start: 0, end: 0 };

/// A span.
#[derive(PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct Span {
    /// The start offset of the span (inclusive).
    pub start: usize,

    /// The end offset of the span (exclusive).
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Returns a new [`Span`] that encloses `self and `end`.
    pub fn to(self, end: Span) -> Self {
        Self::new(
            std::cmp::min(self.start, end.start),
            std::cmp::max(self.end, end.end),
        )
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(value: Span) -> Self {
        value.start..value.end
    }
}

impl ariadne::Span for Span {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_to_works_forwards() {
        let start = Span::new(0, 3);
        let end = Span::new(4, 6);

        assert_eq!(start.to(end), Span::new(0, 6))
    }

    #[test]
    fn span_to_works_backwards() {
        let start = Span::new(3, 7);
        let end = Span::new(0, 2);

        assert_eq!(start.to(end), Span::new(0, 7))
    }
}
