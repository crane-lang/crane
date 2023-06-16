use smol_str::SmolStr;

// TODO: Add span to `Ident`.
/// An identifier.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident(pub SmolStr);

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
