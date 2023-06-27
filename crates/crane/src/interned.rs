use std::cmp::Ordering;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::ptr;

/// This private module is used to prevent accessing the [`InternedMarker`]
/// from outside of the [`interned`] module.
///
/// This forces callers to go through [`Interned::new_unchecked`] in order to
/// construct an [`Interned`] value.
///
/// [`interned`]: crate::interned
mod private {
    /// A marker struct used to indicate that a value is interned.
    #[derive(Debug, Clone, Copy)]
    pub struct InternedMarker;
}

/// An interned value.
pub struct Interned<'a, T>(pub &'a T, pub private::InternedMarker);

impl<'a, T> Interned<'a, T> {
    /// Returns a new [`Interned`] value.
    ///
    /// When calling this function you must ensure that the provided value is
    /// both interned and unique.
    #[inline]
    pub const fn new_unchecked(t: &'a T) -> Self {
        Interned(t, private::InternedMarker)
    }
}

impl<'a, T> Deref for Interned<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        self.0
    }
}

impl<T: Debug> Debug for Interned<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a, T> PartialEq for Interned<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'a, T: PartialOrd> PartialOrd for Interned<'a, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if ptr::eq(self.0, other.0) {
            Some(Ordering::Equal)
        } else {
            let ordering = self.0.partial_cmp(other.0);
            debug_assert_ne!(ordering, Some(Ordering::Equal));
            ordering
        }
    }
}

impl<'a, T: Ord> Ord for Interned<'a, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        if ptr::eq(self.0, other.0) {
            Ordering::Equal
        } else {
            let ordering = self.0.cmp(other.0);
            debug_assert_ne!(ordering, Ordering::Equal);
            ordering
        }
    }
}

impl<'a, T> Eq for Interned<'a, T> {}

impl<'a, T> Hash for Interned<'a, T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.0, state);
    }
}

impl<'a, T> Clone for Interned<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for Interned<'a, T> {}
