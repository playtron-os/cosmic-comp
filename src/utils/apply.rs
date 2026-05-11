//! Utility trait for method-chaining in the style of `cosmic::Apply`.
//!
//! Allows `value.apply(constructor)` as sugar for `constructor(value)`.

/// Extension trait for inline method-chaining (replaces `cosmic::Apply`).
pub trait Apply: Sized {
    fn apply<F, R>(self, f: F) -> R
    where
        F: FnOnce(Self) -> R,
    {
        f(self)
    }
}

// Blanket impl for everything.
impl<T: Sized> Apply for T {}
