//! # Enum Flags
//! `enumflags2` defines a `BitFlags<T>` type, which is a `Set<T>`
//! for enums without associated data.
//!
//! ## Example
//! ```
//! use enumflags2::BitFlags;
//!
//! #[derive(BitFlags, Copy, Clone, Debug, PartialEq)]
//! #[repr(u8)]
//! enum Test {
//!     A = 0b0001,
//!     B = 0b0010,
//!     C = 0b0100,
//!     D = 0b1000,
//! }
//!
//! let a_b = Test::A | Test::B; // BitFlags<Test>
//! let a_c = Test::A | Test::C;
//! let b_c_d = Test::C | Test::B | Test::D;
//!
//! // BitFlags<Test>(0b11, [A, B])
//! println!("{:?}", a_b);
//!
//! // BitFlags<Test>(0b1, [A])
//! println!("{:?}", a_b & a_c);
//!
//! // Iterate over the flags like a normal set!
//! assert_eq!(a_b.iter().collect::<Vec<_>>(), &[Test::A, Test::B]);
//!
//! assert!(a_b.contains(Test::A));
//! assert!(b_c_d.contains(Test::B | Test::C));
//! assert!(!(b_c_d.contains(a_b)));
//!
//! assert!(a_b.intersects(a_c));
//! assert!(!(a_b.intersects(Test::C | Test::D)));
//! ```
//!
//! ## Note
//!
//! By default, the `BitFlags` are `usize`-sized. If you want them to be smaller,
//! specify a `repr` on your enum as in the example above.
//!
//! ## Optional Feature Flags
//!
//! - [`serde`](https://serde.rs/) implements `Serialize` and `Deserialize`
//!   for `BitFlags<T>`.
//! - `std` implements `std::error::Error` for `FromBitsError`.
//! - `not_literal` enables a workaround that allows using discriminant
//!   expressions that can't be evaluated at macro expansion time. Notably,
//!   this includes using pre-existing constants.
//!
//!   This is disabled by default because of the high potential for confusing
//!   error messages - if a flag doesn't have exactly one bit set, the error
//!   message will be "attempt to subtract with overflow", pointing at the
//!   relevant flag.
//!
//! ### Migrating from 0.5
//!
//! The minimum rustc version has been bumped to 1.34.0, because of `syn 1.0`. The
//! version policy from now on will be "what's available on Debian stable", [because
//! Debian is famously slow with new software versions][debian-snailpace].
//!
//! You should no longer depend on `enumflags2_derive` directly.
//! Use the reexport from the `enumflags2` crate.
//! semver guarantees will be violated if you depend on the derive crate directly.
//!
//! The derive macro has been renamed to `BitFlags`, to make it clearer what the
//! derive does.
//!
//! The `nostd` feature flag has been removed. The crate now only depends on `libcore`
//! by default. Enable the `std` flag to get an implementation of `std::error::Error`
//! on error types.
//!
//! Flags more than one bit set have been found to have inconsistent semantics.
//! They are now rejected at compile-time. The same applies to flags without any
//! bit set. If you were relying on this in your code, please [open an issue][issue]
//! and explain your usecase.
//!
//! `BitFlags::from_bits` returns a `Result` instead of an `Option`. This might
//! necessitate some minor changes in your code.
//!
//! `BitFlags::not` has been removed. Use the `!` operator instead.
//!
//! [debian-snailpace]: https://www.jwz.org/blog/2016/04/i-would-like-debian-to-stop-shipping-xscreensaver/
//! [issue]: https://github.com/NieDzejkob/enumflags2/issues/new
//#![warn(missing_docs)]
#![cfg_attr(all(not(test), not(feature = "std")), no_std)]

use core::iter::FromIterator;
use core::ops::{Not,
    BitAnd, BitOr, BitXor,
    BitAndAssign, BitOrAssign, BitXorAssign,
};
use core::cmp;
use repr::{BitFlagRepr, BitFlagNum};

#[allow(unused_imports)]
#[macro_use]
extern crate enumflags2_derive;

#[doc(hidden)]
pub use enumflags2_derive::BitFlags_internal as BitFlags;

pub mod repr;

/// While the module is public, this is only the case because it needs to be
/// accessed by the derive macro. Do not use this directly. Stability guarantees
/// don't apply.
#[doc(hidden)]
pub mod _internal {
    // Re-export libcore so the macro doesn't inject "extern crate" downstream.
    pub mod core {
        pub use core::{convert, mem, option, ops, result};
    }
}

// Internal debug formatting implementations
mod formatting;

mod fallible;
pub use crate::fallible::FromBitsError;

mod ext;
pub use ext::*;

/// A single flag in a type-safe set of flags.
///
/// Automatically implemented by `derive(EnumFlags)` to make the enum
/// a valid type parameter for `BitFlags<T>`.
pub trait BitFlag
    : Copy + Clone
    + BitFlagRepr<Self>
    + BitOr<Self, Output = <Self as BitFlag>::Flags>
    + BitAnd<Self, Output = <Self as BitFlag>::Flags>
    + BitXor<Self, Output = <Self as BitFlag>::Flags>
    + Not<Output = <Self as BitFlag>::Flags>
    + 'static
{
    /// The underlying integer type.
    type Type: BitFlagNum;

    /// A type-safe set of flags.
    type Flags: EnumFlags<Flag=Self> + BitFlagRepr<Self>;

    /// Return a value with all flag bits set.
    const ALL_BITS: Self::Type;

    /// Return a slice that contains each variant exactly one.
    fn flag_list() -> &'static [Self];

    #[inline]
    /// Return the name of the type for debug formatting purposes.
    ///
    /// This is typically `BitFlags<EnumName>`
    fn bitflags_type_name() -> &'static str {
        "BitFlags"
    }
}

/// A set of bit flags.
///
/// This trait is unsafe to impl because it assumes that the underlying `BitFlagRepr` is
/// capable of representing a set of flags (and any combination rather than a single enum flag).
pub unsafe trait EnumFlags
    : Sized
    + BitFlagRepr<<Self as EnumFlags>::Flag>
{
    type Flag: BitFlag;

    #[inline]
    /// Create a new BitFlags unsafely. Consider using `from_bits` or `from_bits_truncate`.
    unsafe fn new(val: <Self::Flag as BitFlag>::Type) -> Self {
        <Self as BitFlagRepr<Self::Flag>>::from_repr_unchecked(val)
    }

    #[inline]
    /// Create an empty BitFlags. Empty means `0`.
    fn empty() -> Self {
        unsafe { Self::new(<Self::Flag as BitFlag>::Type::EMPTY) }
    }

    #[inline]
    /// Create a BitFlags with all flags set.
    fn all() -> Self {
        unsafe { Self::new(Self::Flag::ALL_BITS) }
    }

    #[inline]
    /// Returns true if all flags are set
    fn is_all(self) -> bool {
        self.bits() == <Self::Flag as BitFlag>::ALL_BITS
    }

    #[inline]
    /// Returns true if no flag is set
    fn is_empty(self) -> bool {
        self.bits() == <Self::Flag as BitFlag>::Type::EMPTY
    }

    #[inline]
    /// Returns the underlying type value
    fn bits(self) -> <Self::Flag as BitFlag>::Type {
        self.into_repr()
    }

    #[inline]
    /// Returns a BitFlags iff the bits value does not contain any illegal flags.
    fn from_bits(bits: <Self::Flag as BitFlag>::Type) -> Result<Self, FromBitsError<Self::Flag>> {
        Self::from_repr(bits)
    }

    #[inline]
    /// Truncates flags that are illegal
    fn from_bits_truncate(bits: <Self::Flag as BitFlag>::Type) -> Self {
        unsafe { Self::new(bits & Self::Flag::ALL_BITS) }
    }

    #[inline]
    fn from_flag(flag: Self::Flag) -> Self {
        unsafe { Self::new(flag.into_bits()) }
    }

    #[inline]
    fn from_flags<B: BitFlagRepr<Self::Flag>>(flags: B) -> Self {
        unsafe { Self::new(flags.into_repr()) }
    }

    /// Returns true if at least one flag is shared.
    fn intersects<B: BitFlagRepr<Self::Flag>>(self, other: B) -> bool {
        (self.bits() & other.into_repr()) != <Self::Flag as BitFlag>::Type::EMPTY
    }

    #[inline]
    /// Returns true iff all flags are contained.
    fn contains<B: BitFlagRepr<Self::Flag>>(self, other: B) -> bool {
        self.contains_repr(other.into_repr())
    }

    #[inline]
    fn bits_or<B: BitFlagRepr<Self::Flag>>(self, other: B) -> Self {
        unsafe {
            Self::from_repr_unchecked(self.into_repr() | other.into_repr())
        }
    }

    #[inline]
    fn bits_and<B: BitFlagRepr<Self::Flag>>(self, other: B) -> Self {
        unsafe {
            Self::from_repr_unchecked(self.into_repr() & other.into_repr())
        }
    }

    #[inline]
    fn bits_xor<B: BitFlagRepr<Self::Flag>>(self, other: B) -> Self {
        unsafe {
            Self::from_repr_unchecked(self.into_repr() ^ other.into_repr())
        }
    }

    #[inline]
    fn bits_not(self) -> Self {
        unsafe {
            Self::from_repr_unchecked((!self.into_repr()) & Self::Flag::ALL_BITS)
        }
    }

    #[inline]
    /// Toggles the matching bits
    fn toggle<B: BitFlagRepr<Self::Flag>>(&mut self, other: B) {
        unsafe {
            self.set_repr_unchecked(self.get_repr() ^ other.into_repr())
        }
    }

    #[inline]
    /// Inserts the flags into the BitFlag
    fn insert<B: BitFlagRepr<Self::Flag>>(&mut self, other: B) {
        unsafe {
            self.set_repr_unchecked(self.get_repr() | other.into_repr())
        }
    }

    #[inline]
    fn mask<B: BitFlagRepr<Self::Flag>>(&mut self, other: B) {
        unsafe {
            self.set_repr_unchecked(self.get_repr() & other.into_repr())
        }
    }

    #[inline]
    /// Removes the matching flags
    fn remove<B: BitFlagRepr<Self::Flag>>(&mut self, other: B) {
        unsafe {
            self.set_repr_unchecked(self.get_repr() & !other.into_repr())
        }
    }

    #[inline]
    /// Returns an iterator that yields each set flag
    fn iter(self) -> Self::IntoIter
    where
        Self: IntoIterator<Item=<Self as EnumFlags>::Flag>,
    {
        self.into_iter()
    }
}

pub trait EnumFlagsConst
    : EnumFlags
{
    const ALL: Self;

    const EMPTY: Self;
}

/// Represents a set of flags of some type `T`.
/// The type must have the `#[derive(BitFlags)]` attribute applied.
#[derive(Copy, Clone, Eq, Hash)]
#[repr(transparent)]
pub struct BitFlags<T: BitFlag> {
    val: T::Type,
}

impl<T: BitFlag> EnumFlagsConst for BitFlags<T> {
    const ALL: Self = Self { val: T::ALL_BITS };
    const EMPTY: Self = Self { val: T::Type::EMPTY };
}

unsafe impl<T: BitFlag> BitFlagRepr<T> for BitFlags<T> {
    #[inline]
    fn into_repr(self) -> T::Type {
        self.val
    }

    #[inline]
    fn get_repr(&self) -> T::Type {
        self.val
    }

    #[inline]
    unsafe fn from_repr_unchecked(val: T::Type) -> Self {
        Self { val }
    }

    unsafe fn set_repr_unchecked(&mut self, val: T::Type) {
        self.val = val;
    }

    fn from_repr(repr: T::Type) -> Result<Self, FromBitsError<T>> {
        let invalid = repr & !T::ALL_BITS;
        if invalid == T::Type::EMPTY {
            unsafe { Ok(Self::from_repr_unchecked(repr)) }
        } else {
            Err(FromBitsError {
                flags: Self::from_bits_truncate(repr),
                invalid,
            })
        }
    }
}

/// The default value returned is one with all flags unset, i. e. [`empty`][Self::empty].
impl<T> Default for BitFlags<T>
where
    T: BitFlag,
{
    fn default() -> Self {
        Self::EMPTY
    }
}

impl<T: BitFlag> From<T> for BitFlags<T> {
    #[inline]
    fn from(t: T) -> BitFlags<T> {
        Self::from_flag(t)
    }
}

unsafe impl<T> EnumFlags for BitFlags<T>
where
    T: BitFlag,
{
    type Flag = T;
}

pub struct BitFlagsIter<T: BitFlag> {
    flags: BitFlags<T>,
    iter: core::slice::Iter<'static, T>,
}

impl<T> Iterator for BitFlagsIter<T>
where
    T: BitFlag,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&flag) = self.iter.next() {
            if self.flags.contains(flag) {
                return Some(flag)
            }
        }

        None
    }
}

impl<T> IntoIterator for BitFlags<T>
where
    T: BitFlag,
{
    type Item = T;
    type IntoIter = BitFlagsIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        BitFlagsIter {
            flags: self,
            iter: T::flag_list().iter(),
        }
    }
}

impl<T, B> cmp::PartialEq<B> for BitFlags<T>
where
    T: BitFlag,
    B: BitFlagRepr<T>,
{
    #[inline]
    fn eq(&self, other: &B) -> bool {
        self.get_repr() == other.get_repr()
    }
}

impl<T, B> BitOr<B> for BitFlags<T>
where
    T: BitFlag,
    B: BitFlagRepr<T>,
{
    type Output = BitFlags<T>;

    #[inline]
    fn bitor(self, other: B) -> BitFlags<T> {
        self.bits_or(other)
    }
}

impl<T, B> BitAnd<B> for BitFlags<T>
where
    T: BitFlag,
    B: BitFlagRepr<T>,
{
    type Output = BitFlags<T>;

    #[inline]
    fn bitand(self, other: B) -> BitFlags<T> {
        self.bits_and(other)
    }
}

impl<T, B> BitXor<B> for BitFlags<T>
where
    T: BitFlag,
    B: BitFlagRepr<T>,
{
    type Output = BitFlags<T>;

    #[inline]
    fn bitxor(self, other: B) -> BitFlags<T> {
        self.bits_xor(other)
    }
}

impl<T, B> BitOrAssign<B> for BitFlags<T>
where
    T: BitFlag,
    B: BitFlagRepr<T>,
{
    #[inline]
    fn bitor_assign(&mut self, other: B) {
        self.insert(other)
    }
}

impl<T, B> BitAndAssign<B> for BitFlags<T>
where
    T: BitFlag,
    B: BitFlagRepr<T>,
{
    #[inline]
    fn bitand_assign(&mut self, other: B) {
        self.mask(other)
    }
}
impl<T, B> BitXorAssign<B> for BitFlags<T>
where
    T: BitFlag,
    B: BitFlagRepr<T>,
{
    #[inline]
    fn bitxor_assign(&mut self, other: B) {
        self.toggle(other)
    }
}

impl<T> Not for BitFlags<T>
where
    T: BitFlag,
{
    type Output = BitFlags<T>;

    #[inline]
    fn not(self) -> BitFlags<T> {
        self.bits_not()
    }
}

impl<T, B> FromIterator<B> for BitFlags<T>
where
    T: BitFlag,
    B: BitFlagRepr<T>
{
    fn from_iter<I>(it: I) -> BitFlags<T>
    where
        I: IntoIterator<Item = B>
    {
        it.into_iter().fold(BitFlags::empty(), |acc, flag| acc.bits_or(flag))
    }
}

impl<T, B> Extend<B> for BitFlags<T>
where
    T: BitFlag,
    B: Into<BitFlags<T>>
{
    fn extend<I>(&mut self, it: I)
    where
        I: IntoIterator<Item = B>
    {
        *self = it.into_iter().fold(*self, |acc, flag| acc.bits_or(flag.into()))
    }
}

#[cfg(feature = "serde")]
mod impl_serde {
    use serde::{Serialize, Deserialize};
    use serde::de::{Error, Unexpected};
    use super::{BitFlags, BitFlag, EnumFlags};

    impl<'a, T> Deserialize<'a> for BitFlags<T>
    where
        T: BitFlag,
        T::Type: Deserialize<'a> + Into<u64>,
    {
        fn deserialize<D: serde::Deserializer<'a>>(d: D) -> Result<Self, D::Error> {
            let val = T::Type::deserialize(d)?;
            Self::from_bits(val)
                .map_err(|_| D::Error::invalid_value(
                    Unexpected::Unsigned(val.into()),
                    &"valid bit representation"
                ))
        }
    }

    impl<T> Serialize for BitFlags<T>
    where
        T: BitFlag,
        T::Type: Serialize,
    {
        fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
            T::Type::serialize(&self.val, s)
        }
    }
}
