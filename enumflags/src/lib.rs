//! # Enum Flags
//! `enumflags2` defines a `BitFlags<T>` type for representing a set of named boolean values
//! efficiently, where `T` is an enum with explicitly defined values. Semantically similar to a
//! `HashSet<EnumWithoutAssociatedData>`, but much more efficient.
//!
//! ## Example
//! ```
//! extern crate enumflags2;
//! #[macro_use]
//! extern crate enumflags2_derive;
//!
//! use enumflags2::BitFlags;
//!
//! #[derive(EnumFlags, Copy, Clone, Debug, PartialEq)]
//! #[repr(u8)]
//! enum Test {
//!     A = 0b0001,
//!     B = 0b0010,
//!     C = 0b0100,
//!     D = 0b1000,
//! }
//!
//! fn main() {
//!     let a_b = Test::A | Test::B; // BitFlags<Test>
//!     let a_c = Test::A | Test::C;
//!     let b_c_d = Test::C | Test::B | Test::D;
//!
//!     // BitFlags<Test>(0b11, [A, B])
//!     println!("{:?}", a_b);
//!
//!     // BitFlags<Test>(0b1, [A])
//!     println!("{:?}", a_b & a_c);
//!
//!     // Iterate over the flags like a normal set!
//!     assert_eq!(a_b.iter().collect::<Vec<_>>(), &[Test::A, Test::B]);
//!
//!     assert!(a_b.contains(Test::A));
//!     assert!(b_c_d.contains(Test::B | Test::C));
//!     assert!(!(b_c_d.contains(a_b)));
//!
//!     assert!(a_b.intersects(a_c));
//!     assert!(!(a_b.intersects(Test::C | Test::D)));
//! }
//! ```
#![warn(missing_docs)]
#![no_std]
#[cfg(not(feature = "nostd"))]
extern crate std;
#[cfg(feature = "nostd")]
extern crate core as std;

use std::fmt::{self, Formatter};
use std::iter::FromIterator;

/// Sealed trait
mod details {
    use std::ops::{BitAnd, BitOr, BitXor, Not};
    use std::cmp::PartialOrd;

    pub trait BitFlagNum
        : Default
        + BitOr<Self, Output = Self>
        + BitAnd<Self, Output = Self>
        + BitXor<Self, Output = Self>
        + Not<Output = Self>
        + PartialOrd<Self>
        + Copy
        + Clone {
    }

    impl BitFlagNum for u8 {}
    impl BitFlagNum for u16 {}
    impl BitFlagNum for u32 {}
    impl BitFlagNum for u64 {}
    impl BitFlagNum for usize {}
}

use details::BitFlagNum;

/// A trait automatically implemented by `derive(EnumFlags)` on `T` to enable debug printing of
/// `BitFlags<T>`. This is necessary because the names of the variants are needed.
pub trait BitFlagsFmt
where
    Self: RawBitFlags,
{
    /// The implementation of Debug redirects here.
    fn fmt(flags: BitFlags<Self>, f: &mut Formatter) -> fmt::Result;
}

/// A trait automatically implemented by `derive(EnumFlags)` to make the enum a valid type parameter
/// for BitFlags.
pub trait RawBitFlags: Copy + Clone + 'static {
    /// The underlying integer type.
    type Type: BitFlagNum;

    /// Return a value with all flag bits set.
    fn all() -> Self::Type;

    /// Return the bits as a number type.
    fn bits(self) -> Self::Type;

    /// Return a slice that contains each variant exactly one.
    fn flag_list() -> &'static [Self];
}

/// Represents a set of flags of some type `T`.
#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct BitFlags<T: RawBitFlags> {
    val: T::Type,
}

impl<T> ::std::fmt::Debug for BitFlags<T>
where
    T: RawBitFlags + BitFlagsFmt,
{
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        T::fmt(self.clone(), fmt)
    }
}

/// The default value returned is one with all flags unset, i. e. [`empty`][Self::empty].
impl<T> Default for BitFlags<T>
where
    T: RawBitFlags,
{
    fn default() -> Self {
        Self::empty()
    }
}

impl<T> BitFlags<T>
where
    T: RawBitFlags,
{
    /// Create a new BitFlags unsafely. Consider using `from_bits` or `from_bits_truncate`.
    pub unsafe fn new(val: T::Type) -> Self {
        BitFlags { val }
    }
}

impl<T: RawBitFlags> From<T> for BitFlags<T> {
    fn from(t: T) -> BitFlags<T> {
        BitFlags { val: t.bits() }
    }
}

impl<T> BitFlags<T>
where
    T: RawBitFlags,
{
    /// Create an empty BitFlags. Empty means `0`.
    pub fn empty() -> Self {
        unsafe { BitFlags::new(T::Type::default()) }
    }

    /// Create a BitFlags with all flags set.
    pub fn all() -> Self {
        unsafe { BitFlags::new(T::all()) }
    }

    /// Returns true if all flags are set
    pub fn is_all(self) -> bool {
        self.val == T::all()
    }

    /// Returns true if no flag is set
    pub fn is_empty(self) -> bool {
        self.val == Self::empty().bits()
    }

    /// Returns the underlying type value
    pub fn bits(self) -> T::Type {
        self.val
    }

    /// Returns true if at least one flag is shared.
    pub fn intersects<B: Into<BitFlags<T>>>(self, other: B) -> bool {
        (self.bits() & other.into().bits()) > Self::empty().bits()
    }

    /// Returns true iff all flags are contained.
    pub fn contains<B: Into<BitFlags<T>>>(self, other: B) -> bool {
        let other = other.into();
        (self.bits() & other.bits()) == other.bits()
    }

    /// Flips all flags
    pub fn not(self) -> Self {
        unsafe { BitFlags::new(!self.bits() & T::all()) }
    }

    /// Returns a BitFlags iff the bits value does not contain any illegal flags.
    pub fn from_bits(bits: T::Type) -> Option<Self> {
        if bits & !Self::all().bits() == Self::empty().bits() {
            unsafe { Some(BitFlags::new(bits)) }
        } else {
            None
        }
    }

    /// Truncates flags that are illegal
    pub fn from_bits_truncate(bits: T::Type) -> Self {
        unsafe { BitFlags::new(bits & T::all()) }
    }

    /// Toggles the matching bits
    pub fn toggle<B: Into<BitFlags<T>>>(&mut self, other: B) {
        *self = *self ^ other.into();
    }

    /// Inserts the flags into the BitFlag
    pub fn insert<B: Into<BitFlags<T>>>(&mut self, other: B) {
        *self = *self | other.into();
    }

    /// Removes the matching flags
    pub fn remove<B: Into<BitFlags<T>>>(&mut self, other: B) {
        *self = *self & !other.into();
    }

    /// Returns an iterator that yields each set flag
    pub fn iter(self) -> impl Iterator<Item = T> {
        T::flag_list().iter().cloned().filter(move |&flag| self.contains(flag))
    }
}

impl<T, B> std::cmp::PartialEq<B> for BitFlags<T>
where
    T: RawBitFlags,
    B: Into<BitFlags<T>> + Copy,
{
    fn eq(&self, other: &B) -> bool {
        self.bits() == Into::<Self>::into(*other).bits()
    }
}

impl<T, B> std::ops::BitOr<B> for BitFlags<T>
where
    T: RawBitFlags,
    B: Into<BitFlags<T>>,
{
    type Output = BitFlags<T>;
    fn bitor(self, other: B) -> BitFlags<T> {
        unsafe { BitFlags::new(self.bits() | other.into().bits()) }
    }
}

impl<T, B> std::ops::BitAnd<B> for BitFlags<T>
where
    T: RawBitFlags,
    B: Into<BitFlags<T>>,
{
    type Output = BitFlags<T>;
    fn bitand(self, other: B) -> BitFlags<T> {
        unsafe { BitFlags::new(self.bits() & other.into().bits()) }
    }
}

impl<T, B> std::ops::BitXor<B> for BitFlags<T>
where
    T: RawBitFlags,
    B: Into<BitFlags<T>>,
{
    type Output = BitFlags<T>;
    fn bitxor(self, other: B) -> BitFlags<T> {
        unsafe { BitFlags::new((self.bits() ^ other.into().bits()) & T::all()) }
    }
}

impl<T, B> std::ops::BitOrAssign<B> for BitFlags<T>
where
    T: RawBitFlags,
    B: Into<BitFlags<T>>,
{
    fn bitor_assign(&mut self, other: B) {
        *self = *self | other;
    }
}

impl<T, B> std::ops::BitAndAssign<B> for BitFlags<T>
where
    T: RawBitFlags,
    B: Into<BitFlags<T>>,
{
    fn bitand_assign(&mut self, other: B) {
        *self = *self & other;
    }
}
impl<T, B> std::ops::BitXorAssign<B> for BitFlags<T>
where
    T: RawBitFlags,
    B: Into<BitFlags<T>>,
{
    fn bitxor_assign(&mut self, other: B) {
        *self = *self ^ other;
    }
}

impl<T> std::ops::Not for BitFlags<T>
where
    T: RawBitFlags,
{
    type Output = BitFlags<T>;
    fn not(self) -> BitFlags<T> {
        self.not()
    }
}

impl<T, B> FromIterator<B> for BitFlags<T>
where
    T: RawBitFlags,
    B: Into<BitFlags<T>>
{
    fn from_iter<I>(it: I) -> BitFlags<T>
    where 
        I: IntoIterator<Item = B>
    {
        let mut flags = BitFlags::empty();
        for flag in it {
            flags |= flag.into();
        }
        flags
    }
}
