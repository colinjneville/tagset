//! Easily create trait-dispatching sum types (TagSets).
//! * Fixed discriminant values - Each included type's discriminant will not change,
//!   even as new variants are added in future versions.
//! * Build-in serde support - Easily derive Serialize and Deserialize for your set.
//! * Supports any trait - Trait items which are not dispatchable (such as associated types) can be
//!   explicitly provided.
//! * Compositable sets - Sets can easily include other sets as sub-sets.
//! * Extended trait defaults - Annotate your trait to allow for trait defaults not supported by standard Rust,
//!   such as associated type defaults.
//! * (Mostly) scope-independent - With a few rare exceptions, nothing is required to be in scope,
//!   and types and traits are not required to have unique identifiers.
//! ```rust
//! /// Mark traits with `telety`(<https://crates.io/crates/telety>):
//! pub mod my_trait {
//!     #[telety::telety(crate::my_trait)]
//!     pub trait MyTrait {
//!         fn ascii_char(&self) -> u8;
//!     }
//! }
//!
//! pub mod my_impl {
//!     #[derive(Debug)]
//!     pub struct A;
//!     impl super::my_trait::MyTrait for A {
//!         fn ascii_char(&self) -> u8 {
//!             b'A'
//!         }
//!     }
//!
//!     #[derive(Debug)]
//!     pub struct B;
//!     impl super::my_trait::MyTrait for B {
//!         fn ascii_char(&self) -> u8 {
//!             b'B'
//!         }
//!     }
//! }
//!
//! pub mod my_set {
//!     # use tagset::tagset;
//!
//!     // Define your dispatching set type using tagset attributes:
//!     // Implements `MyTrait` for `MyImplementor`
//!     #[tagset(impl super::my_trait::MyTrait)]
//!     // Adds `A` as a variant of `MyImplementer` and generates a `From<A>` and `TryFrom<MySet>` impl
//!     #[tagset(super::my_impl::A)]
//!     // Adds `B` as a variant of `MyImplementer` and generates a `From<B>` and `TryFrom<MySet>` impl
//!     #[tagset(super::my_impl::B)]
//!     #[tagset(derive(Debug))]
//!     pub struct MySet;
//! }
//!
//! fn main() {
//!     use my_trait::MyTrait as _;
//!
//!     // MySet implements From for all variants
//!     let set_a: my_set::MySet = my_impl::A.into();
//!     let set_b: my_set::MySet = my_impl::B.into();
//!
//!     assert_eq!(set_a.ascii_char(), b'A');
//!     assert_eq!(set_b.ascii_char(), b'B');
//!
//!     // TryFrom<MySet> is implemented for all variants,
//!     // as well as &MySet and &mut MySet versions
//!     let a: my_impl::A = set_a.try_into().unwrap();
//!     // Conversion by value returns the original value in Err
//!     let result_b: Result<my_impl::A, my_set::MySet> = set_b.try_into();
//!     let set_b: my_set::MySet = result_b.unwrap_err();
//! }
//! ```
//! You can also add generic parameters and bounds:
//! ```rust
//!
//! pub mod my_trait {
//!     #[telety::telety(crate::my_trait)]
//!     pub trait MyTrait<U> { }
//! }
//!
//! pub mod my_impl {
//!     #[derive(Clone)]
//!     pub struct Parameter<T>(T);
//!     impl<T, U> super::my_trait::MyTrait<U> for Parameter<T> { }
//! }
//!
//! pub mod my_set {
//!     # use tagset::tagset;
//!
//!     #[tagset(impl<U> super::my_trait::MyTrait<U>
//!         // See the type bounds section for details
//!         where for<VAR> VAR: Clone
//!     )]
//!     // You can use type parameters from the set
//!     #[tagset(super::my_impl::Parameter<T>)]
//!     pub struct MySet<T>;
//! }
//!
//! # fn main() { }
//! ```
//!
//! # Discriminants
//! tagset supports consistent discriminants. Like enums, by default numbering is incremental starting at 0.  
//!
//! To choose the initial discriminant, use the `#[tagset(index(...))]` attribute. `index` takes an integer
//! literal with an optional suffix if you want to specify the discriminant type. e.g., `[tagset(index(-4ii8))]`
//! sets the discriminant type as `i8`, and the first discriminant value to `-4`.  
//!
//! To modify the default numbering, use `#[tagset(reserved(...))]` or `#[tagset(deprecated(...))]` attributes.
//! (Both are functually equivalent, but semantically distinguish discriminants that can and cannot be used in
//! the future). Both attributes take a range literal with an open start and an inclusive or exclusive end, e.g.
//! `..5` or `..=5`. All discriminants starting after the previous variant to the end of the range will be skipped.
//!
//! ```rust
//! # use tagset::tagset;
//! pub struct A;
//! pub struct B;
//! pub struct C;
//!
//! #[tagset(index(2u8))]
//! #[tagset(A)] // 2
//! #[tagset(deprecated(..5))]
//! #[tagset(B)] // 5
//! #[tagset(C)] // 6
//! #[tagset(reserved(..16))]
//! pub struct MySet;
//!
//! fn main() {
//!     use tagset::TagSet as _;
//!
//!     let a: MySet = A.into();
//!     let b: MySet = B.into();
//!     let c: MySet = C.into();
//!     
//!     assert_eq!(a.discriminant(), 2u8);
//!     assert_eq!(b.discriminant(), 5u8);
//!     assert_eq!(c.discriminant(), 6u8);
//! }
//! ```  
//!
//! Discriminant values are available at runtime through the [TagSet] trait, and at compile-time with
//! [TagSetDiscriminant] (variant type to variant discriminant) and [TagSetTypeI32] (variant discriminant to variant type)
//! (use the trait corresponding to the discriminant type, [TagSetTypeU8], etc.).  
//!
//! # External traits
//! If a trait comes from an external crate, you can create a telety proxy for it:
//! ```rust
//! pub mod external_crate {
//!     // No telety attribute
//!     pub trait MyTrait {
//!         fn ascii_char(&self) -> u8;
//!     }
//! }
//!
//! pub mod internal_proxy {
//!     // The actual trait from external_crate is re-exported here, along with the
//!     // telety info as if it were defined here
//!     #[telety::telety(crate::internal_proxy, proxy = "crate::external_crate::MyTrait")]
//!     pub trait MyTrait {
//!         fn ascii_char(&self) -> u8;
//!     }
//! }
//!
//! pub mod proxy_impl {
//!     # use tagset::tagset;
//!
//!     pub struct A;
//!     
//!     impl super::external_crate::MyTrait for A {
//!         fn ascii_char(&self) -> u8 {
//!             b'A'
//!         }
//!     }
//!
//!     pub struct B;
//!
//!     // Either external_crate::MyTrait or internal_proxy::MyTrait can be used -
//!     // they refer to the exact same trait
//!     impl super::internal_proxy::MyTrait for B {
//!         fn ascii_char(&self) -> u8 {
//!             b'B'
//!         }
//!     }
//!
//!     #[tagset(impl super::internal_proxy::MyTrait)]
//!     #[tagset(A)]
//!     #[tagset(B)]
//!     pub struct Set;
//! }
//!
//! # fn main() { }
//! ```
//!
//! # Overrides
//!
//! You can override item definitions inside traits:
//! ```rust
//! pub mod my_trait {
//!     #[telety::telety(crate::my_trait)]
//!     pub trait MyTrait {
//!         type Output;
//!
//!         fn get(&self) -> Self::Output;
//!     }
//! }
//!
//! pub mod my_impl {
//!     pub struct A(pub i32);
//!     
//!     impl super::my_trait::MyTrait for A {
//!         type Output = i32;
//!         fn get(&self) -> Self::Output {
//!             self.0
//!         }
//!     }
//!
//!     pub struct B(pub u32);
//!     
//!     impl super::my_trait::MyTrait for B {
//!         type Output = u32;
//!         fn get(&self) -> Self::Output {
//!             self.0
//!         }
//!     }
//! }
//!
//! pub mod my_set {
//!     # use tagset::tagset;
//!
//!     #[tagset(impl super::my_trait::MyTrait {
//!         // associated types usually require an override
//!         type Output = i64;
//!         // trait functions will usually delegate to the variant by default,
//!         // but sometimes this is not possible (e.g. there is no reciever,
//!         // or the reciever is not convertable such as Rc<Self>), or not
//!         // desired.
//!         // Overrides are provided 2 macros to generate implementations based
//!         // based on the included types:
//!         //   `match_by_value!(value, value_identifier => expr)`
//!         //     `value` is a value of type `Self`, `&Self`, or `&mut Self`.
//!         //     `value_identifier` is any identifier to be used in `expr` as
//!         //       the inner value of the set.
//!         //     `expr` is an expression in which `value_identifier` is the
//!         //       inner value of the set.
//!         //   `match_by_discriminant!(discriminant, type_identifier => expr)`
//!         //     `discriminant` is a value of the type of the set's discriminant.
//!         //       If the value is invalid, the function will panic.
//!         //     `type_identifier` is any identifier to be substituted with the
//!         //       type path corresponding to `discriminant`.
//!         //     `expr` is an expression in which `type_identifier` will be
//!         //       substituted for `discriminant`'s corresponding type path.
//!         fn get(&self) -> Self::Output {
//!             match_by_value!(self, value => value.get() as i64)
//!         }
//!     })]
//!     #[tagset(super::my_impl::A)]
//!     #[tagset(super::my_impl::B)]
//!     pub struct MySet;
//! }
//!
//! fn main() {
//!     use my_trait::MyTrait as _;
//!
//!     let a: my_set::MySet = my_impl::A(3i32).into();
//!     let b: my_set::MySet = my_impl::B(4u32).into();
//!     
//!     assert_eq!(a.get() + b.get(), 7);
//! }
//! ```
//!
//! # Type bounds
//! tagset support a special syntax when specifying type bounds:  
//! `for <VAR>: VAR: MyTrait`  
//! This bound is expanded for variant, substituting `VAR` in the bound for the type path of the variant.
//! For example, in a set containing types `A`, `B`, and `C`, this is equivalent to:
//! ```rust, ignore
//!     A: MyTrait,
//!     B: MyTrait,
//!     C: MyTrait,
//! ```
//!
//! # Includes
//! Sets are compositable: you can use `#[tagset(include(MySubSet))]` to include all variant types from
//! a telety-enabled `MySubSet`.  
//! Included types will retain their *relative* discriminants, but not their *absolute* discriminants.  
//! ```rust
//! pub mod my_subset {
//!     # use tagset::tagset;
//!
//!     pub struct A;
//!     pub struct B;
//!     
//!     #[telety::telety(crate::my_subset)]
//!     #[tagset(index(7i32))]
//!     #[tagset(A)]
//!     #[tagset(reserved(..11))]
//!     #[tagset(B)]
//!     pub struct MySubSet;
//! }
//!
//! pub mod my_set {
//!     # use tagset::tagset;
//!
//!     pub struct C;
//!     pub struct D;
//!
//!     #[tagset(index(3u64))]
//!     #[tagset(C)]
//!     // Though `A`'s discriminant is 7 in `MySubSet`,
//!     // here it will be 4
//!     #[tagset(include(super::my_subset::MySubSet))]
//!     // `D`'s discriminant will be directly after `C`'s (8 -> 9)
//!     #[tagset(D)]
//!     pub struct MySet;
//! }
//!
//! fn main() {
//!     let a: my_subset::MySubSet = my_subset::A.into();
//!     let b: my_subset::MySubSet = my_subset::B.into();
//!     assert_eq!(a.discriminant(), 7);
//!     assert_eq!(b.discriminant(), 11);
//!
//!     use tagset::TagSet as _;
//!     let a: my_set::MySet = my_subset::A.into();
//!     let b: my_set::MySet = my_subset::B.into();
//!     let c: my_set::MySet = my_set::C.into();
//!     let d: my_set::MySet = my_set::D.into();
//!
//!     assert_eq!(a.discriminant(), 4);
//!     assert_eq!(b.discriminant(), 8);
//!     assert_eq!(c.discriminant(), 3);
//!     assert_eq!(d.discriminant(), 9);
//! }
//! ```
//!
//! # Metadata
//! tagset supports adding metadata to traits to control default implementation.  
//! To add metadata to a trait, use the [tagset_meta] attribute, then use `#[meta(...)]` attributes
//! where desired.
//!
//! ## default
//! `default` can be applied to associated types or associated functions.  
//! When applied to an associated type, it takes a type path which will serve as the default for
//! any implementing set. In this case, it is not necessary to provide an override in the set impl.  
//! When applied to a trait function, it takes a function body. The macros mentioned in the overrides
//! section are available here as well.  
//! Note: telety aliases all types used in the function body so they are usable at the set definition
//! without importing them, but it cannot alias `use` items or values. Those must be textually valid
//! at the location of the set definition.
//!
//! ## bounds
//! By default, the set implements traits when all its variants implement the trait. This may be too
//! strict or too lax for some default implementations. The `bounds` meta attribute takes
//! where predicates to use as the baseline bounds to implement the trait for a set. See the
//! type bounds section for special bounds syntax.
//!
//! ```rust
//! pub mod my_trait {
//!     #[telety::telety(crate::my_trait)]
//!     #[tagset::tagset_meta]
//!     // We don't need any bounds for our default impl
//!     #[meta(bounds())]
//!     pub trait MyTrait {
//!         #[meta(default(std::convert::Infallible))]
//!         type Error;
//!
//!         #[meta(default {
//!             match_by_value!(self, value => Ok(std::any::type_name_of_val(value)))
//!         })]
//!         fn name(&self) -> Result<&'static str, Self::Error>;
//!     }
//! }
//!
//! pub mod my_set {
//!     # use tagset::tagset;
//!
//!     // Because MyTrait has no default bounds, these don't need to implement MyTrait
//!     pub struct A;
//!     pub struct B;
//!     
//!     #[tagset(impl super::my_trait::MyTrait)]
//!     #[tagset(A)]
//!     #[tagset(B)]
//!     pub struct MySet;
//! }
//!
//! fn main() {
//!     use my_trait::MyTrait as _;
//!
//!     let a: my_set::MySet = my_set::A.into();
//!     let b: my_set::MySet = my_set::B.into();
//!
//!     assert_eq!(a.name().unwrap().split("::").last().unwrap(), "A");
//!     assert_eq!(b.name().unwrap().split("::").last().unwrap(), "B");
//! }
//! ```
//!
//! # derive
//!
//! tagset currently uses a private internal type when implementing the set. Because of this,
//! derives do not work as-is. Wrap them in a tagset attribute:
//! ```rust
//! pub mod my_set {
//!     # use tagset::tagset;
//!
//!     #[derive(Clone)]
//!     pub struct A;
//!
//!     #[tagset(derive(Clone))]
//!     #[tagset(A)]
//!     pub struct MySet;
//! }
//!
//! fn main() {
//!     let before: my_set::MySet = my_set::A.into();
//!     let after = before.clone();
//! }
//! ```
//!
//! # serde
//!
//! When the serde feature is enabled, tagset includes Serialize and Deserialize proxies
//! with default implementations. Currently, the Deserialize implementation requires also
//! implementing the trait [serde::DeserializeFromDiscriminant], but this may be automatic
//! in the future.
//!
//! ```rust,ignore
//! # // This does not work as a doctest for some reason, see tests/serde_roundtrip.rs
//! # use tagset::tagset;
//!
//! #[derive(serde::Serialize, serde::Deserialize)]
//! pub struct Number(pub i32);
//!
//! #[derive(serde::Serialize, serde::Deserialize)]
//! pub struct Text(pub String);
//!
//! #[tagset(impl tagset::proxy::serde::Serialize)]
//! #[tagset(impl<'de> tagset::serde::DeserializeFromDiscriminant<'de>)]
//! #[tagset(impl<'de> tagset::proxy::serde::Deserialize<'de>)]
//! #[tagset(Number)]
//! #[tagset(Text)]
//! pub struct MySet;
//!
//! fn main() -> anyhow::Result<()> {
//!     let value: MySet = Text("asdf".to_string()).into();
//!
//!     let serialized = serde_json::to_string(&value)?;
//!     let deserialized: MySet = serde_json::from_str(&*serialized)?;
//!     
//!     let text: Text = deserialized.try_into().unwrap_or_else(|_| unreachable!());
//!     assert_eq!(text.0, "asdf");
//!     Ok(())
//! }
//! ```
//!
//! The default (de)serialization implementation structures the set as a 2-element tuple.
//! The first element is the discriminant, the second is the inner variant.
//!

#![allow(
    clippy::crate_in_macro_def,
    reason = "This is intentional for telety aliasing"
)]

// This allows us to use the #[tagset_meta] in this crate, which creates paths beginning with '::tagset'
extern crate self as tagset;

/// telety proxy definitions
pub mod proxy;

#[cfg(feature = "serde")]
/// serde helpers
pub mod serde;

pub use tagset_macro::tagset;

pub use tagset_macro::tagset_meta;

#[doc(hidden)]
pub mod __private {
    #[cfg(feature = "serde")]
    pub use crate::serde::__private as serde;

    #[diagnostic::on_unimplemented(
        message = "'{Self}' is not a supported tagset receiver type",
        note = "Receiver must be self, &self, or &mut self"
    )]
    pub trait IntoImpl {
        type Impl;
        fn into_impl(self) -> Self::Impl;
    }

    pub use telety;

    pub use core::convert::{From, TryFrom};
    pub use core::marker::PhantomData;
    pub use core::mem::transmute;
    pub use core::ptr::from_ref;

    pub use tagset_macro::tagset_impl;
    pub use tagset_macro::tagset_telety_alias_map;
    pub use tagset_macro::tagset_trait_impl;
}

/// Implemented for all types with the [macro@tagset] attribute.
pub trait TagSet {
    /// The type of the discriminant value
    type Repr;

    /// The discriminant value as determined by the underlying variant type
    fn discriminant(&self) -> Self::Repr;
}

/// Implemented for all types with the [macro@tagset] attribute.
/// Allows compile-time access to the discriminant of a given variant type.
pub trait TagSetDiscriminant<V>: TagSet {
    const DISCRIMINANT: Self::Repr;
}

macro_rules! discriminant_traits {
    ($($ty:ty),+) => {
        paste::paste! {
            $(
                /// Implemented for types with the [macro@tagset] attribute with a [$ty] discriminant.
                /// Allows compile-time access to the variant type of a given discriminant value.
                pub trait [<TagSetType $ty:camel>]<const N: $ty>: TagSet<Repr = $ty> {
                    type Type;
                }
            )+

            // #[macro_export]
            // macro_rules! discriminant_from_type {
            //     $(
            //         ($ty) => {
            //             $crate::[<TypeFromDiscriminant $ty:snake>]
            //         };
            //     )+
            // }
        }
    };
}

discriminant_traits!(
    u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize
);
