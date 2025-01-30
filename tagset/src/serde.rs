#[telety::telety(crate::serde)]
#[crate::tagset_meta]
#[meta(bounds(for<VARIANT> VARIANT: serde::Deserialize<'de>))]
pub trait DeserializeFromDiscriminant<'de>: crate::TagSet + Sized {
    #[meta(default {
        match_by_discriminant!(discriminant, Variant => 
            <Self as ::tagset::serde::DeserializeFromDiscriminant<'_>>::next_element_of_type::<_, Variant>(seq),
            // Invalid discriminant
            Err(<Self as ::tagset::serde::DeserializeFromDiscriminant<'_>>::invalid_discriminant(discriminant))
        )
    })]
    fn next_element<A>(seq: A, discriminant: &Self::Repr) -> Result<Self, A::Error>
    where
        A: serde::de::SeqAccess<'de>;

    fn next_element_of_type<A, T>(mut seq: A) -> Result<Self, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
        T: serde::Deserialize<'de> + Into<Self>,
    {
        let element = seq
            .next_element::<T>()?
            .ok_or(serde::de::Error::invalid_length(
                1,
                &"tuple struct with 2 elements",
            ))?;
        Ok(element.into())
    }

    fn invalid_discriminant<Error>(discriminant: &Self::Repr) -> Error
    where
        Self::Repr: crate::__private::serde::ToUnexpected,
        Error: serde::de::Error,
    {
        let unexpected = crate::__private::serde::ToUnexpected::to_unexpected(discriminant);
        serde::de::Error::invalid_value(unexpected, &"a valid discriminant")
    }
}

#[doc(hidden)]
pub mod __private {
    use std::marker::PhantomData;

    pub use ::serde as serde_crate;

    pub trait ToUnexpected {
        fn to_unexpected(&self) -> serde::de::Unexpected;
    }

    pub struct Visitor<T>(PhantomData<T>);

    impl<T> Default for Visitor<T> {
        fn default() -> Self {
            Self(Default::default())
        }
    }

    impl<'de, T> serde::de::Visitor<'de> for Visitor<T>
    where
        T: crate::serde::DeserializeFromDiscriminant<'de, Repr: serde::Deserialize<'de>>,
    {
        type Value = T;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(formatter, "a length 2 tuple")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>,
        {
            let discriminant =
                seq.next_element::<T::Repr>()?
                    .ok_or(serde::de::Error::invalid_length(
                        0,
                        &"tuple struct with 2 elements",
                    ))?;

            T::next_element(seq, &discriminant)
        }
    }

    macro_rules! impl_to_unexpected {
        ($($variant:ident $to:ty => $($from:ty),+ );+;) => {
            $(
                $(
                    impl ToUnexpected for $from {
                        fn to_unexpected(&self) -> serde::de::Unexpected {
                            serde::de::Unexpected::$variant(<$to>::from(*self))
                        }
                    }
                )+
            )+
        };
    }

    impl_to_unexpected!(
        Unsigned u64 => u8, u16, u32, u64;
        Signed i64 => i8, i16, i32, i64;
    );
}
