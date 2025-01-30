#[telety::telety(crate::proxy::serde, proxy = "serde::Serialize")]
#[crate::tagset_meta]
pub trait Serialize {
    #[meta(default {
        use tagset::__private::serde::serde_crate::ser::SerializeTuple;
        use tagset::TagSet;

        let mut seq = serializer.serialize_tuple(2)?;
        seq.serialize_element(&self.discriminant())?;
        match_by_value!(self, value => seq.serialize_element(value)?);
        seq.end()
    })]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer;
}

#[telety::telety(crate::proxy::serde, proxy = "serde::Deserialize")]
#[crate::tagset_meta]
pub trait Deserialize<'de>: Sized {
    #[meta(default {
        deserializer.deserialize_tuple(2, tagset::__private::serde::Visitor::default())
    })]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>;
}
