mod proxy {
    use std::fmt;

    #[telety::telety(crate::proxy, proxy = "fmt::Debug")]
    pub trait Debug {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error>;
    }
}

macro_rules! test_item {
    ($t:ty, $v:ty, $n:literal) => {{
        crate::assert_discriminant::<$t, $v, $n>();
        crate::round_trip::<$t, $v>
    }};
}

mod basic {
    use tagset::tagset;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT0;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT1;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT2;

    #[tagset(impl super::proxy::Debug)]
    #[tagset(impl tagset::proxy::serde::Serialize)]
    #[tagset(impl<'de> tagset::serde::DeserializeFromDiscriminant<'de>)]
    #[tagset(impl<'de> tagset::proxy::serde::Deserialize<'de>)]
    #[tagset(MyT0)]
    #[tagset(MyT1)]
    #[tagset(MyT2)]
    struct MySet;

    #[test]
    fn basic() {
        test_item!(MySet, MyT0, 0)(MyT0);
        test_item!(MySet, MyT1, 1)(MyT1);
        test_item!(MySet, MyT2, 2)(MyT2);
    }
}

mod indexing {
    use tagset::tagset;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT0;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT1;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT2;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT3;

    #[tagset(index(10u32))]
    #[tagset(impl super::proxy::Debug)]
    #[tagset(impl tagset::proxy::serde::Serialize)]
    #[tagset(impl<'de> tagset::serde::DeserializeFromDiscriminant<'de>)]
    #[tagset(impl<'de> tagset::proxy::serde::Deserialize<'de>)]
    // 10
    #[tagset(MyT0)]
    #[tagset(deprecated)]
    // 12
    #[tagset(MyT1)]
    #[tagset(reserved(..=13))]
    // 14
    #[tagset(MyT2)]
    #[tagset(deprecated(..17))]
    // 17
    #[tagset(MyT3)]
    struct MySet3;

    #[test]
    fn indexing() {
        test_item!(MySet3, MyT0, 10)(MyT0);
        test_item!(MySet3, MyT1, 12)(MyT1);
        test_item!(MySet3, MyT2, 14)(MyT2);
        test_item!(MySet3, MyT3, 17)(MyT3);
    }
}

pub mod include {
    use tagset::tagset;

    pub mod submodule {
        use tagset::tagset;

        #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
        pub struct MySubT0;
        #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
        pub struct MySubT1;

        #[telety::telety(crate::include::submodule)]
        #[tagset(index(-99i32))]
        #[tagset(reserved(..-98))]
        #[tagset(MySubT0)] // -98
        #[tagset(reserved(..-95))]
        #[tagset(MySubT1)] // -95
        #[tagset(reserved(..=-94))]
        pub struct MySubSet;
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT0;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT1;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT2;

    #[tagset(impl super::proxy::Debug)]
    #[tagset(impl tagset::proxy::serde::Serialize)]
    #[tagset(impl<'de> tagset::serde::DeserializeFromDiscriminant<'de>)]
    #[tagset(impl<'de> tagset::proxy::serde::Deserialize<'de>)]
    #[tagset(MyT0)]
    #[tagset(include(submodule::MySubSet))]
    #[tagset(MyT1)]
    #[tagset(MyT2)]
    struct MySet4;

    #[test]
    fn include() {
        test_item!(MySet4, MyT0, 0)(MyT0);
        test_item!(MySet4, submodule::MySubT0, 2)(submodule::MySubT0);
        test_item!(MySet4, submodule::MySubT1, 5)(submodule::MySubT1);
        test_item!(MySet4, MyT1, 7)(MyT1);
        test_item!(MySet4, MyT2, 8)(MyT2);
    }
}

mod generics {
    use tagset::tagset;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT0<T>(T);
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT1<T>(T);

    #[tagset(impl super::proxy::Debug)]
    #[tagset(impl tagset::proxy::serde::Serialize)]
    #[tagset(impl<'de> tagset::serde::DeserializeFromDiscriminant<'de>)]
    #[tagset(impl<'de> tagset::proxy::serde::Deserialize<'de>)]
    #[tagset(MyT0<T>)]
    #[tagset(MyT1<T>)]
    struct MySet5<T>
    where
        T: std::fmt::Debug;

    #[test]
    fn generics() {
        test_item!(MySet5::<String>, MyT0::<_>, 0)(MyT0(format!("asdf")));
        test_item!(MySet5::<u32>, MyT1::<_>, 1)(MyT1(4u32));
    }
}

mod item_override {
    use tagset::tagset;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT0;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT1;

    #[tagset(impl super::proxy::Debug {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
            write!(f, "MySet6")
        }
    })]
    #[tagset(MyT0)]
    #[tagset(MyT1)]
    struct MySet6;

    #[test]
    fn item_override() {
        let set: MySet6 = MyT0.into();
        assert_eq!(format!("{:?}", set), "MySet6");
    }
}

mod extern_tag {}

#[cfg(feature = "serde")]
mod include_generics {
    use tagset::tagset;

    pub mod submodule {
        use tagset::tagset;

        #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
        pub struct MySubT0<T>(pub(crate) T);
        #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
        pub struct MySubT1<T, U>(pub(crate) T, pub(crate) U);

        #[telety::telety(crate::include_generics::submodule)]
        #[tagset(impl tagset::proxy::serde::Serialize)]
        // #[tagset(impl<'de> tagset::serde::DeserializeFromDiscriminant<'de>)]
        // #[tagset(impl<'de> tagset::proxy::serde::Deserialize<'de>)]
        #[tagset(index(-99i32))]
        #[tagset(reserved(..-98))]
        #[tagset(MySubT0<T>)] // -98
        #[tagset(reserved(..-95))]
        #[tagset(MySubT1<T, U>)] // -95
        #[tagset(reserved(..=-94))]
        pub struct MySubSet2<T, U>;
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT0<T>(T);
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT1;
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    struct MyT2<U>(U);

    #[tagset(impl super::proxy::Debug)]
    #[tagset(impl tagset::proxy::serde::Serialize)]
    #[tagset(impl<'de> tagset::serde::DeserializeFromDiscriminant<'de>)]
    #[tagset(impl<'de> tagset::proxy::serde::Deserialize<'de>)]
    #[tagset(MyT0<T>)]
    #[tagset(include(submodule::MySubSet2<T, U>))]
    #[tagset(MyT1)]
    #[tagset(MyT2<U>)]
    struct MySet7<T, U>;

    #[test]
    fn include() {
        test_item!(MySet7<i32, u32>, MyT0<i32>, 0)(MyT0(0));
        test_item!(MySet7<i32, u32>, submodule::MySubT0<i32>, 2)(submodule::MySubT0(0));
        test_item!(MySet7<i32, u32>, submodule::MySubT1<i32, u32>, 5)(submodule::MySubT1(0, 0));
        test_item!(MySet7<i32, u32>, MyT1, 7)(MyT1);
        test_item!(MySet7<i32, u32>, MyT2<u32>, 8)(MyT2(0));
    }
}

// TODO included generics

fn round_trip<T, V>(value: V) -> T
where
    V: Into<T> + proxy::Debug,
    T: serde::Serialize + serde::de::DeserializeOwned + proxy::Debug,
{
    let value_str = format!("{value:?}");
    let erased: T = value.into();
    let erased_str = format!("{:?}", &erased);
    assert_eq!(value_str, erased_str,);
    let s = serde_json::to_string_pretty(&erased).expect("to_string_pretty");
    println!("{s}");
    let erased2: T = serde_json::from_str(&*s).expect("from_slice");
    assert_eq!(erased_str, format!("{:?}", &erased2),);
    erased2
}

fn assert_discriminant<T, V, const N: u32>()
where
    T: tagset::TagSetDiscriminant<V, Repr = u32>,
{
    assert_eq!(N, T::DISCRIMINANT);
}
