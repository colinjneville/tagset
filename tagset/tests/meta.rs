mod traits {
    #[telety::telety(crate::traits)]
    #[tagset::tagset_meta]
    pub trait Tr {
        #[meta(default {
            0
        })]
        fn do_it(&self) -> i32;
    }

    struct A;
    impl Tr for A {
        fn do_it(&self) -> i32 {
            1
        }
    }
    struct B;
    impl Tr for B {
        fn do_it(&self) -> i32 {
            2
        }
    }
    struct C;
    impl Tr for C {
        fn do_it(&self) -> i32 {
            3
        }
    }

    #[tagset::tagset(impl Tr)]
    #[tagset(A)]
    #[tagset(B)]
    #[tagset(C)]
    struct S;

    #[test]
    fn default_impl() {
        // Set element impls are ignored, the default impl is used
        let s = S::from(A);
        assert_eq!(s.do_it(), 0);
    }
}
