
#[telety::telety(crate)]
#[tagset::tagset_meta]
#[meta(bounds())]
trait Tr {
    #[meta(default {
        let mut variants = vec![];
        foreach!(T => {
            variants.push(std::any::type_name::<T>().to_string());
        });
        variants
    })]
    fn variants() -> Vec<String>;

    #[meta(default {
        match_by_discriminant!(discriminant, T => {
            std::any::type_name::<T>().to_string()
        })
    })]
    fn discriminant_variant(discriminant: i32) -> String;

    #[meta(default {
        match_by_value!(self, variant => {
            std::any::type_name_of_val(variant).to_string()
        })
    })]
    fn variant(&self) -> String;
}

struct A;
struct B;
struct C;

#[tagset::tagset(impl Tr)]
#[tagset(A)]
#[tagset(B)]
#[tagset(C)]
struct S;

#[test]
fn foreach() {
    let variants = <S as Tr>::variants();
    assert_eq!(variants, vec!["macros::A", "macros::B", "macros::C"]);    
}

#[test]
fn match_by_value() {
    let s = S::from(A);
    assert_eq!(s.variant(), "macros::A");
    let s = S::from(B);
    assert_eq!(s.variant(), "macros::B");
    let s = S::from(C);
    assert_eq!(s.variant(), "macros::C");
}

#[test]
fn match_by_discriminant() {
    assert_eq!(S::discriminant_variant(0), "macros::A");
    assert_eq!(S::discriminant_variant(1), "macros::B");
    assert_eq!(S::discriminant_variant(2), "macros::C");
}