use tagset::tagset;

#[derive(Debug)]
pub struct A;
#[derive(Debug)]
pub struct B;

#[tagset(A)]
#[tagset(B)]
#[tagset(derive(Debug))]
pub struct MySet;

#[test]
fn from_impls() {
    let a = A;
    let b = B;

    let mut set_a: MySet = a.into();
    let mut set_b: MySet = b.into();

    let _: &A = (&set_a).try_into().unwrap();
    let _: &B = (&set_b).try_into().unwrap();
    let _: &mut A = (&mut set_a).try_into().unwrap();
    let _: &mut B = (&mut set_b).try_into().unwrap();
    let _: A = set_a.try_into().unwrap();
    let _: B = set_b.try_into().unwrap();
}