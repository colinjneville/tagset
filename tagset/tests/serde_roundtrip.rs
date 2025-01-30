use tagset::tagset;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Number(i32);

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Text(String);

#[tagset(impl tagset::proxy::serde::Serialize)]
#[tagset(impl<'de> tagset::serde::DeserializeFromDiscriminant<'de>)]
#[tagset(impl<'de> tagset::proxy::serde::Deserialize<'de>)]
#[tagset(Number)]
#[tagset(Text)]
pub struct MySet;

#[test]
fn serde_roundtrip() -> anyhow::Result<()> {
    let value: MySet = Text("asdf".to_string()).into();

    let serialized = serde_json::to_string(&value)?;
    println!("{serialized}");
    let deserialized: MySet = serde_json::from_str(&*serialized)?;

    let b: Text = deserialized.try_into().unwrap_or_else(|_| unreachable!());
    assert_eq!(b.0, "asdf");
    Ok(())
}
