pub(crate) fn make_alias_map<'map>(
    telety: &'map telety::Telety,
) -> syn::Result<telety::alias::Map<'map>> {
    let mut sub_map = telety.alias_map().new_sub_map("tagset");

    let mut direct = crate::direct::TagsetDirect::new();
    directed_visit::visit(
        &mut direct,
        &mut telety::visitor::IdentifyAliases::new(&mut sub_map),
        telety.item(),
    );
    direct.into_result()?;

    // let attrs: Result<Vec<_>, _> = telety.attributes()
    //     .iter()
    //     .filter(|attr| attr.meta.path().is_ident("tagset"))
    //     .map(|attr| attr.parse_args::<parsing::Attr>())
    //     .collect();
    // let attrs = attrs?;

    // let mut visitor = telety::visitor::IdentifyAliases::new(&mut sub_map);

    // for attr in &attrs {
    //     attr.visit(&mut visitor);
    // }

    Ok(sub_map)
}
