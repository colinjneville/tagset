mod direct;
mod parsing;
mod remove_attr;
mod tagset;
mod tagset_impl;
mod tagset_meta;
mod tagset_telety_alias_map;
mod tagset_trait_impl;
mod telety_util;
mod tokens;

#[proc_macro_attribute]
pub fn tagset(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let options = parsing::MacroOptions::new(proc_macro2::Span::call_site(), false);
    let (Ok(tokens) | Err(tokens)) =
        tagset::tagset(attr.into(), item.into(), options).map_err(|e| e.into_compile_error());
    tokens.into()
}

#[proc_macro_attribute]
pub fn tagset_serde(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let options = parsing::MacroOptions::new(proc_macro2::Span::call_site(), true);
    let (Ok(tokens) | Err(tokens)) =
        tagset::tagset(attr.into(), item.into(), options).map_err(|e| e.into_compile_error());
    tokens.into()
}

#[doc(hidden)]
#[proc_macro]
pub fn tagset_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let (Ok(tokens) | Err(tokens)) =
        tagset_impl::tagset_impl(input.into()).map_err(|e| e.into_compile_error());
    tokens.into()
}

#[doc(hidden)]
#[proc_macro]
pub fn tagset_telety_alias_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let (Ok(tokens) | Err(tokens)) = tagset_telety_alias_map::tagset_telety_alias_map(input.into())
        .map_err(|e| e.into_compile_error());
    tokens.into()
}

#[doc(hidden)]
#[proc_macro]
pub fn tagset_trait_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let (Ok(tokens) | Err(tokens)) =
        tagset_trait_impl::tagset_trait_impl(input.into()).map_err(|e| e.into_compile_error());
    tokens.into()
}

#[proc_macro_attribute]
pub fn tagset_meta(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let (Ok(tokens) | Err(tokens)) =
        tagset_meta::tagset_meta(attr.into(), item.into()).map_err(|e| e.into_compile_error());
    tokens.into()
}
