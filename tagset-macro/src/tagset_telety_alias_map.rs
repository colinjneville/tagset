use proc_macro2::TokenStream;
use quote::quote;
use syn::parse2;

use crate::telety_util;

pub(crate) fn tagset_telety_alias_map(input: TokenStream) -> syn::Result<TokenStream> {
    let item: syn::Item = parse2(input)?;
    let telety = telety::Telety::new(&item)?;
    let sub_map = telety_util::make_alias_map(&telety)?;

    Ok(quote! {
        #sub_map
    })
}
