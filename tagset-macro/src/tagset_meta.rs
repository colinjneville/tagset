use std::hash::{Hash as _, Hasher as _};

use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{parse_quote, spanned::Spanned as _};

pub(crate) fn tagset_meta(_attr: TokenStream, item: TokenStream) -> syn::Result<TokenStream> {
    let mut hasher = std::hash::DefaultHasher::new();
    // TODO This depends on details of the Span being exposed in the Debug formatting.
    // Once Span::file stabilizes (hopefully) soon, that should be enough to avoid
    // all non-contrived collisions.
    format!("{:?}", item.span()).hash(&mut hasher);
    item.to_string().hash(&mut hasher);
    let hash = hasher.finish();

    let mut item: syn::Item = syn::parse2(item)?;
    let syn::Item::Trait(item_trait) = &item else {
        return Err(syn::Error::new(item.span(), "Expected a trait"));
    };
    let ident = &item_trait.ident;

    let telety_module = {
        let needle = syn::Ident::new("__needle__", Span::call_site());
        let haystack = quote! {
            ::tagset::__private::tagset_telety_alias_map! {
                #needle
            }
        };

        let command = telety::v1::TY
            .apply(parse_quote!(super::#ident), needle, haystack)
            .with_fallback(TokenStream::new())
            .with_macro_forwarding(format_ident!("tagset_telety_{hash}"))
            .with_telety_path(parse_quote!(::tagset::__private::telety));

        let module = telety::alias::Module::from_named_item(&item)?.new_child("tagset");
        module.with_contents(&command)
    };

    // Remove our fake attributes
    let mut direct = crate::direct::TagsetDirect::new();
    let mut visitor = crate::remove_attr::RemoveAttr::new(|attr| {
        attr.path().is_ident(crate::parsing::TagsetMetaNone::IDENT)
    });
    directed_visit::visit_mut(&mut direct, &mut visitor, &mut item);
    direct.into_result()?;

    Ok(quote::quote! {
        #item
        #telety_module
    })
}

#[cfg(test)]
mod test {
    use quote::ToTokens;

    fn expect_aliases(map: &telety::alias::Map, aliases: &[&str]) {
        use std::collections::HashSet;

        let mut expected_aliases = HashSet::new();
        for alias in aliases {
            let inserted = expected_aliases.insert(*alias);
            assert!(inserted);
        }

        for alias in map.iter_aliases() {
            let s = alias.aliased_path().to_token_stream().to_string();
            if !expected_aliases.remove(&*s) {
                panic!("Type alias for '{}' was not expected", s);
            }
        }

        if let Some(remaining) = expected_aliases.into_iter().next() {
            panic!("Type '{}' not present in alias map", remaining);
        }
    }

    #[test]
    fn parse_default() {
        let item = syn::parse_quote! {
            #[telety(crate)]
            trait Tr {
                #[meta(default {
                    let a: A = A::default();
                    let b: b::B = b::B::new();
                })]
                fn f(i: i32) -> u32;
            }
        };

        // We aren't in a proc macro context, but we need something set here so
        // crateifying doesn't panic
        unsafe {
            std::env::set_var("CARGO_CRATE_NAME", "mycrate");
        }
        let telety = telety::Telety::new(&item).unwrap();
        let map = crate::telety_util::make_alias_map(&telety).unwrap();
        expect_aliases(&map, &["A", "b :: B"]);
    }
}
