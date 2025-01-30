use std::hash::{Hash as _, Hasher};

use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{parse_quote, spanned::Spanned};

use crate::{
    parsing::{self, MacroOptions},
    tokens,
};

pub(crate) fn tagset(
    attr: TokenStream,
    item: TokenStream,
    options: MacroOptions,
) -> syn::Result<TokenStream> {
    if attr.is_empty() {
        Ok(item)
    } else {
        let mut hasher = std::hash::DefaultHasher::new();
        // TODO This depends on details of the Span being exposed in the Debug formatting.
        // Once Span::file stabilizes (hopefully) soon, that should be enough to avoid
        // all non-contrived collisions.
        // Once proc_macro2 is updated to stabilize the Span methods added in 1.88,
        // this conversion can be removed
        let span: proc_macro::Span = item.span().unwrap();
        span.file().hash(&mut hasher);
        span.line().hash(&mut hasher);
        span.column().hash(&mut hasher);
        // format!("{:?}", item.span()).hash(&mut hasher);
        // item.to_string().hash(&mut hasher);
        let hash = hasher.finish();

        let mut item: syn::Item = syn::parse2(item)?;

        let input = parsing::TagsetImplInput::from_item(options, attr, &mut item)?;
        // Scan attributes for derives so we can output the public struct here
        // TODO we should probably just move struct generation to final output
        let mut derives = vec![];
        for attr in &input.remaining_attrs.element {
            if let parsing::Attr::Derive(attr_derive) = attr {
                attr_derive.push_derives(&mut derives)?;
            }
        }

        let ident = &input.struct_ident;

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

        let syn::Item::Struct(mut item) = item else {
            unreachable!()
        };

        let mut phantom_data_params = syn::punctuated::Punctuated::<_, syn::Token![,]>::new();
        for type_param in item.generics.type_params() {
            phantom_data_params.push(&type_param.ident);
        }

        let extra_lifetime: syn::Lifetime = parse_quote!('__a);
        let mut extra_lifetime_generics = item.generics.clone();
        extra_lifetime_generics
            .params
            .push(syn::GenericParam::Lifetime(syn::LifetimeParam::new(
                extra_lifetime.clone(),
            )));

        // TODO strip unused generics for internal enum
        let (impl_generics, type_generics, where_clause) = item.generics.split_for_impl();
        let (
            extra_lifetime_impl_generics,
            _extra_lifetime_type_generics,
            extra_lifetime_where_clause,
        ) = extra_lifetime_generics.split_for_impl();

        let private_path = tokens::private_module();
        let impl_enum_ident = tokens::impl_enum_ident(ident);

        item.fields = syn::Fields::Unnamed(parse_quote! {
            (
                #impl_enum_ident #type_generics,
                #private_path::PhantomData<(#phantom_data_params)>,
            )
        });

        item.semi_token = None;

        let private_module = tokens::private_module();

        Ok(quote! {
            #[derive(#(#derives),*)]
            #item

            ::tagset::__private::tagset_impl! {
                #input
            }

            impl #impl_generics #private_module::IntoImpl for #ident #type_generics
            #where_clause {
                type Impl = #impl_enum_ident #type_generics;
                fn into_impl(self) -> Self::Impl { self.0 }
            }

            impl #extra_lifetime_impl_generics #private_module::IntoImpl for &#extra_lifetime #ident #type_generics
            #extra_lifetime_where_clause {
                type Impl = &#extra_lifetime #impl_enum_ident #type_generics;
                fn into_impl(self) -> Self::Impl { &self.0 }
            }

            impl #extra_lifetime_impl_generics #private_module::IntoImpl for &#extra_lifetime mut #ident #type_generics
            #extra_lifetime_where_clause {
                type Impl = &#extra_lifetime mut #impl_enum_ident #type_generics;
                fn into_impl(self) -> Self::Impl { &mut self.0 }
            }

            #telety_module
        })
    }
}
