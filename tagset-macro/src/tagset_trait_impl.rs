use std::collections::HashMap;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, TokenStreamExt, quote};
use syn::{parse_quote, parse_quote_spanned, parse2, spanned::Spanned};

use crate::parsing;

fn set_span_recursive(input: TokenStream, span: Span) -> TokenStream {
    let mut output = TokenStream::new();
    for mut tt in input {
        tt.set_span(span);

        if let proc_macro2::TokenTree::Group(group) = &mut tt {
            let group_stream = set_span_recursive(group.stream(), span);
            *group = proc_macro2::Group::new(group.delimiter(), group_stream);
        }
        output.append(tt);
    }
    output
}

pub(crate) fn tagset_trait_impl(input: TokenStream) -> syn::Result<TokenStream> {
    let parsing::TagsetTraitImplInput {
        struct_type,
        trait_type,
        mut generics,
        where_clause,
        mappings,
        telety_item,
        impl_item_overrides,
        ..
    } = parse2(input)?;

    generics.where_clause = where_clause;

    let item = telety_item.into_item();

    let telety = telety::Telety::new(&item)?;
    let alias_map = telety.alias_map();
    let mut telety_visitor = alias_map.visitor();
    // TODO  this needs double-checking, converting Self causes problems in function signatures
    // (e.g. `&self` gets translated to `&self: &Self` in syn, and the receiver can't be changed from Self)
    // but can leaving Self ever cause problems inside trait impls?
    telety_visitor.set_apply_associated_types(false);

    let syn::Item::Trait(item_trait) = &item else {
        unreachable!()
    };

    let mut meta_bounds = None;
    for attr in &item_trait.attrs {
        if let Some(tagset_meta) = parsing::TagsetMetaNone::from_meta(&attr.meta)? {
            match tagset_meta {
                parsing::TagsetMeta::Default(tagset_default) => {
                    return Err(syn::Error::new(
                        tagset_default.span(),
                        "Unexpected tagset_meta attribute",
                    ));
                }
                parsing::TagsetMeta::Bounds(tagset_bounds) => {
                    meta_bounds = Some(tagset_bounds.predicates)
                }
            }
        }
    }

    if let Some(meta_bounds) = meta_bounds {
        for meta_bound in meta_bounds {
            let predicates = match parsing::extended_bounds_predicate_split(meta_bound.clone())? {
                parsing::PredicateType::Closed(where_predicate) => vec![where_predicate],
                parsing::PredicateType::Open(open_predicate) => mappings
                    .element
                    .iter()
                    .map(|m| open_predicate.apply(&m.ty))
                    .collect::<Result<_, _>>()?,
            };

            for predicate in predicates {
                generics.make_where_clause().predicates.push(predicate);
            }
        }
    } else {
        // default bounds
        for mapping in &mappings.element {
            let ty = &mapping.ty;
            generics
                .make_where_clause()
                .predicates
                .push(parse_quote!(#ty: #trait_type));
        }
    }

    let mut trait_item_overrides_map = HashMap::new();
    for impl_item_override in impl_item_overrides.element {
        let trait_override_ident = match &impl_item_override {
            syn::ImplItem::Const(trait_item_const) => trait_item_const.ident.clone(),
            syn::ImplItem::Fn(trait_item_fn) => trait_item_fn.sig.ident.clone(),
            syn::ImplItem::Type(trait_item_type) => trait_item_type.ident.clone(),
            _ => {
                return Err(syn::Error::new(
                    impl_item_override.span(),
                    "impl item type is not supported",
                ));
            }
        };

        trait_item_overrides_map.insert(trait_override_ident, impl_item_override);
    }

    let (impl_generics, _type_generics, where_clause) = generics.split_for_impl();

    let mut new_trait_items = vec![];

    for trait_item in &item_trait.items {
        let span = trait_item.span();

        fn get_default_attr<Df: parsing::TagsetDefaultKind>(
            attrs: &Vec<syn::Attribute>,
        ) -> syn::Result<Option<parsing::TagsetDefault<Df>>> {
            let mut tagset_default = None;
            for attr in attrs {
                if let Some(parsing::TagsetMeta::Default(td)) =
                    parsing::TagsetMeta::from_meta(&attr.meta)?
                {
                    if tagset_default.replace(td).is_some() {
                        return Err(syn::Error::new(attr.span(), "Multiple default attributes"));
                    }
                }
            }
            Ok(tagset_default)
        }

        let new_trait_item = match trait_item {
            syn::TraitItem::Fn(trait_item_fn) => {
                let tagset_default = get_default_attr::<parsing::FnBody>(&trait_item_fn.attrs)?;

                if let Some(trait_item_override) =
                    trait_item_overrides_map.remove(&trait_item_fn.sig.ident)
                {
                    // An override provided at the tagset site
                    let syn::ImplItem::Fn(mut fn_override) = trait_item_override else {
                        return Err(syn::Error::new(
                            trait_item_override.span(),
                            "Expected a fn override",
                        ));
                    };

                    // TODO signature shortcutting is currently all-or-nothing:
                    // ```
                    // fn my_fn(...) {
                    //   do_it(my_value)
                    // }
                    // ```
                    // becomes, e.g.:
                    // ```
                    // fn my_fn(&self, my_value: i32) -> i32 {
                    //   do_it(my_value)
                    // }
                    // ```
                    if fn_override.sig.variadic.is_some() {
                        let span = fn_override.span();
                        let mut direct = crate::direct::TagsetDirect::new();
                        let mut sig = trait_item_fn.sig.clone();
                        directed_visit::visit_mut(&mut direct, &mut telety_visitor, &mut sig);
                        direct.into_result()?;
                        fn_override.sig =
                            syn::parse2(set_span_recursive(sig.to_token_stream(), span))
                                .expect("Only spans should be modified");
                    }

                    Some(syn::ImplItem::Fn(fn_override))
                } else if let Some(tagset_default) = tagset_default {
                    // An override provided at the trait site via tagset_meta
                    let sig = &trait_item_fn.sig;

                    let mut fn_impl = parse_quote_spanned! { span =>
                        #sig {
                            #tagset_default
                        }
                    };

                    let mut direct = crate::direct::TagsetDirect::new();
                    directed_visit::visit_mut(&mut direct, &mut telety_visitor, &mut fn_impl);
                    direct.into_result()?;

                    Some(fn_impl)
                } else if trait_item_fn.default.is_some() {
                    // Standard default impl
                    None
                } else {
                    // The default implementation of delegating the call to each variant
                    let mut sig = trait_item_fn.sig.clone();
                    let mut direct = crate::direct::TagsetDirect::new();
                    directed_visit::visit_mut(&mut direct, &mut telety_visitor, &mut sig);
                    direct.into_result()?;
                    let fn_ident = &trait_item_fn.sig.ident;

                    let field_ident: syn::Ident = parse_quote!(__field);

                    let mut args = syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::new();
                    let mut arg_index: usize = 0;

                    for arg in &mut sig.inputs {
                        let arg_ident = match arg {
                            syn::FnArg::Receiver(_receiver) => parse_quote!(#field_ident),
                            syn::FnArg::Typed(pat_type) => {
                                let arg_ident = quote::format_ident!("__arg{}", arg_index);
                                arg_index += 1;
                                // Overwrite the existing pattern with a simple ident (this is simpler,
                                // and some patterns are lossy, e.g. if we have `[a, b]: &[i32; 2]`
                                // we can't turn `a: &i32` and `b: &i32` back to a `&[i32; 2]`).
                                *pat_type.pat = parse_quote_spanned! { span =>
                                    #arg_ident
                                };
                                parse_quote_spanned! { span =>
                                    #arg_ident.into()
                                }
                            }
                        };

                        args.push(arg_ident);
                    }

                    Some(parse_quote_spanned! { span =>
                        #sig {
                            match_by_value!(self, #field_ident => <_ as #trait_type>::#fn_ident(#args))
                        }
                    })
                }
            }
            syn::TraitItem::Const(trait_item_const) => {
                if let Some(trait_item_override) =
                    trait_item_overrides_map.remove(&trait_item_const.ident)
                {
                    Some(trait_item_override)
                } else if trait_item_const.default.is_none() {
                    // We could copy the first entry's value like we do for types,
                    // but this is more likely to cause logic errors
                    return Err(syn::Error::new(
                        trait_item_const.span(),
                        "Trait const items must be specified",
                    ));
                } else {
                    None
                }
            }
            syn::TraitItem::Type(trait_item_type) => {
                let tagset_default = get_default_attr::<syn::Type>(&trait_item_type.attrs)?;

                if let Some(item_override) = trait_item_overrides_map.remove(&trait_item_type.ident)
                {
                    Some(item_override)
                } else if let Some(tagset_default) = tagset_default {
                    let syn::TraitItemType {
                        attrs: _attrs,
                        type_token,
                        ident,
                        generics,
                        colon_token,
                        bounds,
                        default: _default,
                        semi_token,
                    } = trait_item_type;

                    let mut ty_impl = parse_quote_spanned! { span =>
                        #type_token #ident #generics #colon_token #bounds = #tagset_default #semi_token
                    };

                    let mut direct = crate::direct::TagsetDirect::new();
                    directed_visit::visit_mut(&mut direct, &mut telety_visitor, &mut ty_impl);
                    direct.into_result()?;

                    Some(ty_impl)
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(new_trait_item) = new_trait_item {
            new_trait_items.push(new_trait_item);
        }
    }

    // There should be no remaining overrides
    let mut errors: Option<syn::Error> = None;
    for (ident, _override) in trait_item_overrides_map.into_iter() {
        let error = syn::Error::new(ident.span(), "Unknown trait item");
        match errors.as_mut() {
            Some(e) => e.combine(error),
            None => errors = Some(error),
        }
    }

    if let Some(errors) = errors {
        return Err(errors);
    }

    // TODO This is bare minimum required hygiene, but where ever
    // telety macro_rules mixes with proc macro output, we need
    // a unified span
    let call_site = Span::call_site();
    let trait_impl: syn::ItemImpl = parse_quote_spanned! { call_site =>
        impl #impl_generics #trait_type for #struct_type
        #where_clause {
            #(#new_trait_items)*
        }
    };

    Ok(quote! {
        #trait_impl
    })
}
