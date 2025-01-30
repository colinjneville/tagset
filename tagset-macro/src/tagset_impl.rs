use std::{borrow::Cow, mem};

use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_quote, parse2};

use crate::{
    parsing::{self, TagsetImplInput},
    telety_util, tokens,
};

pub(crate) fn tagset_impl(input: TokenStream) -> syn::Result<TokenStream> {
    let mut input: parsing::TagsetImplInput = parse2(input)?;

    // TODO
    let crate_path = quote!(::tagset);
    let private_module = tokens::private_module();

    let telety_item = input.telety_item.element.take();
    if let Some(telety_item) = telety_item {
        let mut telety_item = telety_item.into_item();

        input.next_discriminant.element.get_or_insert_default();

        let mut include_input =
            parsing::TagsetImplInput::from_telety(input.macro_options.clone(), &mut telety_item)?;

        let telety = telety::Telety::new(&telety_item)?;
        let sub_map = telety_util::make_alias_map(&telety)?;

        let generics = match telety.item() {
            syn::Item::Enum(item_enum) => &item_enum.generics,
            syn::Item::Struct(item_struct) => &item_struct.generics,
            syn::Item::Union(item_union) => &item_union.generics,
            _ => unreachable!(),
        };

        input
            .context
            .element
            .last_mut()
            .expect("a telety item must have context")
            .set_params(generics.clone());

        let mut alias_visitor = sub_map.visitor();
        let mut generics_visitor = input.visitor()?;

        fn get_offset(
            input0: &parsing::TagsetImplInput,
            input1: &parsing::TagsetImplInput,
        ) -> syn::Result<i128> {
            let index0 = input0.next_discriminant.element.as_ref().unwrap().value;
            let index1 = input1
                .next_discriminant
                .element
                .as_ref()
                .map(Cow::Borrowed)
                .unwrap_or_default()
                .value;

            Ok(index0 - index1)
        }

        // Translate the arguments to use global telety aliases and local discriminant
        let include_attrs = mem::take(&mut include_input.remaining_attrs.element);
        let mut added_attrs = vec![];
        for mut attr in include_attrs.into_iter().rev() {
            match &mut attr {
                // Ignored in include
                parsing::Attr::Impl(_attr_impl) => continue,
                parsing::Attr::Derive(_attr_derive) => continue,

                // Not used directly, but the index is needed to determine offsets
                parsing::Attr::Index(attr_index) => {
                    attr_index.apply(&mut include_input)?;
                    continue;
                }

                // Adjust offset
                parsing::Attr::Deprecated(attr_deprecated) => {
                    let offset = get_offset(&input, &include_input)?;
                    attr_deprecated.offset(offset)?;
                }
                parsing::Attr::Reserved(attr_reserved) => {
                    let offset = get_offset(&input, &include_input)?;
                    attr_reserved.offset(offset)?;
                }

                // No additional adjustment required
                parsing::Attr::Include(_attr_include) => {}
                parsing::Attr::Variant(_attr_variant) => {}

                // Invalid
                parsing::Attr::Pop(_attr_pop) => unreachable!("pop should not appear in include"),
            }

            // Transform the types to aliases
            let mut direct = crate::direct::TagsetDirect::new();
            directed_visit::visit_mut(&mut direct, &mut alias_visitor, &mut attr);
            direct.take_result()?;

            if let Some(generics_visitor) = &mut generics_visitor {
                directed_visit::visit_mut(&mut direct, generics_visitor, &mut attr);
                direct.take_result()?;
            }

            added_attrs.push(attr);
        }

        for added_attr in added_attrs.into_iter().rev() {
            input.remaining_attrs.element.push(added_attr);
        }
    }

    while let Some(pair) = input.remaining_attrs.element.pop() {
        let attr = pair.into_value();

        if let Some(command) = attr.apply(&mut input)? {
            // Attribute processing will continue in the next macro invocation
            return Ok(command);
        }
    }

    // All attributes have been processed, create the enum and trait impls
    let TagsetImplInput {
        macro_options,
        struct_ident,
        mut generics,
        where_clause,
        discriminant_type,
        derives,
        trait_impls,
        // context,
        mappings,
        ..
    } = input;
    generics.where_clause = where_clause;
    let derives = derives.element;

    let (_impl_generics, type_generics, _where_clause) = generics.split_for_impl();
    let struct_type: syn::TypePath = parse_quote!(#struct_ident #type_generics);

    let discriminant_type = discriminant_type
        .element
        .unwrap_or_else(|| parse_quote!(u32));
    let tag_set_type_ident = {
        let mut string = discriminant_type.to_string();
        string.get_mut(0..1).unwrap().make_ascii_uppercase();
        quote::format_ident!("TagSetType{string}")
    };

    let mut commands = vec![];
    for trait_impl in trait_impls.element {
        let parsing::TagsetTraitImpl {
            trait_type,
            generics_brace: _generics_brace,
            generics: mut new_generics,
            where_clause: mut new_where_clause,
            open_predicates,
            overrides,
        } = trait_impl;

        for param in &generics.params {
            new_generics.params.push(param.clone());
        }

        if let Some(where_clause) = &generics.where_clause {
            for predicate in &where_clause.predicates {
                new_where_clause
                    .get_or_insert(syn::WhereClause {
                        where_token: Default::default(),
                        predicates: Default::default(),
                    })
                    .predicates
                    .push(predicate.clone());
            }
        }

        for open_predicate in &open_predicates.element {
            for mapping in &mappings.element {
                new_where_clause
                    .get_or_insert(syn::WhereClause {
                        where_token: Default::default(),
                        predicates: Default::default(),
                    })
                    .predicates
                    .push(open_predicate.apply(&mapping.ty)?);
            }
        }

        new_generics.where_clause = new_where_clause;

        let trait_impl_input = parsing::TagsetTraitImplInput::new(
            macro_options.clone(),
            struct_type.clone(),
            new_generics,
            trait_type.element.clone(),
            overrides,
            mappings.element.clone(),
        );

        let mut path = trait_type.element.clone();
        if let Some(segment) = path.segments.last_mut() {
            segment.arguments = syn::PathArguments::None;
        }

        let command = telety::v1::TY.apply(
            path,
            parsing::TeletyItem::needle(),
            quote! {
                ::tagset::__private::tagset_trait_impl! {
                    #trait_impl_input
                }
            },
        );

        commands.push(command);
    }

    let enum_ident = tokens::impl_enum_ident(&struct_ident);
    let enum_type = tokens::impl_enum_type(&struct_type);

    let extra_lifetime: syn::Lifetime = parse_quote!('_ref);
    let mut extra_lifetime_generics = generics.clone();
    extra_lifetime_generics.params.push(parse_quote!(#extra_lifetime));

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let (extra_lifetime_impl_generics, _extra_lifetime_type_generics, _extra_lifetime_where_clause) = extra_lifetime_generics.split_for_impl();

    let mut match_value_macro = TokenStream::new();
    let mut match_discriminant_macro = TokenStream::new();

    let mut arms = vec![];
    let mut into_impls = vec![];
    let mut subtrait_impls = vec![];

    for mapping in mappings.element {
        let parsing::Mapping { discriminant, ty } = &mapping;

        let variant_ident = mapping.variant_ident()?;

        match_value_macro = quote! {
            #match_value_macro
            #enum_ident::#variant_ident($field) => { $ex }
        };

        // Allow #ty to be used in expression positions
        let turbo_ty = if let syn::Type::Path(ty_path) = ty {
            let mut ty_path = ty_path.clone();
            for segment in &mut ty_path.path.segments {
                if let syn::PathArguments::AngleBracketed(args) = &mut segment.arguments {
                    args.colon2_token = Some(Default::default());
                }
            }
            Cow::Owned(syn::Type::Path(ty_path))
        } else {
            Cow::Borrowed(ty)
        };

        match_discriminant_macro = quote! {
            #match_discriminant_macro
            #discriminant => {
                #private_module::telety::__private::find_and_replace! {
                    $ty,
                    (#turbo_ty),
                    $ex
                }
            }
        };

        arms.push(mapping.to_variant()?);

        let variant_ident = mapping.variant_ident()?;
        into_impls.push(quote! {
            impl #impl_generics #private_module::From<#ty> for #struct_ident #type_generics
            #where_clause {
                fn from(value: #ty) -> Self {
                    Self(#enum_type::#variant_ident(value), #private_module::PhantomData)
                }
            }
        });

        into_impls.push(quote! {
            impl #impl_generics #private_module::TryFrom<#struct_ident #type_generics> for #ty
            #where_clause {
                type Error = #struct_ident #type_generics;

                fn try_from(value: #struct_ident #type_generics) -> Result<Self, Self::Error> {
                    if let #enum_ident::#variant_ident(value) = value.0 {
                        Ok(value)
                    } else {
                        Err(value)
                    }
                }
            }
        });

        into_impls.push(quote! {
            impl #extra_lifetime_impl_generics #private_module::TryFrom<&#extra_lifetime #struct_ident #type_generics> for &#extra_lifetime #ty
            #where_clause {
                type Error = ();

                fn try_from(value: &#extra_lifetime #struct_ident #type_generics) -> Result<Self, Self::Error> {
                    if let #enum_ident::#variant_ident(value) = &value.0 {
                        Ok(value)
                    } else {
                        Err(())
                    }
                }
            }
        });

        into_impls.push(quote! {
            impl #extra_lifetime_impl_generics #private_module::TryFrom<&#extra_lifetime mut #struct_ident #type_generics> for &#extra_lifetime mut #ty
            #where_clause {
                type Error = ();

                fn try_from(value: &#extra_lifetime mut #struct_ident #type_generics) -> Result<Self, Self::Error> {
                    if let #enum_ident::#variant_ident(value) = &mut value.0 {
                        Ok(value)
                    } else {
                        Err(())
                    }
                }
            }
        });

        subtrait_impls.push(quote! {
            impl #impl_generics #crate_path::TagSetDiscriminant<#ty> for #struct_ident #type_generics
            #where_clause {
                const DISCRIMINANT: Self::Repr = #discriminant;
            }

            impl #impl_generics #crate_path::#tag_set_type_ident<#discriminant> for #struct_ident #type_generics
            #where_clause {
                type Type = #ty;
            }
        });
    }

    let match_value_macro: syn::Stmt = parse_quote! {
        #[allow(unused_macros)]
        macro_rules! match_by_value {
            ($value:expr, $field:ident => $ex:expr) => {
                {
                    match #private_module::IntoImpl::into_impl($value) {
                        #match_value_macro
                    }
                }
            };
        }
    };

    let match_discriminant_macro: syn::Stmt = parse_quote! {
        #[allow(unused_macros)]
        macro_rules! match_by_discriminant {
            ($discriminant:expr, $ty:ident => $ex:expr $(, $($ex_invalid:expr)?)?) => {
                #[allow(unreachable_patterns)]
                match $discriminant {
                    #match_discriminant_macro
                    $($(_ => { $ex_invalid })?)?
                    _ => { unreachable!("Invalid discriminant") }
                }
            };
        }
    };

    // TODO should be private, but is leaked by the IntoImpl trait
    let vis = &input.struct_vis;

    Ok(quote! {
        #[doc(hidden)]
        #[repr(#discriminant_type)]
        #[derive(#derives)]
        #vis enum #enum_ident #impl_generics
        #where_clause
        {
            #(#arms),*
        }

        const _: () = {
            #match_value_macro
            #match_discriminant_macro

            impl #impl_generics #crate_path::TagSet for #struct_ident #type_generics
            #where_clause {
                type Repr = #discriminant_type;

                fn discriminant(&self) -> #discriminant_type {
                    let p = #private_module::from_ref(&self.0) as *const #discriminant_type;
                    unsafe { *p }
                }
            }

            #(#subtrait_impls)*

            #(#into_impls)*

            #(#commands)*
        };
    })
}
