use std::mem;

use directed_visit::{Director, Visitor};
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{parse_quote, spanned::Spanned as _};

use crate::direct;

type CommaList<T> = syn::punctuated::Punctuated<T, syn::Token![,]>;

mod kw {
    syn::custom_keyword!(reserved);
    syn::custom_keyword!(deprecated);
    syn::custom_keyword!(include);
    syn::custom_keyword!(pop);
    syn::custom_keyword!(derive);
    // TODO rename?
    syn::custom_keyword!(index);
}

#[derive(Clone)]
pub(crate) struct Discriminant {
    pub(crate) span: Span,
    pub(crate) value: i128,
    pub(crate) suffix: Option<String>,
}

impl Default for Discriminant {
    fn default() -> Self {
        Self {
            span: Span::call_site(),
            value: 0,
            suffix: None,
        }
    }
}

impl quote::ToTokens for Discriminant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            span,
            value,
            suffix,
        } = self;

        let suffix = suffix.as_deref().unwrap_or("");
        syn::LitInt::new(&format!("{value}{suffix}"), *span).to_tokens(tokens);
    }
}

impl syn::parse::Parse for Discriminant {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let neg: Option<syn::Token![-]> = input.parse()?;
        let lit: syn::LitInt = input.parse()?;
        let suffix = if lit.suffix().is_empty() {
            None
        } else {
            Some(lit.suffix().to_string())
        };

        let value: i128 = lit.base10_parse()?;
        let value = if neg.is_some() { -value } else { value };

        Ok(Self {
            span: lit.span(),
            value,
            suffix,
        })
    }
}

#[derive(Default)]
pub(crate) struct CommaElement<T> {
    pub element: T,
    comma: syn::token::Comma,
}

impl<T> CommaElement<T> {
    fn new(element: T) -> Self {
        Self {
            element,
            comma: Default::default(),
        }
    }
}

impl<T: syn::parse::Parse> CommaElement<Option<T>> {
    fn parse_option(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let element = if input.peek(syn::Token![,]) {
            None
        } else {
            Some(input.parse()?)
        };

        Ok(Self {
            element,
            comma: input.parse()?,
        })
    }
}

impl<T: quote::ToTokens> quote::ToTokens for CommaElement<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { element, comma } = self;

        element.to_tokens(tokens);
        comma.to_tokens(tokens);
    }
}

impl<T: syn::parse::Parse> syn::parse::Parse for CommaElement<T> {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            element: input.parse()?,
            comma: input.parse()?,
        })
    }
}

#[derive(Clone, Default)]
pub(crate) struct Parened<T> {
    paren: syn::token::Paren,
    pub element: T,
}

impl<T: syn::parse::Parse> Parened<CommaList<T>> {
    fn parse_terminated(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Self {
            paren: syn::parenthesized!(content in input),
            element: CommaList::parse_terminated(&content)?,
        })
    }
}

impl<T: syn::parse::Parse> syn::parse::Parse for Parened<T> {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Self {
            paren: syn::parenthesized!(content in input),
            element: content.parse()?,
        })
    }
}

impl<T: quote::ToTokens> quote::ToTokens for Parened<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { paren, element } = self;

        paren.surround(tokens, |tokens| {
            element.to_tokens(tokens);
        });
    }
}

#[derive(Clone, Default)]
pub(crate) struct Braced<T> {
    brace: syn::token::Brace,
    pub element: T,
}

impl<T> Braced<T> {
    fn new(element: T) -> Self {
        Self {
            brace: Default::default(),
            element,
        }
    }
}

impl<T: quote::ToTokens> quote::ToTokens for Braced<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { brace, element } = self;
        brace.surround(tokens, |tokens| {
            element.to_tokens(tokens);
        });
    }
}

impl<T> Braced<Vec<T>> {
    fn to_tokens(&self, tokens: &mut TokenStream)
    where
        T: quote::ToTokens,
    {
        let Self { brace, element } = self;

        brace.surround(tokens, |tokens| {
            for item in element {
                item.to_tokens(tokens);
            }
        });
    }

    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self>
    where
        T: syn::parse::Parse,
    {
        let content;
        let brace = syn::braced!(content in input);
        let mut element = vec![];
        while !content.is_empty() {
            element.push(content.parse()?);
        }

        Ok(Self { brace, element })
    }
}

impl<T: syn::parse::Parse> syn::parse::Parse for Braced<T> {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Self {
            brace: syn::braced!(content in input),
            element: content.parse()?,
        })
    }
}

impl<T: syn::parse::Parse> Braced<CommaList<T>> {
    fn new_vec(v: Vec<T>) -> Self {
        let mut element = syn::punctuated::Punctuated::new();
        for i in v {
            element.push(i);
        }

        Self {
            brace: Default::default(),
            element,
        }
    }

    fn parse_terminated(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Self {
            brace: syn::braced!(content in input),
            element: CommaList::parse_terminated(&content)?,
        })
    }
}

pub(crate) enum PredicateType {
    Closed(syn::WherePredicate),
    Open(OpenPredicate),
}

pub(crate) struct OpenPredicate {
    generic_param: syn::Ident,
    predicate: syn::PredicateType,
}

impl OpenPredicate {
    fn new(generic_param: syn::Ident, predicate: syn::PredicateType) -> Self {
        Self {
            generic_param,
            predicate,
        }
    }

    pub(crate) fn apply(&self, substitution: &syn::Type) -> syn::Result<syn::WherePredicate> {
        let mut closed = self.predicate.clone();

        let mut director = direct::TagsetDirect::new();
        directed_visit::visit_mut(
            &mut director,
            &mut OpenPredicateVisitor {
                generic_param: &self.generic_param,
                substitution,
            },
            &mut closed,
        );
        director.into_result()?;

        Ok(syn::WherePredicate::Type(closed))
    }
}

impl quote::ToTokens for OpenPredicate {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            generic_param,
            predicate,
        } = self;

        generic_param.to_tokens(tokens);
        predicate.to_tokens(tokens);
    }
}

impl syn::parse::Parse for OpenPredicate {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            generic_param: input.parse()?,
            predicate: {
                let syn::WherePredicate::Type(predicate) = input.parse()? else {
                    unreachable!("Expected a type predicate")
                };
                predicate
            },
        })
    }
}

struct OpenPredicateVisitor<'p, 's> {
    generic_param: &'p syn::Ident,
    substitution: &'s syn::Type,
}

impl<'p, 's> directed_visit::syn::visit::FullMut for OpenPredicateVisitor<'p, 's> {
    fn visit_type_mut<D>(visitor: Visitor<'_, D, Self>, node: &mut syn::Type)
    where
        D: directed_visit::DirectMut<Self, syn::Type> + ?Sized,
    {
        if let syn::Type::Path(type_path) = node {
            if type_path.qself.is_none() {
                if let Some(first_segment) = type_path.path.segments.first() {
                    if &first_segment.ident == visitor.generic_param {
                        let substitution = visitor.substitution;
                        let mut iter = type_path.path.segments.iter().peekable();
                        // Skip the first segment as we are replacing it
                        iter.next();

                        let ty = if iter.peek().is_some() {
                            parse_quote!(<#substitution>#(::#iter)*)
                        } else {
                            parse_quote!(#substitution)
                        };
                        *node = ty;

                        return;
                    }
                }
            }
        }

        Visitor::visit_mut(visitor, node);
    }
}

#[derive(Clone)]
pub(crate) struct MacroOptions {
    pub(crate) use_serde: syn::LitBool,
}

impl MacroOptions {
    pub fn new(span: Span, use_serde: bool) -> Self {
        Self {
            use_serde: syn::LitBool::new(use_serde, span),
        }
    }
}

impl syn::parse::Parse for MacroOptions {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            use_serde: input.parse()?,
        })
    }
}

impl quote::ToTokens for MacroOptions {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Self { use_serde } = self;

        use_serde.to_tokens(tokens);
    }
}

#[allow(clippy::large_enum_variant)]
pub(crate) enum TeletyItem {
    Item(syn::Item),
    Needle,
}

impl TeletyItem {
    pub(crate) fn into_item(self) -> syn::Item {
        let Self::Item(item) = self else {
            panic!("telety needle was not replaced")
        };
        item
    }

    pub(crate) fn needle() -> proc_macro2::Ident {
        proc_macro2::Ident::new("__needle", Span::call_site())
    }
}

impl syn::parse::Parse for TeletyItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self::Item(syn::Item::parse(input)?))
    }
}

impl quote::ToTokens for TeletyItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TeletyItem::Item(item) => item.to_tokens(tokens),
            TeletyItem::Needle => Self::needle().to_tokens(tokens),
        }
    }
}

#[derive(Clone)]
pub(crate) struct Mapping {
    pub discriminant: Discriminant,
    pub ty: syn::Type,
}

impl Mapping {
    pub(crate) fn to_variant(&self) -> syn::Result<syn::Variant> {
        let Self { discriminant, ty } = self;

        let ident = self.variant_ident()?;

        Ok(parse_quote!(
            #ident(#ty) = #discriminant
        ))
    }

    pub(crate) fn variant_ident(&self) -> syn::Result<syn::Ident> {
        let value = self.discriminant.value;
        let is_negative = if value.is_negative() { "N" } else { "" };

        let abs_val = value.abs();
        Ok(format_ident!("Entry{is_negative}{abs_val}"))
    }
}

impl syn::parse::Parse for Mapping {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            discriminant: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl quote::ToTokens for Mapping {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { discriminant, ty } = self;

        discriminant.to_tokens(tokens);
        ty.to_tokens(tokens);
    }
}

pub(crate) struct TagsetTraitImplInput {
    pub(crate) macro_options: MacroOptions,
    pub(crate) struct_type: syn::TypePath,

    generics_brace: syn::token::Brace,
    // {
    pub(crate) generics: syn::Generics,
    pub(crate) where_clause: Option<syn::WhereClause>,
    // }
    pub(crate) mappings: Braced<CommaList<Mapping>>,

    pub(crate) trait_type: syn::Path,

    pub(crate) impl_item_overrides: Braced<Vec<syn::ImplItem>>,

    pub(crate) telety_item: TeletyItem,
}

impl TagsetTraitImplInput {
    pub(crate) fn new(
        macro_options: MacroOptions,
        struct_type: syn::TypePath,
        mut generics: syn::Generics,
        trait_type: syn::Path,
        overrides: Braced<Vec<syn::ImplItem>>,
        mappings: syn::punctuated::Punctuated<Mapping, syn::Token![,]>,
    ) -> Self {
        let where_clause = generics.where_clause.take();

        Self {
            macro_options,
            struct_type,
            generics_brace: Default::default(),
            generics,
            where_clause,
            mappings: Braced::new(mappings),
            trait_type,
            impl_item_overrides: overrides,
            telety_item: TeletyItem::Needle,
        }
    }
}

impl quote::ToTokens for TagsetTraitImplInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            macro_options,
            struct_type,
            generics_brace,
            generics,
            where_clause,
            mappings,
            trait_type,
            impl_item_overrides,
            telety_item,
        } = self;

        macro_options.to_tokens(tokens);

        struct_type.to_tokens(tokens);

        generics_brace.surround(tokens, |tokens| {
            generics.to_tokens(tokens);
            where_clause.to_tokens(tokens);
        });

        mappings.to_tokens(tokens);

        trait_type.to_tokens(tokens);

        impl_item_overrides.to_tokens(tokens);

        telety_item.to_tokens(tokens);
    }
}

impl syn::parse::Parse for TagsetTraitImplInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let macro_options = input.parse()?;
        let struct_type = input.parse()?;

        let generics_content;
        let generics_brace = syn::braced!(generics_content in input);
        // {
        let generics = generics_content.parse()?;
        let where_clause = extended_where_clause_parse(&generics_content)?;
        // }

        let mappings = Braced::parse_terminated(input)?;

        let trait_type = input.parse()?;

        let mut impl_item_overrides = vec![];
        let overrides_content;
        syn::braced!(overrides_content in input);
        while !overrides_content.is_empty() {
            impl_item_overrides.push(overrides_content.parse()?);
        }
        let impl_item_overrides = Braced::new(impl_item_overrides);

        let telety_item = input.parse()?;

        Ok(Self {
            macro_options,
            struct_type,
            generics_brace,
            generics,
            where_clause,
            mappings,
            trait_type,
            impl_item_overrides,
            telety_item,
        })
    }
}

pub(crate) struct TagsetContext {
    // ...and this is filled in post-telety jump
    generic_params: CommaElement<syn::Generics>,
    // This is filled in pre-telety jump...
    generic_args: Option<syn::AngleBracketedGenericArguments>,
}

impl TagsetContext {
    pub(crate) fn new(generic_args: syn::PathArguments) -> Self {
        let generic_args = match generic_args {
            syn::PathArguments::None => None,
            syn::PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                Some(angle_bracketed_generic_arguments)
            }
            syn::PathArguments::Parenthesized(_) => {
                unreachable!("expected angle bracketed generic arguments")
            }
        };

        Self {
            generic_params: Default::default(),
            generic_args,
        }
    }

    pub(crate) fn set_params(&mut self, generic_params: syn::Generics) {
        let old_generic_params =
            std::mem::replace(&mut self.generic_params.element, generic_params);
        debug_assert_eq!(old_generic_params, Default::default());
    }

    pub(crate) fn visitor(&self) -> syn::Result<telety::visitor::ApplyGenericArguments> {
        telety::visitor::ApplyGenericArguments::new(
            &self.generic_params.element,
            self.generic_args
                .as_ref()
                .map(|a| a.args.iter())
                .into_iter()
                .flatten(),
        )
    }
}

impl syn::parse::Parse for TagsetContext {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            generic_params: input.parse()?,
            generic_args: {
                if input.is_empty() {
                    None
                } else {
                    Some(input.parse()?)
                }
            },
        })
    }
}

impl quote::ToTokens for TagsetContext {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            generic_params,
            generic_args,
        } = self;

        generic_params.to_tokens(tokens);
        generic_args.to_tokens(tokens);
    }
}

pub(crate) struct TagsetTraitImpl {
    pub(crate) trait_type: CommaElement<syn::Path>,
    pub(crate) generics_brace: syn::token::Brace,
    // {
    pub(crate) generics: syn::Generics,
    pub(crate) where_clause: Option<syn::WhereClause>,
    // }
    pub(crate) open_predicates: Braced<CommaList<OpenPredicate>>,
    pub(crate) overrides: Braced<Vec<syn::ImplItem>>,
}

impl TagsetTraitImpl {
    fn new(
        trait_type: syn::Path,
        mut generics: syn::Generics,
        overrides: Vec<syn::ImplItem>,
    ) -> syn::Result<Self> {
        let mut where_clause = generics.where_clause.take();
        let open_predicates = Braced::new_vec(extended_where_clause_split(where_clause.as_mut())?);
        let overrides = Braced::new(overrides);

        Ok(Self {
            trait_type: CommaElement::new(trait_type),
            generics_brace: Default::default(),
            generics,
            where_clause,
            open_predicates,
            overrides,
        })
    }
}

impl quote::ToTokens for TagsetTraitImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            trait_type,
            generics_brace,
            generics,
            where_clause,
            open_predicates,
            overrides,
        } = self;

        trait_type.to_tokens(tokens);
        generics_brace.surround(tokens, |tokens| {
            generics.to_tokens(tokens);
            where_clause.to_tokens(tokens);
        });
        open_predicates.to_tokens(tokens);
        overrides.to_tokens(tokens);
    }
}

impl syn::parse::Parse for TagsetTraitImpl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let trait_type = input.parse()?;

        let generics_content;
        let generics_brace = syn::braced!(generics_content in input);
        let generics = generics_content.parse()?;
        let where_clause = generics_content.parse()?;

        let open_predicates = Braced::parse_terminated(input)?;
        let overrides = Braced::parse(input)?;

        Ok(Self {
            trait_type,
            generics_brace,
            generics,
            where_clause,
            open_predicates,
            overrides,
        })
    }
}

pub(crate) struct TagsetImplInput {
    pub macro_options: MacroOptions,
    pub struct_vis: syn::Visibility,
    pub struct_ident: syn::Ident,
    generics_brace: syn::token::Brace,
    // {
    pub generics: syn::Generics,
    pub where_clause: Option<syn::WhereClause>,
    // }
    pub discriminant_type: CommaElement<Option<syn::Ident>>,
    pub telety_item: CommaElement<Option<TeletyItem>>,
    pub next_discriminant: CommaElement<Option<Discriminant>>,

    pub derives: Braced<CommaList<syn::Path>>,
    pub trait_impls: Braced<Vec<TagsetTraitImpl>>,

    pub context: Braced<CommaList<TagsetContext>>,

    pub remaining_attrs: Braced<CommaList<Attr>>,

    pub mappings: Braced<CommaList<Mapping>>,
}

impl TagsetImplInput {
    pub(crate) fn from_telety(
        macro_options: MacroOptions,
        item: &mut syn::Item,
    ) -> syn::Result<Self> {
        Self::new(macro_options, None, item)
    }

    pub(crate) fn from_item(
        macro_options: MacroOptions,
        first_attr: TokenStream,
        item: &mut syn::Item,
    ) -> syn::Result<Self> {
        Self::new(macro_options, Some(first_attr), item)
    }

    fn new(
        macro_options: MacroOptions,
        first_attr: Option<TokenStream>,
        item: &mut syn::Item,
    ) -> syn::Result<Self> {
        let syn::Item::Struct(
            item @ syn::ItemStruct {
                fields: syn::Fields::Unit,
                ..
            },
        ) = item
        else {
            return Err(syn::Error::new(
                item.span(),
                "tagset must be applied to a unit struct (contents will be generated by the attribute)",
            ));
        };

        let mut generics = item.generics.clone();
        let where_clause = generics.where_clause.take();

        let mut remaining_attrs = CommaList::new();

        // TODO Hacky nonsense (telety path must retain all attrs, local path must remove tagset attrs)
        let retain = first_attr.is_none();
        let attrs = if retain {
            item.attrs.clone()
        } else {
            mem::take(&mut item.attrs)
        };

        for attr in attrs.into_iter().rev() {
            if attr.path().is_ident("tagset") {
                remaining_attrs.push(attr.parse_args()?);
            } else if !retain {
                item.attrs.push(attr);
            }
        }
        if !retain {
            item.attrs.reverse();
        }

        if let Some(first_attr) = first_attr {
            remaining_attrs.push(syn::parse2(first_attr)?);
        }

        let input = Self {
            macro_options,
            struct_vis: item.vis.clone(),
            struct_ident: item.ident.clone(),
            generics_brace: Default::default(),
            generics,
            where_clause,
            discriminant_type: Default::default(),
            telety_item: Default::default(),
            next_discriminant: Default::default(),
            derives: Default::default(),
            trait_impls: Default::default(),
            context: Default::default(),
            remaining_attrs: Braced::new(remaining_attrs),
            mappings: Default::default(),
        };

        Ok(input)
    }

    pub(crate) fn visitor(&self) -> syn::Result<Option<telety::visitor::ApplyGenericArguments>> {
        if let Some(context) = self.context.element.last() {
            Ok(Some(context.visitor()?))
        } else {
            Ok(None)
        }
    }
}

impl syn::parse::Parse for TagsetImplInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let macro_options = input.parse()?;
        let struct_vis = input.parse()?;
        let struct_ident = input.parse()?;

        let content;
        let generics_brace = syn::braced!(content in input);
        let generics = content.parse()?;
        let where_clause = content.parse()?;

        Ok(Self {
            macro_options,
            struct_vis,
            struct_ident,
            generics_brace,
            generics,
            where_clause,
            discriminant_type: CommaElement::parse_option(input)?,
            telety_item: CommaElement::parse_option(input)?,
            next_discriminant: CommaElement::parse_option(input)?,
            derives: Braced::parse_terminated(input)?,
            trait_impls: Braced::parse(input)?,
            context: Braced::parse_terminated(input)?,
            remaining_attrs: Braced::parse_terminated(input)?,
            mappings: Braced::parse_terminated(input)?,
        })
    }
}

impl quote::ToTokens for TagsetImplInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            macro_options,
            struct_vis,
            struct_ident,
            generics_brace,
            generics,
            where_clause,
            discriminant_type,
            telety_item,
            next_discriminant,
            derives,
            trait_impls,
            context,
            remaining_attrs,
            mappings,
        } = self;

        macro_options.to_tokens(tokens);
        struct_vis.to_tokens(tokens);
        struct_ident.to_tokens(tokens);

        generics_brace.surround(tokens, |ts| {
            generics.to_tokens(ts);
            where_clause.to_tokens(ts);
        });

        discriminant_type.to_tokens(tokens);
        telety_item.to_tokens(tokens);
        next_discriminant.to_tokens(tokens);
        derives.to_tokens(tokens);
        trait_impls.to_tokens(tokens);
        context.to_tokens(tokens);
        remaining_attrs.to_tokens(tokens);
        mappings.to_tokens(tokens);
    }
}

pub(crate) enum Attr {
    Impl(AttrImpl),
    Variant(AttrVariant),
    Deprecated(AttrDeprecated),
    Reserved(AttrReserved),
    Include(AttrInclude),
    Derive(AttrDerive),
    Index(AttrIndex),
    Pop(AttrPop),
}

impl Attr {
    pub(crate) fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<Option<TokenStream>> {
        match self {
            Self::Impl(attr_impl) => attr_impl.apply(input).map(|_| None),
            Self::Variant(attr_variant) => attr_variant.apply(input).map(|_| None),
            Self::Deprecated(attr_deprecated) => attr_deprecated.apply(input).map(|_| None),
            Self::Reserved(attr_reserved) => attr_reserved.apply(input).map(|_| None),
            Self::Include(attr_include) => attr_include.apply(input).map(Some),
            Self::Derive(attr_derive) => attr_derive.apply(input).map(|_| None),
            Self::Index(attr_index) => attr_index.apply(input).map(|_| None),
            Self::Pop(attr_pop) => attr_pop.apply(input).map(|_| None),
        }
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        // We currently only care about Types for generating a telety alias map
        match self {
            Self::Impl(attr_impl) => attr_impl.direct(director),
            Self::Variant(attr_variant) => attr_variant.direct(director),
            Self::Deprecated(attr_deprecated) => attr_deprecated.direct(director),
            Self::Reserved(attr_reserved) => attr_reserved.direct(director),
            Self::Include(attr_include) => attr_include.direct(director),
            Self::Derive(attr_derive) => attr_derive.direct(director),
            Self::Index(attr_index) => attr_index.direct(director),
            Self::Pop(attr_pop) => attr_pop.direct(director),
        }
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        // We currently only care about Types for generating a telety alias map
        match self {
            Self::Impl(attr_impl) => attr_impl.direct_mut(director),
            Self::Variant(attr_variant) => attr_variant.direct_mut(director),
            Self::Deprecated(attr_deprecated) => attr_deprecated.direct_mut(director),
            Self::Reserved(attr_reserved) => attr_reserved.direct_mut(director),
            Self::Include(attr_include) => attr_include.direct_mut(director),
            Self::Derive(attr_derive) => attr_derive.direct_mut(director),
            Self::Index(attr_index) => attr_index.direct_mut(director),
            Self::Pop(attr_pop) => attr_pop.direct_mut(director),
        }
    }
}

impl quote::ToTokens for Attr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Impl(tagset_impl) => tagset_impl.to_tokens(tokens),
            Self::Variant(tagset_variant) => tagset_variant.to_tokens(tokens),
            Self::Deprecated(tagset_deprecated) => tagset_deprecated.to_tokens(tokens),
            Self::Reserved(tagset_reserved) => tagset_reserved.to_tokens(tokens),
            Self::Include(tagset_include) => tagset_include.to_tokens(tokens),
            Self::Derive(tagset_derive) => tagset_derive.to_tokens(tokens),
            Self::Index(tagset_index) => tagset_index.to_tokens(tokens),
            Self::Pop(tagset_pop) => tagset_pop.to_tokens(tokens),
        }
    }
}

impl syn::parse::Parse for Attr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        let kind = if lookahead.peek(kw::deprecated) {
            Self::Deprecated(input.parse()?)
        } else if lookahead.peek(kw::reserved) {
            Self::Reserved(input.parse()?)
        } else if lookahead.peek(kw::include) {
            Self::Include(input.parse()?)
        } else if lookahead.peek(kw::pop) {
            Self::Pop(input.parse()?)
        } else if lookahead.peek(kw::derive) {
            Self::Derive(input.parse()?)
        } else if lookahead.peek(kw::index) {
            Self::Index(input.parse()?)
        } else if lookahead.peek(syn::Token![impl]) {
            Self::Impl(input.parse()?)
        } else {
            Self::Variant(input.parse()?)
        };

        Ok(kind)
    }
}

pub(crate) struct AttrIndex {
    index: kw::index,
    arg: Parened<ArgIndex>,
}

impl AttrIndex {
    pub(crate) fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<()> {
        if input.next_discriminant.element.is_none() {
            input.next_discriminant.element = Some(self.arg.element.start.clone());
        } else {
            return Err(syn::Error::new(
                self.span(),
                "index must appear before variants",
            ));
        }

        if let Some(suffix) = &self.arg.element.start.suffix {
            input.discriminant_type.element =
                Some(syn::Ident::new(suffix, self.arg.element.start.span()));
        }

        Ok(())
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        _director: &mut Director<'_, D, V>,
    ) {
        // No need to visit these args
        // self.args.visit(visitor);
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        _director: &mut Director<'_, D, V>,
    ) {
        // No need to visit these args
        // self.args.visit_mut(visitor);
    }
}

impl quote::ToTokens for AttrIndex {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { index, arg } = self;

        index.to_tokens(tokens);
        arg.to_tokens(tokens);
    }
}

impl syn::parse::Parse for AttrIndex {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            index: input.parse()?,
            arg: input.parse()?,
        })
    }
}

pub(crate) struct AttrDerive {
    derive: kw::derive,
    args: Parened<CommaList<ArgType>>,
}

impl AttrDerive {
    pub(crate) fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<()> {
        for arg in &self.args.element {
            input
                .derives
                .element
                .push(syn::parse2(arg.ty.to_token_stream())?);
        }

        Ok(())
    }

    pub(crate) fn push_derives<'attr>(
        &'attr self,
        derives: &mut Vec<&'attr syn::Type>,
    ) -> syn::Result<()> {
        for arg in &self.args.element {
            derives.push(&arg.ty);
        }

        Ok(())
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        _director: &mut Director<'_, D, V>,
    ) {
        // Intentionally a no-op for now so we don't make unnecessary (and invalid)
        // aliases for the derive macros
        // self.args.visit(visitor);
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        _director: &mut Director<'_, D, V>,
    ) {
        // self.args.visit_mut(visitor);
    }
}

impl quote::ToTokens for AttrDerive {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { derive, args } = self;

        derive.to_tokens(tokens);
        args.to_tokens(tokens);
    }
}

impl syn::parse::Parse for AttrDerive {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            derive: input.parse()?,
            args: Parened::parse_terminated(input)?,
        })
    }
}

pub(crate) struct ArgIndex {
    start: Discriminant,
}

impl quote::ToTokens for ArgIndex {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { start } = self;

        start.to_tokens(tokens);
    }
}

impl syn::parse::Parse for ArgIndex {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            start: input.parse()?,
        })
    }
}

#[derive(Clone)]
pub(crate) struct ArgRange {
    limits: syn::RangeLimits,
    end: Discriminant,
}

impl ArgRange {
    fn last_index_exclusive(&self) -> syn::Result<i128> {
        let mut i = self.end.value;
        if let syn::RangeLimits::Closed(_) = &self.limits {
            i += 1;
        }

        Ok(i)
    }
}

impl quote::ToTokens for ArgRange {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { limits, end } = self;

        limits.to_tokens(tokens);
        end.to_tokens(tokens);
    }
}

impl syn::parse::Parse for ArgRange {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            limits: input.parse()?,
            end: input.parse()?,
        })
    }
}

#[derive(Clone)]
pub(crate) struct ArgType {
    ty: syn::Type,
}

impl ArgType {
    pub fn type_path(&self) -> syn::Result<&syn::TypePath> {
        if let syn::Type::Path(type_path) = &self.ty {
            Ok(type_path)
        } else {
            Err(syn::Error::new(self.ty.span(), "Expected a type path"))
        }
    }

    pub fn path(&self) -> syn::Result<&syn::Path> {
        Ok(&self.type_path()?.path)
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        Director::direct(director, &self.ty);
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        Director::direct_mut(director, &mut self.ty);
    }
}

impl syn::parse::Parse for ArgType {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self { ty: input.parse()? })
    }
}

impl quote::ToTokens for ArgType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { ty } = self;

        ty.to_tokens(tokens);
    }
}

#[derive(Clone)]
pub(crate) struct AttrDeprecated {
    deprecated: kw::deprecated,
    arg: Option<Parened<ArgRange>>,
}

impl AttrDeprecated {
    fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<()> {
        let next_discriminant = input.next_discriminant.element.get_or_insert_default();

        let new_end = if let Some(arg) = &self.arg {
            arg.element.last_index_exclusive()?
        } else {
            next_discriminant.value + 1
        };

        next_discriminant.value = new_end;

        Ok(())
    }

    pub(crate) fn offset(&mut self, n: i128) -> syn::Result<()> {
        if let Some(arg) = &mut self.arg {
            arg.element.end.value += n;
        }
        Ok(())
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        _director: &mut Director<'_, D, V>,
    ) {
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        _director: &mut Director<'_, D, V>,
    ) {
    }
}

impl quote::ToTokens for AttrDeprecated {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { deprecated, arg } = self;

        deprecated.to_tokens(tokens);
        arg.to_tokens(tokens);
    }
}

impl syn::parse::Parse for AttrDeprecated {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let deprecated = input.parse()?;
        let arg = if input.peek(syn::token::Paren) {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Self { deprecated, arg })
    }
}

#[derive(Clone)]
pub(crate) struct AttrReserved {
    reserved: kw::reserved,
    arg: Option<Parened<ArgRange>>,
}

impl AttrReserved {
    pub(crate) fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<()> {
        let next_discriminant = input.next_discriminant.element.get_or_insert_default();

        let new_end = if let Some(arg) = &self.arg {
            arg.element.last_index_exclusive()?
        } else {
            next_discriminant.value + 1
        };

        next_discriminant.value = new_end;

        Ok(())
    }

    pub(crate) fn offset(&mut self, n: i128) -> syn::Result<()> {
        if let Some(arg) = &mut self.arg {
            arg.element.end.value += n;
        }
        Ok(())
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        _director: &mut Director<'_, D, V>,
    ) {
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        _director: &mut Director<'_, D, V>,
    ) {
    }
}

impl quote::ToTokens for AttrReserved {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { reserved, arg } = self;

        reserved.to_tokens(tokens);
        arg.to_tokens(tokens);
    }
}

impl syn::parse::Parse for AttrReserved {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let reserved = input.parse()?;
        let arg = if input.peek(syn::token::Paren) {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Self { reserved, arg })
    }
}

#[derive(Clone)]
pub(crate) struct AttrInclude {
    include: kw::include,
    arg: Parened<ArgType>,
}

impl AttrInclude {
    pub(crate) fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<TokenStream> {
        input.telety_item.element = Some(TeletyItem::Needle);

        input
            .remaining_attrs
            .element
            .push(Attr::Pop(AttrPop::new()));

        let mut ty_path = self.arg.element.path()?.clone();
        let args = if let Some(segment) = ty_path.segments.last_mut() {
            std::mem::take(&mut segment.arguments)
        } else {
            syn::PathArguments::None
        };

        input.context.element.push(TagsetContext::new(args));

        let command = telety::v1::TY
            .apply(
                ty_path,
                TeletyItem::needle(),
                quote! {
                    ::tagset::__private::tagset_impl! {
                        #input
                    }
                },
            )
            // .with_fallback(quote!(compile_error!("Types must be telety-enabled to be included")))
            .with_telety_path(parse_quote!(::tagset::__private::telety));

        Ok(command.to_token_stream())
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        let Self {
            include: _include,
            arg,
        } = self;

        arg.element.direct(director);
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        let Self {
            include: _include,
            arg,
        } = self;

        arg.element.direct_mut(director);
    }
}

impl quote::ToTokens for AttrInclude {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { include, arg } = self;

        include.to_tokens(tokens);
        arg.to_tokens(tokens);
    }
}

impl syn::parse::Parse for AttrInclude {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            include: input.parse()?,
            arg: input.parse()?,
        })
    }
}

#[derive(Clone)]
pub(crate) struct AttrPop {
    pop: kw::pop,
}

impl AttrPop {
    pub(crate) fn new() -> Self {
        Self {
            pop: Default::default(),
        }
    }

    pub(crate) fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<()> {
        input
            .context
            .element
            .pop()
            .expect("context stack should not be empty at a pop");
        Ok(())
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        _director: &mut Director<'_, D, V>,
    ) {
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        _director: &mut Director<'_, D, V>,
    ) {
    }
}

impl quote::ToTokens for AttrPop {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { pop } = self;

        pop.to_tokens(tokens);
    }
}

impl syn::parse::Parse for AttrPop {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            pop: input.parse()?,
        })
    }
}

#[derive(Clone)]
pub(crate) struct AttrImpl {
    // Internally represented as a regular ItemImpl so that visitors
    // can make sense of the generic scoping
    item_impl: syn::ItemImpl,
}

impl AttrImpl {
    fn trait_(&self) -> &syn::Path {
        &self.item_impl.trait_.as_ref().unwrap().1
    }

    pub(crate) fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<()> {
        let trait_impl = TagsetTraitImpl::new(
            self.trait_().clone(),
            self.item_impl.generics.clone(),
            self.item_impl.items.clone(),
        )?;
        input.trait_impls.element.push(trait_impl);
        Ok(())
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        Director::direct(director, &self.item_impl);
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        Director::direct_mut(director, &mut self.item_impl);
    }
}

impl quote::ToTokens for AttrImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let syn::ItemImpl {
            impl_token: impl_,
            generics: impl_generics,
            trait_,
            brace_token: brace_,
            items: overrides,
            ..
        } = &self.item_impl;
        let trait_ = &trait_.as_ref().unwrap().1;

        impl_.to_tokens(tokens);
        impl_generics.to_tokens(tokens);
        trait_.to_tokens(tokens);
        impl_generics.where_clause.to_tokens(tokens);

        if !overrides.is_empty() {
            brace_.surround(tokens, |tokens| {
                for override_ in overrides {
                    override_.to_tokens(tokens);
                }
            });
        }
    }
}

#[cfg(test)]
mod extended_where_test {
    use quote::ToTokens as _;
    use syn::parse::Parser as _;

    #[test]
    fn extended_where_parse() {
        let input = quote::quote! {
            where
                for<VAR> VAR: Clone,
                for<'a> i32: Clone,
        };

        let parsed = super::extended_where_clause_parse
            .parse2(input.clone())
            .unwrap();

        assert_eq!(input.to_string(), parsed.to_token_stream().to_string());
    }

    #[test]
    fn extended_where_split() {
        let input = quote::quote! {
            where
                for<VAR> VAR: Clone,
                for<'a> i32: Clone,
        };

        let mut parsed = super::extended_where_clause_parse
            .parse2(input.clone())
            .unwrap();

        let open_predicates = super::extended_where_clause_split(parsed.as_mut()).unwrap();

        assert_eq!(open_predicates.len(), 1);
        assert_eq!(
            open_predicates[0].to_token_stream().to_string(),
            quote::quote!(VAR VAR: Clone).to_string()
        );
        assert_eq!(
            parsed.to_token_stream().to_string(),
            quote::quote!(where for<'a> i32: Clone).to_string()
        );
    }

    #[test]
    fn open_predicate_apply() {
        fn apply(
            open_predicate: super::OpenPredicate,
            substitution: syn::Type,
            expected: proc_macro2::TokenStream,
        ) {
            let predicate = open_predicate.apply(&substitution).unwrap();
            assert_eq!(
                predicate.to_token_stream().to_string(),
                expected.to_string()
            );
        }
        apply(
            syn::parse_quote!(VAR VAR: Clone),
            syn::parse_quote!(i32),
            quote::quote!(i32: Clone),
        );
        apply(
            syn::parse_quote!(VAR VAR::Ty: Clone),
            syn::parse_quote!(i32),
            quote::quote!(<i32>::Ty: Clone),
        );
        apply(
            syn::parse_quote!(VAR <VAR>::Ty: Clone),
            syn::parse_quote!(i32),
            quote::quote!(<i32>::Ty: Clone),
        );
        apply(
            syn::parse_quote!(VAR VAR::Ty: Clone),
            syn::parse_quote!([i32; 0]),
            quote::quote!(<[i32; 0]>::Ty: Clone),
        );
        apply(
            syn::parse_quote!(VAR <VAR as MyTrait>::Ty: Clone),
            syn::parse_quote!([i32; 0]),
            quote::quote!(<[i32; 0] as MyTrait>::Ty: Clone),
        );
    }
}

fn extended_where_predicates_parse(
    input: syn::parse::ParseStream,
) -> syn::Result<syn::punctuated::Punctuated<syn::WherePredicate, syn::Token![,]>> {
    fn extended_where_predicate_parse(
        input: syn::parse::ParseStream,
    ) -> syn::Result<syn::WherePredicate> {
        let mut bound_lifetimes = None;

        if (!input.peek(syn::Lifetime) || !input.peek2(syn::Token![:]))
            && input.peek(syn::Token![for])
        {
            bound_lifetimes = Some(syn::BoundLifetimes {
                for_token: input.parse()?,
                lt_token: input.parse()?,
                lifetimes: {
                    let mut lifetimes = syn::punctuated::Punctuated::new();
                    while !input.peek(syn::Token![>]) {
                        // TODO This could be more restrictive
                        lifetimes.push_value(input.parse()?);
                        if input.peek(syn::Token![>]) {
                            break;
                        }
                        lifetimes.push_punct(input.parse()?);
                    }
                    lifetimes
                },
                gt_token: input.parse()?,
            });
        }

        let mut where_predicate = input.parse()?;
        if let syn::WherePredicate::Type(where_predicate_type) = &mut where_predicate {
            where_predicate_type.lifetimes = bound_lifetimes;
        }

        Ok(where_predicate)
    }

    let mut predicates = syn::punctuated::Punctuated::new();
    loop {
        if input.is_empty()
            || input.peek(syn::token::Brace)
            || input.peek(syn::Token![,])
            || input.peek(syn::Token![;])
            || input.peek(syn::Token![:]) && !input.peek(syn::Token![::])
            || input.peek(syn::Token![=])
        {
            break;
        }
        let value = extended_where_predicate_parse(input)?;
        predicates.push_value(value);
        if !input.peek(syn::Token![,]) {
            break;
        }
        let punct = input.parse()?;
        predicates.push_punct(punct);
    }
    Ok(predicates)
}

// Custom Generics parsing to allow HKTB-like open type bounds (e.g. `for<VAR> VAR: Clone`)
fn extended_where_clause_parse(
    input: syn::parse::ParseStream,
) -> syn::Result<Option<syn::WhereClause>> {
    if input.peek(syn::Token![where]) {
        Ok(Some(syn::WhereClause {
            where_token: input.parse()?,
            predicates: extended_where_predicates_parse(input)?,
        }))
    } else {
        Ok(None)
    }
}

pub(crate) fn extended_where_clause_split(
    where_clause: Option<&mut syn::WhereClause>,
) -> syn::Result<Vec<OpenPredicate>> {
    // We allow a special syntax of a single HKTB-like type parameter to apply a bound to all types in the tagset
    // `for<VAR> VAR: Clone`

    let mut open_predicates = vec![];
    if let Some(where_clause) = where_clause {
        for predicate in std::mem::take(&mut where_clause.predicates).into_iter() {
            match extended_bounds_predicate_split(predicate)? {
                PredicateType::Closed(where_predicate) => {
                    where_clause.predicates.push(where_predicate)
                }
                PredicateType::Open(open_predicate) => open_predicates.push(open_predicate),
            }
        }
    }

    Ok(open_predicates)
}

pub(crate) fn extended_bounds_predicate_split(
    predicate: syn::WherePredicate,
) -> syn::Result<PredicateType> {
    if let syn::WherePredicate::Type(mut predicate_type) = predicate {
        if let Some(bound_lifetimes) = predicate_type.lifetimes.as_ref()
            && bound_lifetimes.lifetimes.len() == 1
            && matches!(
                bound_lifetimes.lifetimes.first(),
                Some(syn::GenericParam::Type(_))
            )
        {
            let Some(syn::GenericParam::Type(type_param)) = predicate_type
                .lifetimes
                .take()
                .unwrap()
                .lifetimes
                .into_iter()
                .next()
            else {
                unreachable!()
            };

            if let Some(colon_token) = &type_param.colon_token {
                return Err(syn::Error::new(
                    colon_token.span(),
                    "Bounds are not supported here",
                ));
            }

            Ok(PredicateType::Open(OpenPredicate::new(
                type_param.ident,
                predicate_type,
            )))
        } else {
            Ok(PredicateType::Closed(syn::WherePredicate::Type(
                predicate_type,
            )))
        }
    } else {
        Ok(PredicateType::Closed(predicate))
    }
}

impl syn::parse::Parse for AttrImpl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let impl_: syn::Token![impl] = input.parse()?;
        let mut impl_generics: syn::Generics = input.parse()?;
        let trait_: syn::Path = input.parse()?;

        impl_generics.where_clause = extended_where_clause_parse(input)?;

        let (brace, overrides) = if input.peek(syn::token::Brace) {
            let overrides_content;
            let brace = syn::braced!(overrides_content in input);

            let mut overrides = vec![];
            while !overrides_content.is_empty() {
                overrides.push(overrides_content.parse()?);
            }
            (brace, overrides)
        } else {
            (Default::default(), vec![])
        };

        let item_impl = syn::ItemImpl {
            attrs: vec![],
            defaultness: None,
            unsafety: None,
            impl_token: impl_,
            generics: impl_generics,
            trait_: Some((None, trait_, Default::default())),
            // Self doesn't make sense, but it's easier and it shouldn't matter
            self_ty: Box::new(parse_quote!(Self)),
            brace_token: brace,
            items: overrides,
        };

        Ok(Self { item_impl })
    }
}

#[derive(Clone)]
pub(crate) struct AttrVariant {
    ty: syn::Type,
}

impl AttrVariant {
    pub(crate) fn apply(&self, input: &mut TagsetImplInput) -> syn::Result<()> {
        let next_discriminant = input.next_discriminant.element.get_or_insert_default();

        let discriminant = next_discriminant.clone();
        next_discriminant.value += 1;

        input.mappings.element.push(Mapping {
            discriminant,
            ty: self.ty.clone(),
        });

        Ok(())
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        let Self { ty } = self;

        Director::direct(director, ty);
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        let Self { ty } = self;

        Director::direct_mut(director, ty);
    }
}

impl quote::ToTokens for AttrVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { ty } = self;

        ty.to_tokens(tokens);
    }
}

impl syn::parse::Parse for AttrVariant {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self { ty: input.parse()? })
    }
}

pub(crate) type TagsetMetaNone = TagsetMeta<NoDefault>;

pub(crate) enum NoDefault {}

impl syn::parse::Parse for NoDefault {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Err(input.error("default is not permitted here"))
    }
}

impl quote::ToTokens for NoDefault {
    fn to_tokens(&self, _tokens: &mut TokenStream) {
        unreachable!()
    }
}

impl TagsetDefaultKind for NoDefault {
    fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        _director: &mut Director<'_, D, V>,
    ) {
        unreachable!()
    }

    fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        _director: &mut Director<'_, D, V>,
    ) {
        unreachable!()
    }
}

pub(crate) enum TagsetMeta<Df = NoDefault> {
    Default(TagsetDefault<Df>),
    Bounds(TagsetBounds),
}

impl<Df> TagsetMeta<Df> {
    pub const IDENT: &str = "meta";
}

impl<Df> TagsetMeta<Df>
where
    Df: TagsetDefaultKind,
{
    pub(crate) fn from_meta(meta: &syn::Meta) -> syn::Result<Option<Self>> {
        if let syn::Meta::List(meta_list) = meta {
            Self::from_meta_list(meta_list)
        } else {
            Ok(None)
        }
    }

    pub(crate) fn from_meta_list(meta_list: &syn::MetaList) -> syn::Result<Option<Self>> {
        match meta_list.path.get_ident() {
            Some(ident) => {
                if ident == Self::IDENT {
                    let sub_attribute: syn::Meta = meta_list.parse_args()?;
                    let ident = sub_attribute.path().require_ident()?;

                    if ident == TagsetDefaultNone::IDENT {
                        sub_attribute
                            .require_list()?
                            .parse_args()
                            .map(Self::Default)
                            .map(Some)
                    } else if ident == TagsetBounds::IDENT {
                        sub_attribute
                            .require_list()?
                            .parse_args()
                            .map(Self::Bounds)
                            .map(Some)
                    } else {
                        Err(syn::Error::new(ident.span(), "Unknown meta attribute"))
                    }
                } else {
                    Ok(None)
                }
            }
            None => Ok(None),
        }
    }

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        match self {
            Self::Default(tagset_default) => tagset_default.direct(director),
            Self::Bounds(tagset_bounds) => tagset_bounds.direct(director),
        }
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        match self {
            Self::Default(tagset_default) => tagset_default.direct_mut(director),
            Self::Bounds(tagset_bounds) => tagset_bounds.direct_mut(director),
        }
    }
}

impl<Df: TagsetDefaultKind> quote::ToTokens for TagsetMeta<Df> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TagsetMeta::Default(tagset_default) => tagset_default.to_tokens(tokens),
            TagsetMeta::Bounds(tagset_bounds) => tagset_bounds.to_tokens(tokens),
        }
    }
}

pub(crate) type TagsetDefaultNone = TagsetDefault<NoDefault>;

pub(crate) trait TagsetDefaultKind: syn::parse::Parse + quote::ToTokens {
    fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    );

    fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    );
}

pub(crate) struct FnBody(Vec<syn::Stmt>);

impl quote::ToTokens for FnBody {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for stmt in &self.0 {
            stmt.to_tokens(tokens);
        }
    }
}

impl syn::parse::Parse for FnBody {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self(syn::Block::parse_within(input)?))
    }
}

impl TagsetDefaultKind for FnBody {
    fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        for stmt in &self.0 {
            Director::direct(director, stmt);
        }
    }

    fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        for stmt in &mut self.0 {
            Director::direct_mut(director, stmt);
        }
    }
}

impl TagsetDefaultKind for syn::Type {
    fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        Director::direct(director, self);
    }

    fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        Director::direct_mut(director, self);
    }
}

pub(crate) struct TagsetDefault<Df = NoDefault>(Df);

impl<Df> TagsetDefault<Df> {
    const IDENT: &str = "default";
}

impl<Df: TagsetDefaultKind> TagsetDefault<Df> {
    fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        self.0.direct(director);
    }

    fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        self.0.direct_mut(director);
    }
}

impl<Df: TagsetDefaultKind> syn::parse::Parse for TagsetDefault<Df> {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self(Df::parse(input)?))
    }
}

impl<Df: TagsetDefaultKind> quote::ToTokens for TagsetDefault<Df> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}

#[derive(Default)]
pub(crate) struct TagsetBounds {
    pub(crate) predicates: syn::punctuated::Punctuated<syn::WherePredicate, syn::Token![,]>,
}

impl TagsetBounds {
    const IDENT: &str = "bounds";

    pub fn direct<
        D: directed_visit::syn::direct::Full<V> + ?Sized,
        V: directed_visit::syn::visit::Full + ?Sized,
    >(
        &self,
        director: &mut Director<'_, D, V>,
    ) {
        for predicate in &self.predicates {
            Director::direct(director, predicate);
        }
    }

    pub fn direct_mut<
        D: directed_visit::syn::direct::FullMut<V> + ?Sized,
        V: directed_visit::syn::visit::FullMut + ?Sized,
    >(
        &mut self,
        director: &mut Director<'_, D, V>,
    ) {
        for predicate in &mut self.predicates {
            Director::direct_mut(director, predicate);
        }
    }
}

impl quote::ToTokens for TagsetBounds {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { predicates } = self;

        predicates.to_tokens(tokens);
    }
}

impl syn::parse::Parse for TagsetBounds {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            predicates: extended_where_predicates_parse(input)?,
        })
    }
}
