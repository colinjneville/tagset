use directed_visit::Director;
use quote::ToTokens as _;
use syn::parse::Parse as _;

use crate::parsing;

enum AttrContext {
    None,
    Fn,
    Type,
}

pub(crate) struct TagsetDirect {
    attr_context: AttrContext,
    error: Option<syn::Error>,
}

impl TagsetDirect {
    pub fn new() -> Self {
        Self {
            attr_context: AttrContext::None,
            error: None,
        }
    }

    fn record_error(&mut self, error: syn::Error) {
        match self.error.as_mut() {
            Some(es) => es.combine(error),
            None => self.error = Some(error),
        }
    }

    pub fn into_result(mut self) -> syn::Result<()> {
        self.take_result()
    }

    pub fn take_result(&mut self) -> syn::Result<()> {
        match self.error.take() {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    fn direct_meta<V: directed_visit::syn::visit::Full + ?Sized, Df: parsing::TagsetDefaultKind>(
        director: &mut directed_visit::Director<'_, Self, V>,
        node: &syn::MetaList,
    ) {
        match parsing::TagsetMeta::<Df>::from_meta_list(node) {
            Ok(Some(tagset_meta)) => {
                tagset_meta.direct(director);
            }
            Ok(None) => {}
            Err(e) => director.record_error(e),
        }
    }

    fn direct_meta_mut<
        V: directed_visit::syn::visit::FullMut + ?Sized,
        Df: parsing::TagsetDefaultKind,
    >(
        director: &mut directed_visit::Director<'_, Self, V>,
        node: &mut syn::MetaList,
    ) {
        match parsing::TagsetMeta::<Df>::from_meta_list(node) {
            Ok(Some(mut tagset_meta)) => {
                tagset_meta.direct_mut(director);
                // Write back the altered arguments
                node.tokens = tagset_meta.to_token_stream();
            }
            Ok(None) => {}
            Err(e) => director.record_error(e),
        }
    }
}

impl<V> directed_visit::syn::direct::Full<V> for TagsetDirect
where
    V: directed_visit::syn::visit::Full + ?Sized,
{
    fn direct_trait_item_fn(
        mut director: directed_visit::Director<'_, Self, V>,
        node: &syn::TraitItemFn,
    ) {
        director.attr_context = AttrContext::Fn;
        directed_visit::syn::direct::default::direct_trait_item_fn(&mut director, node);
        director.attr_context = AttrContext::None;
    }

    fn direct_trait_item_type(
        mut director: directed_visit::Director<'_, Self, V>,
        node: &syn::TraitItemType,
    ) {
        director.attr_context = AttrContext::Type;
        directed_visit::syn::direct::default::direct_trait_item_type(&mut director, node);
        director.attr_context = AttrContext::None;
    }

    fn direct_meta_list(mut director: directed_visit::Director<'_, Self, V>, node: &syn::MetaList) {
        if let Some(ident) = node.path.get_ident() {
            if ident == "tagset" {
                match node.parse_args_with(parsing::Attr::parse) {
                    Ok(attr) => {
                        attr.direct(&mut director);
                        return;
                    }
                    Err(e) => director.record_error(e),
                }
            } else {
                match &director.attr_context {
                    AttrContext::None => {
                        Self::direct_meta::<_, parsing::NoDefault>(&mut director, node)
                    }
                    AttrContext::Fn => Self::direct_meta::<_, parsing::FnBody>(&mut director, node),
                    AttrContext::Type => Self::direct_meta::<_, syn::Type>(&mut director, node),
                }
            }
        }

        directed_visit::syn::direct::default::direct_meta_list(&mut director, node);
    }
}

impl<V> directed_visit::syn::direct::FullMut<V> for TagsetDirect
where
    V: directed_visit::syn::visit::FullMut + ?Sized,
{
    fn direct_meta_list_mut(
        mut director: directed_visit::Director<'_, Self, V>,
        node: &mut syn::MetaList,
    ) {
        if let Some(ident) = node.path.get_ident() {
            if ident == "tagset" {
                match node.parse_args_with(parsing::Attr::parse) {
                    Ok(mut attr) => {
                        attr.direct_mut(&mut director);
                        // Write back the altered arguments
                        node.tokens = attr.to_token_stream();

                        return;
                    }
                    Err(e) => director.record_error(e),
                }
            } else {
                match &director.attr_context {
                    AttrContext::None => {
                        Self::direct_meta_mut::<_, parsing::NoDefault>(&mut director, node)
                    }
                    AttrContext::Fn => {
                        Self::direct_meta_mut::<_, parsing::FnBody>(&mut director, node)
                    }
                    AttrContext::Type => Self::direct_meta_mut::<_, syn::Type>(&mut director, node),
                }
            }
        }

        directed_visit::syn::direct::default_mut::direct_meta_list_mut(&mut director, node);
    }
}

impl<V: directed_visit::syn::visit::Full + ?Sized> directed_visit::Direct<V, parsing::Attr>
    for TagsetDirect
{
    fn direct(mut director: Director<'_, Self, V>, node: &parsing::Attr) {
        node.direct(&mut director);
    }
}

impl<V: directed_visit::syn::visit::FullMut + ?Sized> directed_visit::DirectMut<V, parsing::Attr>
    for TagsetDirect
{
    fn direct_mut(mut director: Director<'_, Self, V>, node: &mut parsing::Attr) {
        node.direct_mut(&mut director);
    }
}

impl directed_visit::VisitMut<parsing::Attr> for telety::visitor::ApplyAliases<'_> {}

impl directed_visit::VisitMut<parsing::Attr> for telety::visitor::ApplyGenericArguments<'_> {}
