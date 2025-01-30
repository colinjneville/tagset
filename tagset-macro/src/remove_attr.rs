pub struct RemoveAttr<F>(F);

impl<F: Fn(&syn::Attribute) -> bool> RemoveAttr<F> {
    pub fn new(f: F) -> Self {
        Self(f)
    }

    fn remove_attr(attr: &mut syn::Attribute) {
        // It is much easier to turn the attribute into a noop cfg attribute
        // than remove it entirely (which requires visiting all the syntax elements
        // which are allowed to have attributes).
        attr.meta = syn::parse_quote! {
            cfg(all())
        };
    }
}

impl<F: Fn(&syn::Attribute) -> bool> directed_visit::syn::visit::FullMut for RemoveAttr<F> {
    fn visit_attribute_mut<D>(
        visitor: directed_visit::Visitor<'_, D, Self>,
        node: &mut syn::Attribute,
    ) where
        D: directed_visit::DirectMut<Self, syn::Attribute> + ?Sized,
    {
        if visitor.0(node) {
            Self::remove_attr(node);
        } else {
            directed_visit::Visitor::visit_mut(visitor, node);
        }
    }
}
