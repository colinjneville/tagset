use syn::parse_quote;

pub(crate) fn private_module() -> syn::Path {
    parse_quote!(::tagset::__private)
}

pub(crate) fn impl_enum_ident(struct_ident: &syn::Ident) -> syn::Ident {
    quote::format_ident!("{struct_ident}TagsetImpl")
}

pub(crate) fn impl_enum_type(struct_type: &syn::TypePath) -> syn::TypePath {
    let private_module = crate::tokens::private_module();
    parse_quote!(<#struct_type as #private_module::IntoImpl>::Impl)
}
