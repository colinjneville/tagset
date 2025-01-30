use proc_macro2::TokenStream;
use quote::quote;
use syn::parse2;

use crate::telety_util;

// struct MetaMapVisitor<'m, 'map> {
//     // sub_map: telety::alias::Map<'p>,
//     telety_visitor: telety::visitor:: telety::visitor::IdentifyAliases<'m, 'map>,
//     is_in_meta: bool,
//     errors: Option<syn::Error>,
// }

// impl<'m, 'map> MetaMapVisitor<'m, 'map> {
//     fn new(sub_map: &'m mut telety::alias::Map<'map>) -> Self {
//         let telety_visitor = telety::visitor::IdentifyAliases::new(alias_map);
//         Self {
//             // sub_map,
//             is_in_meta: false,
//             errors: None,
//         }
//     }

//     fn add_error(&mut self, error: syn::Error) {
//         match self.errors.as_mut() {
//             Some(errors) => errors.combine(error),
//             None => self.errors = Some(error),
//         }
//     }
//     fn into_result(self) -> syn::Result<telety::alias::Map<'p>> {
//         match self.errors {
//             Some(errors) => Err(errors),
//             None => Ok(self.sub_map),
//         }
//     }
// }

// impl<'p, 'ast> syn::visit::Visit<'ast> for MetaMapVisitor<'p> {
//     fn visit_meta(&mut self, i: &'ast syn::Meta) {
//         match crate::parsing::TagsetMeta::from_meta(i) {
//             Ok(Some(meta)) => {
//                 let was_in_meta = std::mem::replace(&mut self.is_in_meta, true);
//                 meta.visit(self);
//                 self.is_in_meta = was_in_meta;
//             }
//             Ok(None) => { }
//             Err(e) => self.add_error(e),
//         }
//     }

//     fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
//         if self.is_in_meta {
//             let _ = self.sub_map.insert(i);
//         }
//     }
// }

// impl<'m, 'map> syn::visit::Visit for Visitor<'m, 'map> {
//     fn visit_meta(&mut self, i: &'ast syn::Meta) {
//         crate::parsing::TagsetMeta::from_meta(i)
//     }
// }

pub(crate) fn tagset_telety_alias_map(input: TokenStream) -> syn::Result<TokenStream> {
    let item: syn::Item = parse2(input)?;
    let telety = telety::Telety::new(&item)?;
    let sub_map = telety_util::make_alias_map(&telety)?;

    Ok(quote! {
        #sub_map
    })
}
