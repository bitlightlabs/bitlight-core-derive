use darling::FromDeriveInput;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident, Index, PathArguments, Type};

mod entity;
mod singer;

pub(crate) fn type_is_optional(ty: &Type) -> bool {
    if let Type::Path(ref type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            return segment.ident == "Option"
                && matches!(segment.arguments, PathArguments::AngleBracketed(..));
        }
    }
    false
}

pub(crate) fn field_ident(ident: Option<&Ident>, index: usize) -> proc_macro2::TokenStream {
    ident.as_ref().map(|v| quote!(#v)).unwrap_or_else(|| {
        let index = Index::from(index);
        quote!(#index)
    })
}

#[proc_macro_derive(EntityBuilder, attributes(entity))]
pub fn derive_entity_builder(input: TokenStream) -> TokenStream {
    entity::EntityBuilderStructReceiver::from_derive_input(&parse_macro_input!(
        input as DeriveInput
    ))
    .unwrap()
    .render_entity()
    .into()
}

#[proc_macro_derive(RgbEntity)]
pub fn derive_rgb_entity(input: TokenStream) -> TokenStream {
    entity::EntityBuilderStructReceiver::from_derive_input(&parse_macro_input!(
        input as DeriveInput
    ))
    .unwrap()
    .render_rgb_entity()
    .into()
}

#[proc_macro_derive(Signer)]
pub fn derive_signer(input: TokenStream) -> TokenStream {
    singer::SignerStructReceiver::from_derive_input(&parse_macro_input!(input as DeriveInput))
        .unwrap()
        .render()
        .into()
}
