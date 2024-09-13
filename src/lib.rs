use darling::{ast::Data, FromDeriveInput, FromField};
use proc_macro::TokenStream;
use syn::{DeriveInput, Generics, Ident, PathArguments, Type};

#[derive(FromField)]
pub(crate) struct StructFieldReceiver {
    ty: Type,
    ident: Option<Ident>,
}

impl StructFieldReceiver {
    pub(crate) fn is_optional(&self) -> bool {
        if let Type::Path(ref type_path) = self.ty {
            if let Some(segment) = type_path.path.segments.last() {
                return segment.ident == "Option"
                    && matches!(segment.arguments, PathArguments::AngleBracketed(..));
            }
        }
        false
    }
}

#[derive(FromDeriveInput)]
pub(crate) struct StructReceiver {
    ident: Ident,
    generics: Generics,
    data: Data<(), StructFieldReceiver>,
}

mod singer;

#[proc_macro_derive(Signer)]
pub fn derive_signature(input: TokenStream) -> TokenStream {
    StructReceiver::from_derive_input(&syn::parse_macro_input!(input as DeriveInput))
        .unwrap()
        .signer_render()
        .into()
}
