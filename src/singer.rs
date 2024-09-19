use darling::ast::Data;
use darling::{FromDeriveInput, FromField};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{Generics, Type};

use crate::{field_ident, type_is_optional};

#[derive(FromField)]
struct SignerFieldReceiver {
    ty: Type,
    ident: Option<Ident>,
}

#[derive(FromDeriveInput)]
pub(crate) struct SignerStructReceiver {
    ident: Ident,
    generics: Generics,
    data: Data<(), SignerFieldReceiver>,
}

impl SignerStructReceiver {
    pub(crate) fn render(self) -> TokenStream {
        let ident = self.ident;
        let (impl_generics, type_generics, where_clause) = self.generics.split_for_impl();

        let field_list = self
            .data
            .as_ref()
            .take_struct()
            .expect("Don't support enum")
            .fields
            .into_iter()
            .filter(|field| !type_is_optional(&field.ty))
            .enumerate()
            .map(|(index, field)| {
                let field_ident = field_ident(field.ident.as_ref(), index);
                let field_name = field_ident.to_string();
                let field_string_name = format_ident!("{}_string", field_name);

                quote! {
                    let #field_string_name = self.#field_ident.to_string();
                    map.insert(#field_name, urlencoding::encode(&#field_string_name));
                }
            });

        quote! {
            impl #impl_generics SignMessage for #ident #type_generics #where_clause {
                fn sign_message(&self, timestamp: i64) -> String {
                    let mut map = std::collections::BTreeMap::new();
                    map.insert("timestamp", std::borrow::Cow::Owned(timestamp.to_string()));
                    #(#field_list)*
                    map.into_iter().map(|(key, value)| format!("{key}={value}"))
                        .collect::<Vec<_>>()
                        .join("&")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use darling::FromDeriveInput;

    use super::SignerStructReceiver;

    #[test]
    fn print_signer_render() {
        let input = r#"
#[derive(Signer)]
struct FakeSigner<T> {
    name: String,
    age: i32,
    bar: Option<T>,
}
"#;

        let parsed = syn::parse_str(input).unwrap();
        let receiver = SignerStructReceiver::from_derive_input(&parsed).unwrap();
        let tokens = receiver.render();
        println!("{}", tokens);
    }
}
