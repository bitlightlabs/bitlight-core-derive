use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::StructReceiver;

impl StructReceiver {
    pub(crate) fn signer_render(self) -> TokenStream {
        let ident = self.ident;
        let (impl_generics, type_generics, where_clause) = self.generics.split_for_impl();

        let field_list = self
            .data
            .as_ref()
            .take_struct()
            .expect("Don't support enum")
            .fields
            .into_iter()
            .filter(|field| !field.is_optional())
            .enumerate()
            .map(|(index, field)| {
                let field_ident = field.ident.as_ref().map(|v| quote!(#v)).unwrap_or_else(|| {
                    let index = syn::Index::from(index);
                    quote!(#index)
                });

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

    use crate::StructReceiver;

    #[test]
    fn print_signer_render() {
        let input = r#"#[derive(Signer)]
pub struct FakeSigner<T> {
    name: String,
    age: i32,
    bar: Option<T>,
}"#;

        let parsed = syn::parse_str(input).unwrap();
        let receiver = StructReceiver::from_derive_input(&parsed).unwrap();
        let tokens = receiver.signer_render();
        println!("{}", tokens);
    }
}
