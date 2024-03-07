use proc_macro::TokenStream;
use syn::DeriveInput;

mod sign;

#[proc_macro_derive(Sign)]
pub fn derive_signature(input: TokenStream) -> TokenStream {
    sign::SignatureContext::from(syn::parse_macro_input!(input as DeriveInput))
        .render()
        .into()
}
