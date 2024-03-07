use std::fmt::{Debug, Formatter};

use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{
    Data, DataStruct, DeriveInput, Fields, FieldsNamed, GenericArgument, Path, PathArguments, Type,
    TypePath,
};

fn get_option_inner(ty: Type) -> (bool, Type) {
    if let Type::Path(TypePath {
        path: Path { ref segments, .. },
        ..
    }) = ty
    {
        if let Some(v) = segments.into_iter().next() {
            if v.ident == "Option" {
                let t = match &v.arguments {
                    PathArguments::AngleBracketed(a) => match a.args.iter().next() {
                        Some(GenericArgument::Type(t)) => t,
                        _ => panic!("Not sure what to do with other GenericArgument"),
                    },
                    _ => panic!("Not sure what to do with other PathArgument"),
                };
                return (true, t.clone());
            };
        }
    }
    (false, ty)
}

#[derive(Debug)]
struct Field {
    _ty: Type,
    name: Ident,
    optional: bool,
}

impl From<syn::Field> for Field {
    fn from(field: syn::Field) -> Self {
        let (optional, ty) = get_option_inner(field.ty);
        Self {
            _ty: ty,
            optional,
            name: field.ident.unwrap(),
        }
    }
}

pub(crate) struct SignatureContext {
    name: Ident,
    fields: Vec<Field>,
}

impl Debug for SignatureContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "name: {}", self.name)?;
        for field in self.fields.iter() {
            writeln!(f, "field: {:?}", field)?;
        }
        Ok(())
    }
}

impl From<DeriveInput> for SignatureContext {
    fn from(input: DeriveInput) -> Self {
        let name = input.ident;

        let fields = if let Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) = input.data
        {
            named
        } else {
            panic!("Unsupported data");
        }
        .into_iter()
        .map(Field::from)
        .collect();

        Self { name, fields }
    }
}

impl SignatureContext {
    fn gen_message(&self) -> TokenStream {
        let mut code = quote! {
            let mut map = std::collections::BTreeMap::new();
        };

        for Field { name, optional, .. } in self.fields.iter() {
            if !optional {
                let name_str = name.to_string();
                let name_string_variable =
                    Ident::new(&format!("{}_string_variable", name_str), name.span());

                code.extend(quote! {
                    let #name_string_variable = self.#name.to_string();
                    map.insert(#name_str, urlencoding::encode(&#name_string_variable));
                });
            }
        }

        code.extend(quote! {
            map.insert("timestamp", std::borrow::Cow::Owned(timestamp.to_string()));

            map.into_iter().map(|(key, value)| {
                format!("{key}={value}")
            })
            .collect::<Vec<_>>()
            .join("&")
        });

        code
    }

    pub(crate) fn render(&self) -> TokenStream {
        let name = &self.name;
        let gen_message = self.gen_message();

        quote! {
            impl SignMessage for #name {
                fn sign_message(&self, timestamp: i64) -> String {
                    #gen_message
                }
            }
        }
    }
}
