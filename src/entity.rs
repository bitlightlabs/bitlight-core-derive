use std::collections::HashSet;

use darling::{ast::Data, FromDeriveInput, FromField};
use iter_tools::{multiunzip, Itertools};
use proc_macro2::TokenStream;
use quote::quote;
use stringcase::pascal_case;
use syn::{GenericArgument, Generics, Ident, PathArguments, Type};

use crate::field_ident;

#[derive(FromField)]
#[darling(attributes(entity))]
struct EntityBuilderFieldReceiver {
    ty: Type,
    ident: Option<Ident>,
    #[darling(default)]
    searchable: bool,
}

#[derive(FromDeriveInput)]
#[darling(attributes(entity), supports(struct_named))]
pub(crate) struct EntityBuilderStructReceiver {
    ident: Ident,
    generics: Generics,
    table_name: String,
    order_by: Option<String>,
    data: Data<(), EntityBuilderFieldReceiver>,
}

fn type_is_gzip_strict_type(ty: &Type) -> Option<&Ident> {
    if let Type::Path(ref type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "GzipStrictType" {
                if let PathArguments::AngleBracketed(ref angle_bracketed) = segment.arguments {
                    if let Some(GenericArgument::Type(Type::Path(ref type_path))) =
                        angle_bracketed.args.last()
                    {
                        return type_path.path.segments.last().map(|segment| &segment.ident);
                    }
                }
            }
        }
    }
    None
}

fn parse_order_by(order_by: Option<&str>) -> HashSet<String> {
    order_by
        .map(|stat| {
            stat.split(",")
                .map(|splits| {
                    let mut splits = splits.trim().split(" ");
                    match (splits.next(), splits.next().unwrap_or("asc")) {
                        (Some(col), order) => {
                            match order.to_ascii_lowercase().as_str() {
                                "asc" | "desc" => (),
                                ty => panic!("Unsupported sort type of: '{ty}'"),
                            }
                            col.to_string()
                        }
                        _ => panic!("Incorrect order by format of: '{stat}'"),
                    }
                })
                .collect()
        })
        .unwrap_or_default()
}

impl EntityBuilderStructReceiver {
    pub(crate) fn render_rgb_entity(&self) -> TokenStream {
        let EntityBuilderStructReceiver { data, ident, .. } = self;

        let (impl_generics, type_generics, where_clause) = self.generics.split_for_impl();

        let gzip_strict_fields = data
            .as_ref()
            .take_struct()
            .unwrap()
            .fields
            .iter()
            .enumerate()
            .filter_map(|(index, field)| {
                let field_ident = field_ident(field.ident.as_ref(), index);
                type_is_gzip_strict_type(&field.ty).map(|data_ident| (field_ident, data_ident))
            })
            .collect::<Vec<_>>();

        if gzip_strict_fields.len() != 1 {
            panic!("Only one GzipStrictData supported.");
        }

        let field_ident = gzip_strict_fields[0].0.clone();
        let rgb_data_ident = gzip_strict_fields[0].1;

        quote! {
            impl #impl_generics crate::entity::RgbPersistenceData<#rgb_data_ident> for #ident #type_generics #where_clause {
                fn into_data(self) -> #rgb_data_ident {
                    self.#field_ident.into_inner()
                }
            }
        }
    }

    pub(crate) fn render_entity(&self) -> TokenStream {
        let EntityBuilderStructReceiver {
            data,
            ident,
            table_name,
            ..
        } = self;

        let (impl_generics, type_generics, where_clause) = self.generics.split_for_impl();

        let mut searchable_key_metas = Vec::new();
        let mut order_by_fields = parse_order_by(self.order_by.as_deref());

        let field_idents = data
            .as_ref()
            .take_struct()
            .unwrap()
            .fields
            .into_iter()
            .fold(Vec::new(), |mut field_idents, field| {
                let field_ident = field.ident.clone().expect("Not support tuple");

                let field_name = field_ident.to_string();

                order_by_fields.remove(&field_name);

                if field.searchable {
                    searchable_key_metas.push((field.ty.clone(), field_ident.clone(), field_name));
                }

                field_idents.push(field_ident);
                field_idents
            });

        if searchable_key_metas.is_empty() {
            panic!("No primary key found");
        }

        if !order_by_fields.is_empty() {
            panic!(
                "The order by statement contains a non-existent columns of: {:?}",
                order_by_fields
            )
        }

        let order_by_tokens = match self.order_by {
            Some(ref order_by) => {
                let order_by = order_by.replace("asc", "ASC").replace("desc", "DESC");
                quote! { Some(#order_by) }
            }
            None => quote! { None },
        };

        let entity_field_enum_ident = Ident::new(&format!("{}Column", ident), ident.span());
        let entity_field_tokens = searchable_key_metas
            .iter()
            .map(|(ty, ident, name)| {
                let enum_field = Ident::new(&pascal_case(&ident.to_string()), ident.span());
                (
                    quote! {
                        #enum_field(&'a #ty)
                    },
                    quote! {
                        Self::#enum_field(..) => #name
                    },
                    quote! {
                        Self::#enum_field(#ident) => #ident
                    },
                )
            })
            .collect::<Vec<_>>();

        let (entity_field_definitions, entity_field_name_impls, entity_field_value_impls): (
            Vec<_>,
            Vec<_>,
            Vec<_>,
        ) = multiunzip(entity_field_tokens);

        let insert_one_sql = format!(
            "INSERT INTO {} ({}) VALUES ({}) ON CONFLICT DO NOTHING RETURNING (XMAX = 0) AS inserted",
            table_name,
            field_idents.iter().join(", "),
            field_idents
                .iter()
                .enumerate()
                .map(|(index, ..)| format!("${}", index + 1))
                .join(", ")
        );

        let insert_one_binds = field_idents
            .iter()
            .map(|ident| quote! { .bind(self.#ident) });

        quote! {
            use crate::entity::{Entity, EntityField};

            pub(crate) enum #entity_field_enum_ident<'a> {
                #(#entity_field_definitions),*
            }

            impl<'a> crate::entity::EntityField for #entity_field_enum_ident<'a> {
                fn name(&self) -> &'static str {
                    match self {
                        #(#entity_field_name_impls),*
                    }
                }

                fn value(&self) -> impl sqlx::Type<sqlx::Postgres> + sqlx::Encode<sqlx::Postgres> {
                    match self {
                        #(#entity_field_value_impls),*
                    }
                }
            }

            impl #ident {
                pub(crate) async fn fetch_one<'a, 'c, E>(columns: &[#entity_field_enum_ident<'a>],  executor: E) -> Result<Self, crate::entity::EntityError>
                where
                    E: sqlx::Executor<'c, Database = sqlx::Postgres>,
                {
                    let where_clause = columns
                        .iter()
                        .enumerate()
                        .map(|(index, column)| format!("{} = ${}", column.name(), index + 1))
                        .collect::<Vec<_>>()
                        .join(" AND ");


                    let sql = format!("SELECT * FROM {} WHERE {where_clause}", Self::table_name());
                    let mut query = sqlx::query_as(&sql);
                    for column in columns {
                        query = query.bind(column.value());
                    }
                    query.fetch_one(executor).await.map_err(Into::into)
               }
            }

            impl #impl_generics crate::entity::Entity for #ident #type_generics #where_clause {
                fn table_name() -> &'static str {
                    #table_name
                }

                fn order_by() -> Option<&'static str> {
                    #order_by_tokens
                }

                async fn insert_one<'c, E>(self, executor: E) -> Result<bool, crate::entity::EntityError>
                where
                    E: sqlx::Executor<'c, Database = sqlx::Postgres>,
                {
                    sqlx::query_as(#insert_one_sql)
                        #(#insert_one_binds)*
                        .fetch_one(executor)
                        .await
                        .map_err(Into::into)
                        .map(|fetched: crate::entity::EntityInserted| fetched.is_inserted())
                }
           }
        }
    }
}

#[cfg(test)]
mod tests {
    use darling::FromDeriveInput;

    use super::EntityBuilderStructReceiver;

    #[test]
    fn print_entity_builder_render() {
        let input = r#"
#[derive(EntityBuilder, RgbEntity)]
#[entity(table_name = "fake_table")]
#[entity(order_by = "name asc, age desc")]
struct FakeEntity {
    #[entity(searchable)]
    id: i64,
    #[entity(searchable)]
    name: String,
    age: i32,
    sex: Sex,
    data: GzipStrictType<AluVMLib>,
}
"#;
        let parsed = syn::parse_str(input).unwrap();
        let receiver = EntityBuilderStructReceiver::from_derive_input(&parsed).unwrap();
        let tokens = receiver.render_entity();
        println!("tokens: {}", tokens);
        let formatted_code = prettyplease::unparse(&syn::parse2(tokens).unwrap());
        println!("{}", formatted_code);
        // let tokens = receiver.render_rgb_entity();
        // println!("{}", tokens);
    }
}
