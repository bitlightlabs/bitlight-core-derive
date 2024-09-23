use std::fmt::{self, Display, Formatter};

use better_default::Default;
use darling::{ast::Data, FromDeriveInput, FromField, FromMeta};
use iter_tools::{multiunzip, Itertools};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{AngleBracketedGenericArguments, GenericArgument, Generics, Ident, PathArguments, Type};

use crate::field_ident;

#[derive(Debug, Clone, Copy, FromMeta)]
enum OrderBy {
    Asc,
    Desc,
}

impl Display for OrderBy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let order = match self {
            OrderBy::Asc => "ASC",
            OrderBy::Desc => "DESC",
        };
        write!(f, "{order}")
    }
}

#[derive(Clone, Copy, Default, FromMeta, Eq, PartialEq)]
enum FieldEntityType {
    #[default]
    Normal,
    Primary,
    Searchable,
}

struct FieldMeta {
    name: String,
    ident: Ident,
    ty: Type,
    entity_type: FieldEntityType,
}

impl FieldMeta {
    fn is_normal(&self) -> bool {
        self.entity_type == FieldEntityType::Normal
    }

    fn is_primary(&self) -> bool {
        self.entity_type == FieldEntityType::Primary
    }
}

#[derive(FromField)]
#[darling(attributes(entity))]
struct EntityBuilderFieldReceiver {
    ty: Type,
    ident: Option<Ident>,
    #[darling(default)]
    field_type: FieldEntityType,
    order_by: Option<OrderBy>,
}

#[derive(FromDeriveInput)]
#[darling(attributes(entity), supports(struct_named))]
pub(crate) struct EntityBuilderStructReceiver {
    ident: Ident,
    generics: Generics,
    table_name: String,
    data: Data<(), EntityBuilderFieldReceiver>,
}

fn append_generic_idents(
    generic_idents: &mut String,
    generics_arguments: &AngleBracketedGenericArguments,
) {
    for argument in generics_arguments.args.iter() {
        if let GenericArgument::Type(Type::Path(type_path)) = argument {
            if let Some(segment) = type_path.path.segments.last() {
                generic_idents.push_str(&segment.ident.to_string());
                if let PathArguments::AngleBracketed(ref generics_arguments) = segment.arguments {
                    generic_idents.push('<');
                    append_generic_idents(generic_idents, generics_arguments);
                    generic_idents.push('>');
                }
            }
        }
    }
}

fn type_is_gzip_strict_type(ty: &Type) -> Option<Type> {
    if let Type::Path(ref type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "GzipStrictType" {
                if let PathArguments::AngleBracketed(ref generics_arguments) = segment.arguments {
                    let mut generics_idents = String::new();
                    append_generic_idents(&mut generics_idents, generics_arguments);
                    if let Some(GenericArgument::Type(Type::Path(ref type_path))) =
                        generics_arguments.args.last()
                    {
                        return type_path.path.segments.last().and_then(|segment| {
                            let ty = if generics_idents.is_empty() {
                                segment.ident.to_string()
                            } else {
                                generics_idents
                            };
                            Type::from_string(&ty).ok()
                        });
                    }
                }
            }
        }
    }
    None
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
            .flat_map(|(index, field)| {
                let field_ident = field_ident(field.ident.as_ref(), index);
                type_is_gzip_strict_type(&field.ty).map(|data_ident| (field_ident, data_ident))
            })
            .collect::<Vec<_>>();

        if gzip_strict_fields.is_empty() {
            panic!("GzipStrictType not found");
        }

        let (fields_names, fields_type): (Vec<_>, Vec<_>) = multiunzip(gzip_strict_fields);
        let (into_data_type, into_data_body) = if fields_type.len() > 1 {
            (
                quote! { (#(#fields_type),*) },
                quote! { (#(self.#fields_names.into_inner()),*) },
            )
        } else {
            (
                quote! { #(#fields_type),* },
                quote! { #(self.#fields_names.into_inner()),* },
            )
        };

        quote! {
            use crate::entity::RgbPersistenceData;

            impl #impl_generics crate::entity::RgbPersistenceData<(#(#fields_type),*)> for #ident #type_generics #where_clause {
                fn into_data(self) -> #into_data_type {
                    #into_data_body
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

        let mut order_by_fragments = Vec::new();

        let fields_meta = data
            .as_ref()
            .take_struct()
            .unwrap()
            .fields
            .into_iter()
            .fold(Vec::new(), |mut fields_meta, field| {
                let field_ident = field.ident.clone().expect("Not support tuple");
                let field_name = field_ident.to_string();

                if let Some(order_by) = field.order_by {
                    order_by_fragments.push(format!("{} {}", field_name, order_by));
                }

                fields_meta.push(FieldMeta {
                    name: field_name,
                    ident: field_ident,
                    ty: field.ty.clone(),
                    entity_type: field.field_type,
                });

                fields_meta
            });

        if fields_meta.iter().filter(|meta| meta.is_primary()).count() == 0 {
            panic!("No primary key found");
        }

        let order_by_tokens = if order_by_fragments.is_empty() {
            quote! { None }
        } else {
            let order_by_statement = order_by_fragments.join(", ");
            quote! {
                Some(#order_by_statement)
            }
        };

        let entity_search_ident = Ident::new(&format!("{}Search", ident), ident.span());

        let entity_field_tokens = fields_meta
            .iter()
            .filter(|meta| !meta.is_normal())
            .map(
                |FieldMeta {
                     ty, name, ident, ..
                 }| {
                    (
                        quote! {
                            #[builder(setter(strip_option), default)]
                            #ident: Option<std::borrow::Cow<'a, #ty>>
                        },
                        {
                            let push = format!(" WHERE {} = ", name);
                            let push_and = format!(" AND {} = ", name);

                            quote! {
                                if let Some(field) = self.#ident.clone() {
                                    if !search_touched  {
                                        query_builder.push(#push);
                                        search_touched = true;
                                    } else {
                                        query_builder.push(#push_and);
                                    }
                                    query_builder.push_bind(field.into_owned());
                                }
                            }
                        },
                    )
                },
            )
            .collect::<Vec<_>>();

        let (entity_search_fields, entity_search_fields_where_clause): (Vec<_>, Vec<_>) =
            multiunzip(entity_field_tokens);

        let insert_one_sql = format!(
            "INSERT INTO {0} ({1}) VALUES ({2})",
            table_name,
            fields_meta.iter().map(|meta| &meta.name).join(", "),
            fields_meta
                .iter()
                .enumerate()
                .map(|(index, ..)| format!("${}", index + 1))
                .join(", ")
        );

        let insert_or_update_sql = format!(
            "{0} ON CONFLICT ({1}) DO UPDATE SET {2} RETURNING (XMAX=0) AS inserted",
            insert_one_sql,
            fields_meta
                .iter()
                .filter(|meta| meta.is_primary())
                .map(|mata| &mata.name)
                .join(", "),
            fields_meta
                .iter()
                .filter(|meta| meta.is_normal())
                .map(|meta| format!("{0} = EXCLUDED.{0}", meta.name))
                .join(", ")
        );

        let insert_bindings = fields_meta
            .iter()
            .map(|FieldMeta { ident, .. }| quote! { .bind(self.#ident) });

        let insert_bindings_cloned = insert_bindings.clone();

        quote! {
            use std::borrow::Borrow;

            use crate::entity::{Entity, EntitySearch};

            #[derive(derive_builder::Builder)]
            #[builder(pattern = "owned", setter(into))]
            pub(crate) struct #entity_search_ident<'a> {
                #(#entity_search_fields),*
            }

            impl crate::entity::EntitySearch for #entity_search_ident<'_> {

                fn query_builder(&self, query_builder: &mut sqlx::QueryBuilder<sqlx::Postgres>) {
                    let mut search_touched = false;
                    query_builder.push(format!("SELECT * FROM {}", #ident::table_name()));
                    #(#entity_search_fields_where_clause)*
                }
            }

            impl #ident {
                pub(crate) async fn fetch_optional_new<'a, 'c, E>(search: #entity_search_ident<'a>,  executor: E) -> Result<Option<Self>, crate::entity::EntityError>
                where
                    E: sqlx::Executor<'c, Database = sqlx::Postgres>,
                {
                    let mut query_builder = sqlx::QueryBuilder::new("");
                    search.query_builder(&mut query_builder);
                    query_builder.build_query_as().fetch_optional(executor).await.map_err(Into::into)
                }

                pub(crate) async fn fetch_one_new<'a, 'c, E>(search: #entity_search_ident<'a>,  executor: E) -> Result<Self, crate::entity::EntityError>
                where
                    E: sqlx::Executor<'c, Database = sqlx::Postgres>,
                {
                   Self::fetch_optional_new(search, executor)
                    .await
                    .and_then(|optional| optional.ok_or(sqlx::Error::RowNotFound).map_err(Into::into))
                }

                pub(crate) async fn insert_or_update<'c, E>(self, executor: E) -> Result<bool, crate::entity::EntityError>
                where
                    E: sqlx::Executor<'c, Database = sqlx::Postgres>,
                {
                    sqlx::query_as(#insert_or_update_sql)
                        #(#insert_bindings_cloned)*
                        .fetch_optional(executor)
                        .await
                        .map_err(Into::into)
                        .map(|optional: Option<crate::entity::EntityInserted>| optional.map(|return_row| return_row.is_inserted()).unwrap_or_default())
                }
            }

            impl #impl_generics crate::entity::Entity for #ident #type_generics #where_clause {
                fn table_name() -> &'static str {
                    #table_name
                }

                fn order_by() -> Option<&'static str> {
                    #order_by_tokens
                }

                async fn insert_one<'c, E>(self, executor: E) -> Result<(), crate::entity::EntityError>
                where
                    E: sqlx::Executor<'c, Database = sqlx::Postgres>,
                {
                    sqlx::query(#insert_one_sql)
                        #(#insert_bindings)*
                        .execute(executor)
                        .await
                        .map_err(Into::into)
                        .map(drop)
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
#[entity(table_name="fake_table")]
struct FakeEntity {
    #[entity(field_type="primary")]
    id: Uuid,
    #[entity(field_type="searchable")]
    #[entity(order_by="desc")]
    name: String,
    #[entity(order_by="asc")]
    age: i32,
    data: GzipStrictType<Foo<Bar<Demo>>>,
    data2: GzipStrictType<String>,
}
"#;
        let parsed = syn::parse_str(input).unwrap();
        let receiver = EntityBuilderStructReceiver::from_derive_input(&parsed).unwrap();
        let tokens = receiver.render_entity();
        println!("{tokens}");
        let formatted_code = prettyplease::unparse(&syn::parse2(tokens).unwrap());
        println!("entity tokens:\n{}\n", formatted_code);
        let tokens = receiver.render_rgb_entity();
        let formatted_code = prettyplease::unparse(&syn::parse2(tokens).unwrap());
        println!("rgb entity tokens:\n{}\n", formatted_code);
    }
}
