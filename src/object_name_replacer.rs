use super::sql_ast_traversal::{
    ast_views::{
        CreateIndexStatementViewMutable, CreateTableStatementViewMutable, DropStatementViewMutable,
        InsertStatementViewMutable, SetOperationViewMutable, TableFactorDerivedViewMut,
        TableFactorTableViewMut, UpdateStatementViewMutable,
    },
    helpers::{extract_binary_identifiers, extract_unary_identifier},
    traverser::VisitResult,
};
use crate::sql_ast_traversal::traverser::SqlAstTraverser;
use sqlparser::ast::{
    self, Assignment, FunctionArgExpr, HiveDistributionStyle, HiveFormat, Ident, JoinConstraint,
    ObjectName, ObjectType, OnConflict, TableAlias, Value,
};
use std::collections::HashMap;

type RelationsToReplace = HashMap<Vec<String>, Vec<String>>;

type TableReference = String;
type ReplacementIdentifiers = Vec<Ident>;
type ReferencableTables = HashMap<TableReference, ReplacementIdentifiers>;

#[derive(Debug)]
struct Scope {
    referencable_tables: ReferencableTables,
}

impl Scope {
    fn new() -> Self {
        Scope {
            referencable_tables: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct ObjectNamesToReplace {
    indices_to_replace: HashMap<String, String>,
    relations_to_replace: RelationsToReplace,
}

#[derive(Debug)]
pub struct ObjectNameReplacer<'a> {
    object_names_to_replace: &'a ObjectNamesToReplace,
    scopes: Vec<Scope>,
}

impl<'a> ObjectNameReplacer<'a> {
    pub fn _new(object_names_to_replace: &'a ObjectNamesToReplace) -> ObjectNameReplacer<'a> {
        ObjectNameReplacer {
            object_names_to_replace,
            scopes: vec![Scope::new()],
        }
    }
}

impl SqlAstTraverser for ObjectNameReplacer<'_> {
    fn post_visit_drop(&mut self, drop: &mut DropStatementViewMutable) -> VisitResult {
        let DropStatementViewMutable {
            cascade: _,
            if_exists: _,
            names,
            object_type,
            purge: _,
            restrict: _,
            temporary: _,
        } = drop;

        match object_type {
            ObjectType::Table | ObjectType::View => (),

            // TODO: implement index
            // TODO: test these cases
            ObjectType::Index | ObjectType::Schema | ObjectType::Role => return Ok(()),

            // I don't think these are "relations", but I don't know what they actually are. so..
            ObjectType::Sequence => return Err("not implemented: Drop::Sequence".to_string()),
            ObjectType::Stage => return Err("not implemented: Drop::Stage".to_string()),
        }

        let new_names = names
            .iter()
            .map(|name| {
                Ok(ObjectName(get_replacement_identifiers(
                    name,
                    &self.object_names_to_replace.relations_to_replace,
                )?))
            })
            .collect::<Result<Vec<_>, String>>()?;

        **names = new_names;

        Ok(())
    }

    fn pre_visit_update(&mut self, _update: &mut UpdateStatementViewMutable) -> VisitResult {
        self.scopes.push(Scope::new());
        Ok(())
    }

    fn post_visit_update(&mut self, _update: &mut UpdateStatementViewMutable) -> VisitResult {
        self.scopes.pop();
        Ok(())
    }

    fn post_visit_assignment(&mut self, assignment: &mut Assignment) -> VisitResult {
        let Assignment { id, value: _ } = assignment;
        extract_unary_identifier(id, "column")?;
        Ok(())
    }

    fn pre_visit_create_table(
        &mut self,
        create_table: &mut CreateTableStatementViewMutable,
    ) -> VisitResult {
        let CreateTableStatementViewMutable {
            if_not_exists: _,
            name: _,
            columns: _,
            auto_increment_offset: _,
            clone,
            cluster_by,
            collation: _,
            comment,
            constraints: _,
            default_charset: _,
            engine,
            external,
            file_format,
            global,
            hive_distribution,
            hive_formats,
            like,
            location,
            on_cluster,
            on_commit: _,
            options: _,
            or_replace: _,
            order_by,
            partition_by: _,
            query: _,
            strict: _,
            table_properties: _,
            temporary,
            transient,
            with_options: _,
            without_rowid,
        } = create_table;

        if clone.is_some() {
            return Err("not implemented: CreateTable::clone".to_string());
        }

        if cluster_by.is_some() {
            return Err("not implemented: CreateTable::cluster_by".to_string());
        }

        if comment.is_some() {
            return Err("not implemented: CreateTable::comment".to_string());
        }

        if engine.is_some() {
            return Err("not implemented: CreateTable::engine".to_string());
        }

        if **external {
            return Err("not implemented: CreateTable::external".to_string());
        }

        if file_format.is_some() {
            return Err("not implemented: CreateTable::file_format".to_string());
        }

        if global.is_some() {
            return Err("not implemented: CreateTable::global".to_string());
        }

        if **hive_distribution != HiveDistributionStyle::NONE {
            return Err("not implemented: CreateTable::hive_distribution".to_string());
        }

        if let Some(HiveFormat {
            location,
            row_format,
            storage,
        }) = hive_formats
        {
            if location.is_some() || row_format.is_some() || storage.is_some() {
                return Err("not implemented: CreateTable::hive_formats".to_string());
            }
        }

        if like.is_some() {
            return Err("not implemented: CreateTable::like".to_string());
        }

        if location.is_some() {
            return Err("not implemented: CreateTable::location".to_string());
        }

        if on_cluster.is_some() {
            return Err("not implemented: CreateTable::on_cluster".to_string());
        }

        if order_by.is_some() {
            return Err("not implemented: CreateTable::order_by".to_string());
        }

        if **temporary {
            return Err("not implemented: CreateTable::temporary".to_string());
        }

        if **transient {
            return Err("not implemented CreateTable::transient".to_string());
        }

        if **without_rowid {
            return Err("not implemented: CreateTable::without_rowid".to_string());
        }

        Ok(())
    }

    fn post_visit_create_table(
        &mut self,
        create_table: &mut CreateTableStatementViewMutable,
    ) -> VisitResult {
        *create_table.name = ObjectName(get_replacement_identifiers(
            create_table.name,
            &self.object_names_to_replace.relations_to_replace,
        )?);

        Ok(())
    }

    fn post_visit_create_index(
        &mut self,
        create_index: &mut CreateIndexStatementViewMutable,
    ) -> VisitResult {
        let CreateIndexStatementViewMutable {
            columns: _,
            name,
            table_name,
            unique: _,
            if_not_exists: _,
            nulls_distinct: _,
            concurrently: _,
            include,
            predicate: _,
            using,
        } = create_index;

        if !include.is_empty() {
            return Err("not implemented: CreateIndex::include".to_string());
        }

        if using.is_some() {
            return Err("not implemented: CreateIndex::using".to_string());
        }

        let ObjectName(name_identifiers) =
            name.as_ref().ok_or("Index name is required".to_string())?;

        let old_index_name = extract_unary_identifier(name_identifiers, "index")?;

        let new_index_name = self
            .object_names_to_replace
            .indices_to_replace
            .get(&old_index_name)
            .ok_or(format!("Unexpected index name: {}", old_index_name))?;

        let ObjectName(table_name_identifiers) = table_name;
        extract_unary_identifier(table_name_identifiers, "index table")?;

        let mut converted_table_reference = get_replacement_identifiers(
            table_name,
            &self.object_names_to_replace.relations_to_replace,
        )?;

        // TODO: change the model so that this check isn't needed
        if converted_table_reference.len() < 1 || converted_table_reference.len() > 2 {
            return Err("Expected 1 or 2 identifiers for the table reference".to_string());
        }

        let new_table_name = converted_table_reference.pop().unwrap();
        let new_schema_name = converted_table_reference.pop();

        **name = Some(if let Some(schema_name) = new_schema_name {
            ObjectName(vec![schema_name, Ident::new(new_index_name)])
        } else {
            ObjectName(vec![Ident::new(new_index_name)])
        });

        **table_name = ObjectName(vec![new_table_name]);

        Ok(())
    }

    fn pre_visit_insert(&mut self, insert: &mut InsertStatementViewMutable) -> VisitResult {
        let InsertStatementViewMutable {
            columns: _,
            table_name: _,
            table_alias: _,
            ignore: _,
            into: _,
            or: _,
            overwrite: _,
            replace_into: _,
            table: _,
            source: _,
            after_columns,
            on: _,
            partitioned: _,
            priority,
            returning: _,
        } = insert;

        if !after_columns.is_empty() {
            return Err("not implemented: Insert::after_columns".to_string());
        }

        if priority.is_some() {
            return Err("not implemented: Insert::priority".to_string());
        }

        Ok(())
    }

    fn post_visit_insert(&mut self, insert: &mut InsertStatementViewMutable) -> VisitResult {
        let InsertStatementViewMutable {
            after_columns: _,
            columns: _,
            ignore: _,
            into: _,
            on: _,
            or: _,
            overwrite: _,
            partitioned: _,
            priority: _,
            replace_into: _,
            returning: _,
            source: _,
            table_alias: _,
            table_name,
            table: _,
        } = insert;

        **table_name = ObjectName(get_replacement_identifiers(
            table_name,
            &self.object_names_to_replace.relations_to_replace,
        )?);

        Ok(())
    }

    fn pre_visit_ast_query(&mut self, _query: &mut Box<ast::Query>) -> VisitResult {
        self.scopes.push(Scope::new());
        Ok(())
    }

    fn post_visit_ast_query(&mut self, _query: &mut Box<ast::Query>) -> VisitResult {
        self.scopes.pop();
        Ok(())
    }

    fn pre_visit_set_operation_left(&mut self, _body: &mut SetOperationViewMutable) -> VisitResult {
        self.scopes.push(Scope::new());
        Ok(())
    }

    fn pre_visit_set_operation_right(
        &mut self,
        _body: &mut SetOperationViewMutable,
    ) -> VisitResult {
        self.scopes.pop();
        self.scopes.push(Scope::new());
        Ok(())
    }

    fn post_visit_set_operation(&mut self, _body: &mut SetOperationViewMutable) -> VisitResult {
        self.scopes.pop();
        Ok(())
    }

    fn pre_visit_table_factor_table(
        &mut self,
        relation: &mut TableFactorTableViewMut,
    ) -> VisitResult {
        let TableFactorTableViewMut {
            alias: _,
            args: _,
            name: _,
            partitions,
            version: _,
            with_hints: _,
        } = relation;

        if !partitions.is_empty() {
            return Err("not implemented: TableFactor::Table::partitions".to_string());
        }

        Ok(())
    }

    fn post_visit_table_factor_table(
        &mut self,
        relation: &mut TableFactorTableViewMut,
    ) -> VisitResult {
        let TableFactorTableViewMut {
            alias,
            args: _,
            name: table_name,
            partitions: _,
            version: _,
            with_hints: _,
        } = relation;

        let scope = self
            .scopes
            .last_mut()
            .ok_or("Expected scope to exist in traverse_table_factor")?;

        add_to_referencable_tables(
            &Some(&table_name),
            alias,
            &self.object_names_to_replace.relations_to_replace,
            &mut scope.referencable_tables,
        )?;

        **table_name = ObjectName(get_replacement_identifiers(
            table_name,
            &self.object_names_to_replace.relations_to_replace,
        )?);

        Ok(())
    }

    fn post_visit_table_factor_derived(
        &mut self,
        relation: &mut TableFactorDerivedViewMut,
    ) -> VisitResult {
        let TableFactorDerivedViewMut {
            alias,
            lateral: _,
            subquery: _,
        } = relation;

        let scope = self
            .scopes
            .last_mut()
            .ok_or("Expected scope to exist in traverse_table_factor_derived")?;

        add_to_referencable_tables(
            &None,
            alias,
            &self.object_names_to_replace.relations_to_replace,
            &mut scope.referencable_tables,
        )?;

        Ok(())
    }

    fn post_visit_compound_identifier(&mut self, identifiers: &mut Vec<Ident>) -> VisitResult {
        let (table_reference, column_name) = extract_binary_identifiers(identifiers, "expression")?;

        let table_replacement_identifiers =
            get_replacement_identifiers_for_table(&self.scopes, &table_reference)?;

        let mut new_identifiers = table_replacement_identifiers.clone();
        new_identifiers.push(Ident::new(&column_name));

        *identifiers = new_identifiers;

        Ok(())
    }

    fn pre_visit_on_conflict(&mut self, on_conflict: &mut OnConflict) -> VisitResult {
        let OnConflict {
            action: _,
            conflict_target,
        } = on_conflict;

        if conflict_target.is_some() {
            return Err("not implemented: OnConflict::conflict_target".to_string());
        }

        Ok(())
    }

    fn pre_visit_join_constraint(&mut self, constraint: &mut JoinConstraint) -> VisitResult {
        if let JoinConstraint::Using(_) = constraint {
            return Err("not implemented: JoinConstraint::Using".to_string());
        }

        Ok(())
    }

    fn pre_visit_function_arg_expr(
        &mut self,
        function_arg_expr: &mut FunctionArgExpr,
    ) -> VisitResult {
        if let FunctionArgExpr::QualifiedWildcard(_) = function_arg_expr {
            return Err("Unhandled syntax: FunctionArgExpr::QualifiedWildcard".to_string());
        }

        Ok(())
    }

    fn pre_visit_value(&mut self, value: &mut Value) -> VisitResult {
        match value {
            Value::Placeholder(value) => {
                if value == "?" {
                    Ok(())
                } else {
                    Err(format!("Unhandled value: Value::Placeholder({})", value))
                }
            }

            Value::SingleQuotedString(_) | Value::Number(_, _) | Value::Null => Ok(()),

            Value::DollarQuotedString(_) => {
                Err("Unhandled value: Value::DollarQuotedString".to_string())
            }
            Value::EscapedStringLiteral(_) => {
                Err("Unhandled value: Value::EscapedStringLiteral".to_string())
            }
            Value::SingleQuotedByteStringLiteral(_) => {
                Err("Unhandled value: Value::SingleQuotedByteStringLiteral".to_string())
            }
            Value::DoubleQuotedByteStringLiteral(_) => {
                Err("Unhandled value: Value::DoubleQuotedByteStringLiteral".to_string())
            }
            Value::RawStringLiteral(_) => {
                Err("Unhandled value: Value::RawStringLiteral".to_string())
            }
            Value::NationalStringLiteral(_) => {
                Err("Unhandled value: Value::NationalStringLiteral".to_string())
            }
            Value::HexStringLiteral(_) => {
                Err("Unhandled value: Value::HexStringLiteral".to_string())
            }
            Value::DoubleQuotedString(_) => {
                Err("Unhandled value: Value::DoubleQuotedString".to_string())
            }
            Value::Boolean(_) => Err("Unhandled value: Value::Boolean".to_string()),
            Value::UnQuotedString(_) => Err("Unhandled value: Value::UnQuotedString".to_string()),
        }?;

        Ok(())
    }
}

fn get_replacement_identifiers(
    ObjectName(identifiers): &ObjectName,
    relations_to_replace: &RelationsToReplace,
) -> Result<Vec<Ident>, String> {
    let output_ident_values = relations_to_replace
        .get(
            &identifiers
                .iter()
                .map(|ident| ident.value.clone())
                .collect::<Vec<_>>(),
        )
        .ok_or("Unexpected relation".to_string())?;

    Ok(output_ident_values
        .into_iter()
        .map(|s| Ident::new(s))
        .collect())
}

fn add_to_referencable_tables(
    table_name: &Option<&ObjectName>,
    alias: &Option<TableAlias>,
    relations_to_replace: &RelationsToReplace,
    referencable_tables: &mut ReferencableTables,
) -> Result<(), String> {
    match (table_name, alias) {
        (
            _,
            Some(TableAlias {
                columns,
                name: alias_ident,
            }),
        ) => {
            if !columns.is_empty() {
                return Err("not implemented: TableAlias::columns".to_string());
            }

            let Ident { value, .. } = alias_ident;

            if referencable_tables.contains_key(value) {
                return Err(format!("Duplicate table alias: {}", value));
            }

            referencable_tables.insert(value.clone(), vec![alias_ident.to_owned()]);
        }
        (Some(table_name), None) => {
            let ObjectName(name_identifiers) = table_name;

            let path = extract_unary_identifier(name_identifiers, "table")?;

            let db_reference = get_replacement_identifiers(table_name, relations_to_replace)?;

            if referencable_tables.contains_key(&path) {
                return Err(format!("Duplicate table name: {}", path));
            }

            referencable_tables.insert(path, db_reference);
        }
        (None, None) => (),
    };

    Ok(())
}

fn get_replacement_identifiers_for_table<'a>(
    scopes: &'a Vec<Scope>,
    table_reference: &str,
) -> Result<&'a Vec<Ident>, String> {
    for scope in scopes.iter().rev() {
        if let Some(replacement_identifiers) = scope.referencable_tables.get(table_reference) {
            return Ok(replacement_identifiers);
        }
    }

    Err(format!(
        "Unknown table reference: {} ({:?})",
        table_reference, scopes
    ))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use sqlformat::FormatOptions;
    use sqlparser::{ast::Statement, dialect::SQLiteDialect, parser::Parser};

    use super::*;

    #[derive(serde::Serialize)]
    struct ObjectNamesToReplaceBTree {
        indices_to_replace: BTreeMap<String, String>,
        relations_to_replace: BTreeMap<Vec<String>, Vec<String>>,
    }

    #[derive(serde::Serialize)]
    struct Report {
        original_query: Vec<String>,
        names_to_replace: ObjectNamesToReplaceBTree,
        output_query: Vec<String>,
        output_ast: Vec<Statement>,
    }

    fn translate_sql(
        query: &str,
        names_to_replace: &ObjectNamesToReplace,
    ) -> Result<Report, String> {
        let dialect = SQLiteDialect {};

        let mut ast = Parser::parse_sql(&dialect, query).map_err(|e| e.to_string())?;

        let mut object_name_replacer = ObjectNameReplacer::_new(names_to_replace);

        object_name_replacer.traverse(&mut ast)?;

        Ok(Report {
            original_query: query
                .trim()
                .split('\n')
                .map(|s| s.trim_start_matches("            "))
                .map(|s| s.to_string())
                .collect(),
            names_to_replace: ObjectNamesToReplaceBTree {
                indices_to_replace: names_to_replace
                    .indices_to_replace
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect(),
                relations_to_replace: names_to_replace
                    .relations_to_replace
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect(),
            },
            output_query: sqlformat::format(
                &ast.iter().map(|s| s.to_string()).collect::<String>(),
                &sqlformat::QueryParams::None,
                FormatOptions::default(),
            )
            .split('\n')
            .map(|s| s.to_string())
            .collect(),
            output_ast: ast,
        })
    }

    #[test]
    fn create_table() {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS "~/books/things" (
                "excluded" BOOLEAN NOT NULL DEFAULT FALSE,
                "externalLink" TEXT NOT NULL,
                "externalLinkTitle" TEXT NOT NULL,
                "id" TEXT PRIMARY KEY NOT NULL,
                "imageUrl" TEXT NOT NULL,
                "subtitle" TEXT NULL,
                "title" TEXT NOT NULL
            )
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/things".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_table_using_parent() {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS "~/books/bings/../things" (
                "excluded" BOOLEAN NOT NULL DEFAULT FALSE,
                "externalLink" TEXT NOT NULL,
                "externalLinkTitle" TEXT NOT NULL,
                "id" TEXT PRIMARY KEY NOT NULL,
                "imageUrl" TEXT NOT NULL,
                "subtitle" TEXT NULL,
                "title" TEXT NOT NULL
            )
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/bings/../things".to_string()],
                vec!["my_books".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_table_using_curdir() {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS "~/books/./things/." (
                "excluded" BOOLEAN NOT NULL DEFAULT FALSE,
                "externalLink" TEXT NOT NULL,
                "externalLinkTitle" TEXT NOT NULL,
                "id" TEXT PRIMARY KEY NOT NULL,
                "imageUrl" TEXT NOT NULL,
                "subtitle" TEXT NULL,
                "title" TEXT NOT NULL
            )
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/./things/.".to_string()],
                vec!["my_books".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_table_compound_identifier() {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS x.y (
                "excluded" BOOLEAN NOT NULL DEFAULT FALSE,
                "externalLink" TEXT NOT NULL,
                "externalLinkTitle" TEXT NOT NULL,
                "id" TEXT PRIMARY KEY NOT NULL,
                "imageUrl" TEXT NOT NULL,
                "subtitle" TEXT NULL,
                "title" TEXT NOT NULL
            )
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["x".to_string(), "y".to_string()],
                vec!["my_books".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn unsupported_statement_type() {
        let sql = r#"close my_eyes;"#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::new(),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_table_outside_home() {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS "sasas" (
                "excluded" BOOLEAN NOT NULL DEFAULT FALSE,
                "externalLink" TEXT NOT NULL,
                "externalLinkTitle" TEXT NOT NULL,
                "id" TEXT PRIMARY KEY NOT NULL,
                "imageUrl" TEXT NOT NULL,
                "subtitle" TEXT NULL,
                "title" TEXT NOT NULL
            )
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (vec!["sasas".to_string()], vec!["asasa".to_string()]),
                (vec!["asasa".to_string()], vec!["sasas".to_string()]),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_table_root() {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS "/dev/null" (
                "excluded" BOOLEAN NOT NULL DEFAULT FALSE,
                "externalLink" TEXT NOT NULL,
                "externalLinkTitle" TEXT NOT NULL,
                "id" TEXT PRIMARY KEY NOT NULL,
                "imageUrl" TEXT NOT NULL,
                "subtitle" TEXT NULL,
                "title" TEXT NOT NULL
            )
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["/dev/null".to_string()],
                vec!["dev_null".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_table_outside_home_using_parent() {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS "~/hi/../../etc/passwd" (
                "excluded" BOOLEAN NOT NULL DEFAULT FALSE,
                "externalLink" TEXT NOT NULL,
                "externalLinkTitle" TEXT NOT NULL,
                "id" TEXT PRIMARY KEY NOT NULL,
                "imageUrl" TEXT NOT NULL,
                "subtitle" TEXT NULL,
                "title" TEXT NOT NULL
            )
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/hi/../../etc/passwd".to_string()],
                vec!["etc".to_string(), "passwd".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_table_multiple() {
        let sql = r#"
            CREATE TABLE IF NOT EXISTS "~/books/things" (
                "excluded" BOOLEAN NOT NULL DEFAULT FALSE,
                "externalLink" TEXT NOT NULL,
                "externalLinkTitle" TEXT NOT NULL,
                "id" TEXT PRIMARY KEY NOT NULL,
                "imageUrl" TEXT NOT NULL,
                "subtitle" TEXT NULL,
                "title" TEXT NOT NULL
            );
            CREATE TABLE IF NOT EXISTS "~/books/thongs" (
                "title" TEXT NOT NULL
            );
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (
                    vec!["~/books/things".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
                (
                    vec!["~/books/thongs".to_string()],
                    vec!["my_books2".to_string(), "tbl".to_string()],
                ),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_index() {
        let sql = r#"CREATE INDEX "scoreIndex" ON "~/books/eloScores" (score)"#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::from_iter([(
                "scoreIndex".to_string(),
                "eloScoresIndex".to_string(),
            )]),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/eloScores".to_string()],
                vec!["scores".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_index_compound_name() {
        let sql = r#"CREATE INDEX a.b ON "~/books/eloScores" (score)"#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::from_iter([(
                "b".to_string(),
                "eloScoresIndex".to_string(),
            )]),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/eloScores".to_string()],
                vec!["scores".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_index_compound_table_name() {
        let sql = r#"CREATE INDEX a ON x.y (score)"#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::from_iter([(
                "a".to_string(),
                "eloScoresIndex".to_string(),
            )]),
            relations_to_replace: HashMap::from_iter([(
                vec!["x".to_string(), "y".to_string()],
                vec!["scores".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn insert() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP)
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/matches".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn insert_from_select() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            SELECT "~/books/matches"."id" || '2', "loserId", "~/books/matches"."winnerId", "matchDate" FROM "~/books/matches"
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/matches".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn parser_error() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP('derp))
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/matches".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn select1_with_alias() {
        let sql = r#"
            SELECT
                things.id, things.title, things.subtitle, things.image_url, things.external_link,
                things.external_link_title, things.excluded
            FROM "~/books/things" AS "things"
            WHERE things.id = ?
            LIMIT 1
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (
                    vec!["~/books/things".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
                (
                    vec!["~/something/else/for/fun".to_string()],
                    vec!["hola".to_string(), "tbl".to_string()],
                ),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn select1() {
        let sql = r#"
            SELECT
                "~/books/things".id, "~/books/things".title, "~/books/things".subtitle,
                "~/books/things".image_url, "~/books/things".external_link,
                "~/books/things".external_link_title, "~/books/things".excluded
            FROM "~/books/things"
            WHERE "~/books/things".id = ?
            LIMIT 1
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/things".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn select_with_join() {
        let sql = r#"
            SELECT
                winner.score AS "winnerScore", loser.score AS "loserScore"
            FROM "~/books/eloScores" AS "winner"
            INNER JOIN "~/books/eloScores" AS "loser"
            WHERE
                winner.thing_id = ?
                AND loser.thing_id = ?
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/eloScores".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn select_null() {
        let sql = "SELECT null";

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/eloScores".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn update() {
        let sql = r#"UPDATE "~/books/eloScores" SET "score" = ? WHERE "thingId" = ?"#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/eloScores".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn subquery() {
        let sql = r#"
            SELECT
                loserId, winnerId
            FROM "~/books/matches"
            WHERE loserId IN (
                SELECT thingId
                FROM "~/books/eloScores"
                ORDER BY score DESC, thingId DESC
                LIMIT 15
            )
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (
                    vec!["~/books/matches".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
                (
                    vec!["~/books/eloScores".to_string()],
                    vec!["super".to_string(), "scores".to_string(), "tbl".to_string()],
                ),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn union_in_subquery() {
        let sql = r#"
            SELECT
                sq.id, sq.num_matches
            FROM (
                SELECT books.id, 0 AS "num_matches"
                FROM "~/books/things" AS "books"
                LEFT JOIN "~/books/matches" AS "matches"
                ON (
                    books.id = matches.winner_id
                    OR books.id = matches.loser_id
                )
                WHERE matches.loser_id IS NULL

                UNION ALL

                SELECT winner_id AS "id", 1 AS "num_matches"
                FROM "~/books/matches"

                UNION ALL

                SELECT loser_id AS "id", 1 AS "num_matches"
                FROM "~/books/matches"
            ) AS "sq"
            GROUP BY sq.id
            ORDER BY sum(sq.num_matches) ASC
            LIMIT ?
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (
                    vec!["~/books/things".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
                (
                    vec!["~/books/matches".to_string()],
                    vec!["my_books".to_string(), "matches".to_string()],
                ),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn a_function() {
        let sql = "SELECT badfunc()";

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/books/eloScores".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(result);
    }

    #[test]
    fn another_select() {
        let sql = r#"
            SELECT
                COUNT(*) AS "rank0"
            FROM "~/books/eloScores" AS "eloScores"
            JOIN "~/books/eloScores" AS "comparisonScore"
                ON eloScores.thingId = ?
            JOIN "~/books/things" AS "books"
                ON books.id = eloScores.thingId
            WHERE
                books.excluded = ?
                AND (
                    eloScores.score > comparisonScore.score
                    OR (
                        eloScores.score = comparisonScore.score
                        AND eloScores.thingId > comparisonScore.thingId
                    )
                )
            LIMIT ? OFFSET ?
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (
                    vec!["~/books/eloScores".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
                (
                    vec!["~/books/things".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn and_another() {
        let sql = r#"
            select "id"
            from (
                select
                    "~/books/things"."id" as "id",
                    0 as "num_matches"
                from "~/books/things"
                left join "~/books/matches" on (
                    "~/books/matches"."winner_id" = "~/books/things"."id"
                    or "~/books/matches"."loser_id" = "~/books/things"."id"
                )
                where "~/books/matches"."loser_id" is null

                union all

                select "winner_id" as "id", 1 as "num_matches"
                from "~/books/matches"

                union all

                select "loser_id" as "id", 1 as "num_matches"
                from "~/books/matches"
            ) as "sq"
            group by "id"
            order by sum("num_matches")
            limit ?
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (
                    vec!["~/books/things".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
                (
                    vec!["~/books/matches".to_string()],
                    vec!["my_books".to_string(), "matches".to_string()],
                ),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn select_wildcard() {
        let sql = r#"select * from "~/heyy"."/is"."/for"."/horses"#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec![
                    "~/heyy".to_string(),
                    "/is".to_string(),
                    "/for".to_string(),
                    "/horses".to_string(),
                ],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn drop_table() {
        let sql = r#"drop table "~/heyy""#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/heyy".to_string()],
                vec!["my_books".to_string(), "tbl".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn drop_multiple_tables() {
        let sql = r#"drop table "~/heyy", "~/okokok""#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (
                    vec!["~/heyy".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
                (vec!["~/okokok".to_string()], vec!["okokok".to_string()]),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn on_conflict_do_nothing() {
        let sql = r#"
            insert into "~/my-data-scraper/reelgood/shows-and-movies"
            ("format", "isWatched", "name", "url", "imageUrl")
            values (?, ?, ?, ?, ?)
            on conflict do nothing
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([(
                vec!["~/my-data-scraper/reelgood/shows-and-movies".to_string()],
                vec!["shows".to_string(), "and".to_string(), "movies".to_string()],
            )]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn union_separate_scopes() {
        let sql = r#"
            select "~/books/things".id, "~/books/things".title
            from "~/books/non-things" as "~/books/things"

            union

            select "~/books/things".id, "~/books/things".title
            from "~/books/things"
        "#;

        let names_to_replace = ObjectNamesToReplace {
            indices_to_replace: HashMap::new(),
            relations_to_replace: HashMap::from_iter([
                (
                    vec!["~/books/things".to_string()],
                    vec!["my_books".to_string(), "tbl".to_string()],
                ),
                (
                    vec!["~/books/non-things".to_string()],
                    vec!["non_books".to_string(), "tbl".to_string()],
                ),
            ]),
        };
        let translate_result = translate_sql(sql, &names_to_replace);
        insta::assert_yaml_snapshot!(translate_result);
    }
}
