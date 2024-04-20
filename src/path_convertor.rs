use super::sql_ast_traversal::{
    ast_views::{
        CreateIndexStatementViewMutable, CreateTableStatementViewMutable, DropStatementViewMutable,
        InsertStatementViewMutable, SetOperationViewMutable, TableFactorDerivedViewMut,
        TableFactorTableViewMut, UpdateStatementViewMutable,
    },
    helpers::{extract_binary_identifiers, extract_unary_identifier},
    traverser::VisitResult,
};
use crate::{
    sql_ast_traversal::traverser::SqlAstTraverser, VALUES_TABLE_INDEX_PREFIX, VALUES_TABLE_NAME,
};
use sqlparser::ast::{
    self, Assignment, Function, FunctionArgExpr, HiveDistributionStyle, HiveFormat, Ident,
    JoinConstraint, ObjectName, ObjectType, OnConflict, TableAlias, Value,
};
use std::collections::HashMap;
use typed_path::Utf8UnixPathBuf;

pub type DatabaseNamesByPath = HashMap<String, String>;

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

pub struct PathConvertor {
    pub database_names: DatabaseNamesByPath,
    scopes: Vec<Scope>,
}

impl PathConvertor {
    pub fn new() -> Self {
        PathConvertor {
            database_names: HashMap::new(),
            scopes: vec![Scope::new()],
        }
    }
}

impl SqlAstTraverser for PathConvertor {
    fn post_visit_drop(&mut self, drop: &mut DropStatementViewMutable) -> VisitResult {
        let DropStatementViewMutable {
            cascade: _,
            if_exists: _,
            names,
            object_type: _,
            purge: _,
            restrict: _,
            temporary: _,
        } = drop;

        let new_names = names
            .iter()
            .map(|name| {
                let db_reference = convert_path_to_database(name, &mut self.database_names)?;
                Ok(ObjectName(get_qualified_values_table_identifiers(
                    &db_reference,
                )))
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
        let db_reference = convert_path_to_database(create_table.name, &mut self.database_names)?;

        *create_table.name = ObjectName(get_qualified_values_table_identifiers(&db_reference));

        Ok(())
    }

    fn pre_visit_create_index(
        &mut self,
        create_index: &mut CreateIndexStatementViewMutable,
    ) -> VisitResult {
        let CreateIndexStatementViewMutable {
            columns: _,
            name,
            table_name: _,
            unique: _,
            if_not_exists: _,
            nulls_distinct: _,
            concurrently,
            include,
            predicate: _,
            using,
        } = create_index;

        if name.is_none() {
            return Err("Index name is required".to_string());
        }

        if **concurrently {
            return Err("not implemented: CreateIndex::concurrently".to_string());
        }

        if !include.is_empty() {
            return Err("not implemented: CreateIndex::include".to_string());
        }

        if using.is_some() {
            return Err("not implemented: CreateIndex::using".to_string());
        }

        Ok(())
    }

    fn post_visit_create_index(
        &mut self,
        create_index: &mut CreateIndexStatementViewMutable,
    ) -> VisitResult {
        let ObjectName(name_identifiers) = create_index.name.as_ref().unwrap();

        let index_name = extract_unary_identifier(name_identifiers, "index")?;

        let translated_db_name =
            convert_path_to_database(&create_index.table_name, &mut self.database_names)?;

        *create_index.name = Some(ObjectName(vec![
            Ident::new(translated_db_name),
            Ident::new(format!("{}{}", VALUES_TABLE_INDEX_PREFIX, index_name)),
        ]));
        *create_index.table_name = ObjectName(vec![Ident::new(VALUES_TABLE_NAME)]);

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

        let translated_db_name = convert_path_to_database(table_name, &mut self.database_names)?;

        **table_name = ObjectName(get_qualified_values_table_identifiers(&translated_db_name));

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

        let db_reference = convert_path_to_database(table_name, &mut self.database_names)?;

        let scope = self
            .scopes
            .last_mut()
            .ok_or("Expected scope to exist in traverse_table_factor")?;

        add_to_referencable_tables(
            &Some(&table_name),
            alias,
            &mut self.database_names,
            &mut scope.referencable_tables,
        )?;

        **table_name = ObjectName(get_qualified_values_table_identifiers(&db_reference));

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
            &mut self.database_names,
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

    fn pre_visit_function(&mut self, func: &mut Function) -> VisitResult {
        let Function {
            args: _,
            distinct,
            filter: _,
            name,
            null_treatment,
            order_by: _,
            over: _,
            // "special" apparently means the function's parentheses are omitted
            special: _,
        } = func;

        if *distinct {
            return Err("Unhandled syntax: Function::distinct".to_string());
        }

        if null_treatment.is_some() {
            return Err("Unhandled syntax: Function::null_treatment".to_string());
        }

        validate_function_name(&name)
    }

    fn pre_visit_drop(&mut self, drop: &mut DropStatementViewMutable) -> VisitResult {
        let DropStatementViewMutable {
            cascade,
            if_exists: _,
            names: _,
            object_type,
            purge,
            restrict,
            temporary,
        } = drop;

        match object_type {
            ObjectType::Table => {}
            ObjectType::View => Err("not implemented: Statement::Drop::View".to_string())?,
            ObjectType::Index => Err("not implemented: Statement::Drop::Index".to_string())?,
            ObjectType::Schema => Err("not implemented: Statement::Drop::Schema".to_string())?,
            ObjectType::Role => Err("not implemented: Statement::Drop::Role".to_string())?,
            ObjectType::Sequence => Err("not implemented: Statement::Drop::Sequence".to_string())?,
            ObjectType::Stage => Err("not implemented: Statement::Drop::Stage".to_string())?,
        };

        if **cascade {
            Err("not implemented: Statement::Drop::cascade".to_string())?;
        };

        if **purge {
            Err("not implemented: Statement::Drop::purge".to_string())?;
        };

        if **restrict {
            Err("not implemented: Statement::Drop::restrict".to_string())?;
        };

        if **temporary {
            Err("not implemented: Statement::Drop::temporary".to_string())?;
        };

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

fn validate_function_name(name: &ObjectName) -> Result<(), String> {
    let ObjectName(name_identifiers) = name;

    let unary_name = extract_unary_identifier(name_identifiers, "function")?;

    let supported_functions = ["COUNT", "CURRENT_TIMESTAMP", "DATETIME", "SUM"];

    if supported_functions
        .iter()
        .any(|&func| func.eq_ignore_ascii_case(&unary_name))
    {
        Ok(())
    } else {
        Err(format!("Unsupported function: {}", unary_name))
    }
}

fn resolve_translated_db_name(databases: &mut DatabaseNamesByPath, path: &str) -> String {
    if databases.contains_key(path) {
        databases[path].clone()
    } else {
        let translated_db_name = if databases.is_empty() {
            "main".to_string()
        } else {
            format!("db{}", databases.len())
        };
        databases.insert(path.to_string(), translated_db_name.clone());
        translated_db_name
    }
}

fn convert_path_to_database(
    ObjectName(identifiers): &ObjectName,
    databases: &mut DatabaseNamesByPath,
) -> Result<String, String> {
    let path_string = extract_unary_identifier(identifiers, "table")?;

    let path_buf = Utf8UnixPathBuf::from(path_string);
    let path_buf = path_buf.normalize();

    let translated_db_name = resolve_translated_db_name(databases, &path_buf.to_string());

    Ok(translated_db_name)
}

fn get_qualified_values_table_identifiers(db_reference: &str) -> Vec<Ident> {
    vec![Ident::new(db_reference), Ident::new(VALUES_TABLE_NAME)]
}

fn add_to_referencable_tables(
    table_name: &Option<&ObjectName>,
    alias: &Option<TableAlias>,
    databases: &mut DatabaseNamesByPath,
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

            let db_reference = convert_path_to_database(table_name, databases)?;

            if referencable_tables.contains_key(&path) {
                return Err(format!("Duplicate table name: {}", path));
            }

            referencable_tables.insert(path, get_qualified_values_table_identifiers(&db_reference));
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
