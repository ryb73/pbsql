use serde::Serialize;
use sqlparser::{
    ast::{
        self, Assignment, Expr, Function, FunctionArg, FunctionArgExpr, GroupByExpr,
        HiveDistributionStyle, HiveFormat, Ident, Join, JoinConstraint, JoinOperator, ObjectName,
        ObjectType, Offset, OnConflict, OnConflictAction, OnInsert, OrderByExpr, Select,
        SelectItem, SetExpr,
        Statement::{self, CreateIndex, CreateTable, Insert},
        TableAlias, TableFactor, TableWithJoins, Value, Values, WildcardAdditionalOptions,
    },
    dialect::SQLiteDialect,
    parser::Parser,
};
use std::collections::HashMap;
use typed_path::Utf8UnixPathBuf;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

#[wasm_bindgen(typescript_custom_section)]
const TS_RUST_RESULT: &'static str = r#"
export type RustResult<O, E> = { Ok: O } | { Err: E };
"#;

const VALUES_TABLE_NAME: &str = "table_contents";
const _VALUES_TABLE_INDEX_PREFIX: &str = "tbl_";

type DatabaseNamesByPath = HashMap<String, String>;

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

#[derive(Debug, Serialize)]
struct TranslatedQuery<Query> {
    databases: DatabaseNamesByPath,
    query: Query,
}

trait SqlAstTraverser<Error> {
    fn new() -> Self;
    fn traverse(&mut self, ast: Vec<Statement>) -> Result<Vec<Statement>, Error>;
    fn traverse_statement(&mut self, statement: Statement) -> Result<Statement, Error>;
    fn traverse_drop(&mut self, drop: &Statement) -> Result<Statement, Error>;
    fn traverse_update(&mut self, update: &Statement) -> Result<Statement, Error>;
    fn traverse_assignment(&mut self, assignment: &Assignment) -> Result<Assignment, Error>;
    fn traverse_create_table(&mut self, create_table: &Statement) -> Result<Statement, Error>;
    fn traverse_create_index(&mut self, create_index: &Statement) -> Result<Statement, Error>;
    fn traverse_insert(&mut self, insert: &Statement) -> Result<Statement, Error>;
    fn traverse_on_insert(
        &mut self,
        on_insert: &Option<OnInsert>,
    ) -> Result<Option<OnInsert>, Error>;
    fn traverse_on_conflict(&mut self, on_conflict: &OnConflict) -> Result<OnConflict, Error>;
    fn traverse_ast_query(&mut self, query: &Box<ast::Query>) -> Result<Box<ast::Query>, Error>;
    fn traverse_offset(&mut self, offset: &Offset) -> Result<Offset, Error>;
    fn traverse_order_by_expr(&mut self, expr: &OrderByExpr) -> Result<OrderByExpr, Error>;
    fn traverse_set_expr(&mut self, body: &SetExpr) -> Result<SetExpr, Error>;
    fn traverse_set_operation(&mut self, body: &SetExpr) -> Result<SetExpr, Error>;
    fn traverse_table_with_joins(
        &mut self,
        table_with_joins: &TableWithJoins,
    ) -> Result<TableWithJoins, Error>;
    fn traverse_join(&mut self, join: &Join) -> Result<Join, Error>;
    fn traverse_join_operator(
        &mut self,
        join_operator: &JoinOperator,
    ) -> Result<JoinOperator, Error>;
    fn traverse_join_constraint(
        &mut self,
        constraint: &JoinConstraint,
    ) -> Result<JoinConstraint, Error>;
    fn traverse_table_factor(&mut self, relation: &TableFactor) -> Result<TableFactor, Error>;
    fn traverse_table_factor_derived(
        &mut self,
        relation: &TableFactor,
    ) -> Result<TableFactor, Error>;
    fn traverse_select_tables(
        &mut self,
        tables: &Vec<TableWithJoins>,
    ) -> Result<Vec<TableWithJoins>, Error>;
    fn traverse_select(&mut self, select: &Box<Select>) -> Result<Box<Select>, Error>;
    fn traverse_select_item(&mut self, select_item: &SelectItem) -> Result<SelectItem, Error>;
    fn traverse_expr(&mut self, expr: &Expr) -> Result<Expr, Error>;
    fn traverse_in_subquery(&mut self, in_subquery: &Expr) -> Result<Expr, Error>;
    fn traverse_function(&mut self, func: &Function) -> Result<Function, Error>;
    fn traverse_function_arg(&mut self, function_arg: &FunctionArg) -> Result<FunctionArg, Error>;
    fn traverse_function_arg_expr(
        &mut self,
        function_arg_expr: &FunctionArgExpr,
    ) -> Result<FunctionArgExpr, Error>;
    fn traverse_wildcard_additional_options(
        &mut self,
        wildcard_additional_options: &WildcardAdditionalOptions,
    ) -> Result<WildcardAdditionalOptions, Error>;
    fn traverse_value(&mut self, value: &Value) -> Result<Value, Error>;
}

struct PathConvertor {
    database_names: DatabaseNamesByPath,
    scopes: Vec<Scope>,
}

impl SqlAstTraverser<String> for PathConvertor {
    fn new() -> Self {
        PathConvertor {
            database_names: HashMap::new(),
            scopes: vec![],
        }
    }

    fn traverse_statement(&mut self, statement: Statement) -> Result<Statement, String> {
        match statement {
            CreateTable { .. } => self.traverse_create_table(&statement),
            CreateIndex { .. } => self.traverse_create_index(&statement),
            Insert { .. } => self.traverse_insert(&statement),
            Statement::Update { .. } => self.traverse_update(&statement),
            Statement::Query(query) => Ok(Statement::Query(self.traverse_ast_query(&query)?)),
            Statement::Drop { .. } => self.traverse_drop(&statement),

            Statement::Analyze { .. } => Err("not implemented: Statement::Analyze".to_string()),
            Statement::Truncate { .. } => Err("not implemented: Statement::Truncate".to_string()),
            Statement::Msck { .. } => Err("not implemented: Statement::Msck".to_string()),
            Statement::AlterTable { .. } => {
                Err("not implemented: Statement::AlterTable".to_string())
            }
            Statement::AlterIndex { .. } => {
                Err("not implemented: Statement::AlterIndex".to_string())
            }
            Statement::AlterView { .. } => Err("not implemented: Statement::AlterView".to_string()),
            Statement::AlterRole { .. } => Err("not implemented: Statement::AlterRole".to_string()),
            Statement::AttachDatabase { .. } => {
                Err("not implemented: Statement::AttachDatabase".to_string())
            }
            Statement::DropFunction { .. } => {
                Err("not implemented: Statement::DropFunction".to_string())
            }
            Statement::Declare { .. } => Err("not implemented: Statement::Declare".to_string()),
            Statement::CreateExtension { .. } => {
                Err("not implemented: Statement::CreateExtension".to_string())
            }
            Statement::Fetch { .. } => Err("not implemented: Statement::Fetch".to_string()),
            Statement::Flush { .. } => Err("not implemented: Statement::Flush".to_string()),
            Statement::Discard { .. } => Err("not implemented: Statement::Discard".to_string()),
            Statement::SetRole { .. } => Err("not implemented: Statement::SetRole".to_string()),
            Statement::SetVariable { .. } => {
                Err("not implemented: Statement::SetVariable".to_string())
            }
            Statement::SetTimeZone { .. } => {
                Err("not implemented: Statement::SetTimeZone".to_string())
            }
            Statement::SetNames { .. } => Err("not implemented: Statement::SetNames".to_string()),
            Statement::SetNamesDefault { .. } => {
                Err("not implemented: Statement::SetNamesDefault".to_string())
            }
            Statement::ShowFunctions { .. } => {
                Err("not implemented: Statement::ShowFunctions".to_string())
            }
            Statement::ShowVariable { .. } => {
                Err("not implemented: Statement::ShowVariable".to_string())
            }
            Statement::ShowVariables { .. } => {
                Err("not implemented: Statement::ShowVariables".to_string())
            }
            Statement::ShowCreate { .. } => {
                Err("not implemented: Statement::ShowCreate".to_string())
            }
            Statement::ShowColumns { .. } => {
                Err("not implemented: Statement::ShowColumns".to_string())
            }
            Statement::ShowTables { .. } => {
                Err("not implemented: Statement::ShowTables".to_string())
            }
            Statement::ShowCollation { .. } => {
                Err("not implemented: Statement::ShowCollation".to_string())
            }
            Statement::Use { .. } => Err("not implemented: Statement::Use".to_string()),
            Statement::StartTransaction { .. } => {
                Err("not implemented: Statement::StartTransaction".to_string())
            }
            Statement::SetTransaction { .. } => {
                Err("not implemented: Statement::SetTransaction".to_string())
            }
            Statement::Comment { .. } => Err("not implemented: Statement::Comment".to_string()),
            Statement::Commit { .. } => Err("not implemented: Statement::Commit".to_string()),
            Statement::Rollback { .. } => Err("not implemented: Statement::Rollback".to_string()),
            Statement::CreateSchema { .. } => {
                Err("not implemented: Statement::CreateSchema".to_string())
            }
            Statement::CreateDatabase { .. } => {
                Err("not implemented: Statement::CreateDatabase".to_string())
            }
            Statement::CreateFunction { .. } => {
                Err("not implemented: Statement::CreateFunction".to_string())
            }
            Statement::CreateProcedure { .. } => {
                Err("not implemented: Statement::CreateProcedure".to_string())
            }
            Statement::CreateMacro { .. } => {
                Err("not implemented: Statement::CreateMacro".to_string())
            }
            Statement::CreateStage { .. } => {
                Err("not implemented: Statement::CreateStage".to_string())
            }
            Statement::Assert { .. } => Err("not implemented: Statement::Assert".to_string()),
            Statement::Grant { .. } => Err("not implemented: Statement::Grant".to_string()),
            Statement::Revoke { .. } => Err("not implemented: Statement::Revoke".to_string()),
            Statement::Deallocate { .. } => {
                Err("not implemented: Statement::Deallocate".to_string())
            }
            Statement::Execute { .. } => Err("not implemented: Statement::Execute".to_string()),
            Statement::Prepare { .. } => Err("not implemented: Statement::Prepare".to_string()),
            Statement::Kill { .. } => Err("not implemented: Statement::Kill".to_string()),
            Statement::ExplainTable { .. } => {
                Err("not implemented: Statement::ExplainTable".to_string())
            }
            Statement::Explain { .. } => Err("not implemented: Statement::Explain".to_string()),
            Statement::Savepoint { .. } => Err("not implemented: Statement::Savepoint".to_string()),
            Statement::ReleaseSavepoint { .. } => {
                Err("not implemented: Statement::ReleaseSavepoint".to_string())
            }
            Statement::Merge { .. } => Err("not implemented: Statement::Merge".to_string()),
            Statement::Cache { .. } => Err("not implemented: Statement::Cache".to_string()),
            Statement::UNCache { .. } => Err("not implemented: Statement::UNCache".to_string()),
            Statement::CreateSequence { .. } => {
                Err("not implemented: Statement::CreateSequence".to_string())
            }
            Statement::CreateType { .. } => {
                Err("not implemented: Statement::CreateType".to_string())
            }
            Statement::Pragma { .. } => Err("not implemented: Statement::Pragma".to_string()),
            Statement::LockTables { .. } => {
                Err("not implemented: Statement::LockTables".to_string())
            }
            Statement::UnlockTables => Err("not implemented: Statement::UnlockTables".to_string()),
            Statement::Directory { .. } => Err("not implemented: Statement::Directory".to_string()),
            Statement::Call(_) => Err("not implemented: Statement::Call".to_string()),
            Statement::Copy { .. } => Err("not implemented: Statement::Copy".to_string()),
            Statement::CopyIntoSnowflake { .. } => {
                Err("not implemented: Statement::CopyIntoSnowflake".to_string())
            }
            Statement::Close { .. } => Err("not implemented: Statement::Close".to_string()),
            Statement::Delete { .. } => Err("not implemented: Statement::Delete".to_string()),
            Statement::CreateView { .. } => {
                Err("not implemented: Statement::CreateView".to_string())
            }
            Statement::CreateVirtualTable { .. } => {
                Err("not implemented: Statement::CreateVirtualTable".to_string())
            }
            Statement::CreateRole { .. } => {
                Err("not implemented: Statement::CreateRole".to_string())
            }
        }
    }

    fn traverse(&mut self, ast: Vec<Statement>) -> Result<Vec<Statement>, String> {
        ast.into_iter()
            .map(|s| self.traverse_statement(s))
            .collect()
    }

    fn traverse_drop(&mut self, drop: &Statement) -> Result<Statement, String> {
        match drop {
            Statement::Drop {
                cascade,
                if_exists,
                names,
                object_type,
                purge,
                restrict,
                temporary,
            } => {
                match object_type {
                    ObjectType::Table => {}
                    ObjectType::View => Err("not implemented: Statement::Drop::View".to_string())?,
                    ObjectType::Index => {
                        Err("not implemented: Statement::Drop::Index".to_string())?
                    }
                    ObjectType::Schema => {
                        Err("not implemented: Statement::Drop::Schema".to_string())?
                    }
                    ObjectType::Role => Err("not implemented: Statement::Drop::Role".to_string())?,
                    ObjectType::Sequence => {
                        Err("not implemented: Statement::Drop::Sequence".to_string())?
                    }
                    ObjectType::Stage => {
                        Err("not implemented: Statement::Drop::Stage".to_string())?
                    }
                };

                if *cascade {
                    Err("not implemented: Statement::Drop::cascade".to_string())?;
                };

                if *purge {
                    Err("not implemented: Statement::Drop::purge".to_string())?;
                };

                if *restrict {
                    Err("not implemented: Statement::Drop::restrict".to_string())?;
                };

                if *temporary {
                    Err("not implemented: Statement::Drop::temporary".to_string())?;
                };

                let new_names = names
                    .iter()
                    .map(|name| {
                        let db_reference =
                            convert_path_to_database(name, &mut self.database_names)?;
                        Ok(ObjectName(get_qualified_values_table_identifiers(
                            &db_reference,
                        )))
                    })
                    .collect::<Result<Vec<_>, String>>()?;

                Ok(Statement::Drop {
                    cascade: false,
                    if_exists: *if_exists,
                    names: new_names,
                    object_type: ObjectType::Table,
                    purge: false,
                    restrict: false,
                    temporary: false,
                })
            }
            _ => {
                unreachable!("Expected a Drop statement")
            }
        }
    }

    fn traverse_update(&mut self, update: &Statement) -> Result<Statement, String> {
        match update {
            Statement::Update {
                assignments,
                from,
                returning,
                selection,
                table,
            } => {
                if returning.is_some() {
                    return Err("not implemented: Statement::Update::returning".to_string());
                }

                self.scopes.push(Scope::new());

                let new_table = self.traverse_table_with_joins(table)?;

                let new_from = from
                    .as_ref()
                    .map(|f| self.traverse_table_with_joins(f))
                    .transpose()?;

                let new_assignments = assignments
                    .iter()
                    .map(|assignment| self.traverse_assignment(assignment))
                    .collect::<Result<Vec<_>, _>>()?;

                let new_selection = selection
                    .as_ref()
                    .map(|s| self.traverse_expr(s))
                    .transpose()?;

                self.scopes.pop();

                Ok(Statement::Update {
                    assignments: new_assignments,
                    from: new_from,
                    selection: new_selection,
                    table: new_table,

                    returning: None,
                })
            }
            _ => {
                unreachable!("Expected an Update statement");
            }
        }
    }

    fn traverse_assignment(
        &mut self,
        Assignment { id, value }: &Assignment,
    ) -> Result<Assignment, String> {
        let new_value = self.traverse_expr(value)?;

        extract_unary_identifier(id, "column")?;

        Ok(Assignment {
            id: id.clone(),
            value: new_value,
        })
    }

    fn traverse_create_table(&mut self, create_table: &Statement) -> Result<Statement, String> {
        if let CreateTable {
            if_not_exists,
            name,
            columns,
            auto_increment_offset,
            clone,
            cluster_by,
            collation,
            comment,
            constraints,
            default_charset,
            engine,
            external,
            file_format,
            global,
            hive_distribution,
            hive_formats,
            like,
            location,
            on_cluster,
            on_commit,
            options,
            or_replace,
            order_by,
            partition_by,
            query,
            strict,
            table_properties,
            temporary,
            transient,
            with_options,
            without_rowid,
        } = create_table
        {
            if clone.is_some() {
                return Err("not implemented: CreateTable::clone".to_string());
            }

            if cluster_by.is_some() {
                return Err("not implemented: CreateTable::cluster_by".to_string());
            }

            if comment.is_some() {
                return Err("not implemented: CreateTable::comment".to_string());
            }

            if !constraints.is_empty() {
                return Err("not implemented: CreateTable::constraints".to_string());
            }

            if engine.is_some() {
                return Err("not implemented: CreateTable::engine".to_string());
            }

            if *external {
                return Err("not implemented: CreateTable::external".to_string());
            }

            if file_format.is_some() {
                return Err("not implemented: CreateTable::file_format".to_string());
            }

            if global.is_some() {
                return Err("not implemented: CreateTable::global".to_string());
            }

            if hive_distribution != &HiveDistributionStyle::NONE {
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

            if on_commit.is_some() {
                return Err("not implemented: CreateTable::on_commit".to_string());
            }

            if options.is_some() {
                return Err("not implemented: CreateTable::options".to_string());
            }

            if order_by.is_some() {
                return Err("not implemented: CreateTable::order_by".to_string());
            }

            if partition_by.is_some() {
                return Err("not implemented: CreateTable::partition_by".to_string());
            }

            if query.is_some() {
                return Err("not implemented: CreateTable::query".to_string());
            }

            if !table_properties.is_empty() {
                return Err("not implemented: CreateTable::table_properties".to_string());
            }

            if *temporary {
                return Err("not implemented: CreateTable::temporary".to_string());
            }

            if *transient {
                return Err("not implemented CreateTable::transient".to_string());
            }

            if !with_options.is_empty() {
                return Err("not implemented: CreateTable::with_options".to_string());
            }

            if *without_rowid {
                return Err("not implemented: CreateTable::without_rowid".to_string());
            }

            let db_reference = convert_path_to_database(name, &mut self.database_names)?;

            Ok(CreateTable {
                auto_increment_offset: *auto_increment_offset,
                collation: collation.clone(),
                columns: columns.clone(),
                default_charset: default_charset.clone(),
                if_not_exists: *if_not_exists,
                name: ObjectName(get_qualified_values_table_identifiers(&db_reference)),
                or_replace: *or_replace,
                strict: *strict,

                clone: None,
                cluster_by: None,
                comment: None,
                constraints: vec![],
                engine: None,
                external: false,
                file_format: None,
                global: None,
                hive_distribution: HiveDistributionStyle::NONE,
                hive_formats: None,
                like: None,
                location: None,
                on_cluster: None,
                on_commit: None,
                options: None,
                order_by: None,
                partition_by: None,
                query: None,
                table_properties: vec![],
                temporary: false,
                transient: false,
                with_options: vec![],
                without_rowid: false,
            })
        } else {
            unreachable!("Expected a CreateTable statement");
        }
    }

    fn traverse_create_index(&mut self, create_index: &Statement) -> Result<Statement, String> {
        match create_index {
            CreateIndex { name: None, .. } => Err("Index name is required")?,
            CreateIndex {
                columns,
                name: Some(ObjectName(name_identifiers)),
                table_name,
                unique,
                if_not_exists,
                nulls_distinct,
                // TODO: handle other fields, if only by throwing an error for unhandled syntax.
                ..
            } => {
                let index_name = extract_unary_identifier(name_identifiers, "index")?;

                let translated_db_name =
                    convert_path_to_database(table_name, &mut self.database_names)?;

                let new_columns = columns
                    .iter()
                    .map(|c| self.traverse_order_by_expr(c))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(CreateIndex {
                    columns: new_columns,
                    if_not_exists: *if_not_exists,
                    name: Some(ObjectName(vec![
                        Ident::new(translated_db_name),
                        Ident::new(format!("{}{}", _VALUES_TABLE_INDEX_PREFIX, index_name)),
                    ])),
                    nulls_distinct: *nulls_distinct,
                    table_name: ObjectName(vec![Ident::new(VALUES_TABLE_NAME)]),
                    unique: *unique,

                    // Not handled
                    concurrently: false,
                    include: vec![],
                    predicate: None,
                    using: None,
                })
            }
            _ => {
                unreachable!("Expected a CreateIndex statement");
            }
        }
    }

    fn traverse_insert(&mut self, insert: &Statement) -> Result<Statement, String> {
        match insert {
            Insert {
                columns,
                table_name,
                table_alias,
                ignore,
                into,
                or,
                overwrite,
                replace_into,
                table,
                source,
                after_columns,
                on,
                partitioned,
                priority,
                returning,
            } => {
                if !after_columns.is_empty() {
                    return Err("not implemented: Insert::after_columns".to_string());
                }

                if partitioned.is_some() {
                    return Err("not implemented: Insert::partitioned".to_string());
                }

                if priority.is_some() {
                    return Err("not implemented: Insert::priority".to_string());
                }

                if returning.is_some() {
                    return Err("not implemented: Insert::returning".to_string());
                }

                let translated_db_name =
                    convert_path_to_database(table_name, &mut self.database_names)?;

                let new_source = source
                    .as_ref()
                    .map(|s| self.traverse_ast_query(s))
                    .transpose()?;

                let new_on = self.traverse_on_insert(on)?;

                Ok(Insert {
                    after_columns: vec![],
                    columns: columns.clone(),
                    ignore: *ignore,
                    into: *into,
                    on: new_on,
                    or: *or,
                    overwrite: *overwrite,
                    replace_into: *replace_into,
                    source: new_source,
                    table_alias: table_alias.clone(),
                    table_name: ObjectName(get_qualified_values_table_identifiers(
                        &translated_db_name,
                    )),
                    table: *table,

                    partitioned: None,
                    priority: None,
                    returning: None,
                })
            }
            _ => {
                unreachable!("Expected an Insert statement");
            }
        }
    }

    fn traverse_on_insert(
        &mut self,
        on_insert: &Option<OnInsert>,
    ) -> Result<Option<OnInsert>, String> {
        match on_insert {
            Some(OnInsert::OnConflict(on_conflict)) => Ok(Some(OnInsert::OnConflict(
                self.traverse_on_conflict(on_conflict)?,
            ))),
            None => Ok(None),

            Some(OnInsert::DuplicateKeyUpdate(..)) => {
                Err("not implemented: OnInsert::DuplicateKeyUpdate".to_string())
            }

            &Some(_) => Err("Unrecognized OnInsert variant".to_string()),
        }
    }

    fn traverse_on_conflict(
        &mut self,
        OnConflict {
            action,
            conflict_target,
        }: &OnConflict,
    ) -> Result<OnConflict, String> {
        if conflict_target.is_some() {
            return Err("not implemented: OnConflict::conflict_target".to_string());
        }

        match action {
            OnConflictAction::DoNothing => Ok(OnConflict {
                action: OnConflictAction::DoNothing,
                conflict_target: None,
            }),

            OnConflictAction::DoUpdate(_) => {
                Err("not implemented: OnConflictAction::DoUpdate".to_string())
            }
        }
    }

    fn traverse_ast_query(&mut self, query: &Box<ast::Query>) -> Result<Box<ast::Query>, String> {
        let ast::Query {
            body,
            fetch,
            for_clause,
            limit,
            limit_by,
            locks,
            offset,
            order_by,
            with,
        } = query.as_ref();

        if fetch.is_some() {
            return Err("not implemented: ast::Query::fetch".to_string());
        }

        if for_clause.is_some() {
            return Err("not implemented: ast::Query::for_clause".to_string());
        }

        if !limit_by.is_empty() {
            return Err("not implemented: ast::Query::limit_by".to_string());
        }

        if !locks.is_empty() {
            return Err("not implemented: ast::Query::locks".to_string());
        }

        if with.is_some() {
            return Err("not implemented: ast::Query::with".to_string());
        }

        self.scopes.push(Scope::new());

        let new_body = self.traverse_set_expr(body)?;

        let new_order_by = order_by
            .iter()
            .map(|expr| self.traverse_order_by_expr(expr))
            .collect::<Result<Vec<_>, _>>()?;

        let new_offset = offset
            .as_ref()
            .map(|o| self.traverse_offset(o))
            .transpose()?;

        let new_limit = limit.as_ref().map(|l| self.traverse_expr(l)).transpose()?;

        self.scopes.pop();

        Ok(Box::new(ast::Query {
            body: Box::new(new_body),
            limit: new_limit,
            order_by: new_order_by,
            offset: new_offset,

            fetch: None,
            for_clause: None,
            limit_by: vec![],
            locks: vec![],
            with: None,
        }))
    }

    fn traverse_offset(&mut self, Offset { value, rows }: &Offset) -> Result<Offset, String> {
        Ok(Offset {
            rows: *rows,
            value: self.traverse_expr(value)?,
        })
    }

    fn traverse_order_by_expr(&mut self, expr: &OrderByExpr) -> Result<OrderByExpr, String> {
        match expr {
            OrderByExpr {
                expr,
                asc,
                nulls_first,
            } => {
                let new_expr = self.traverse_expr(expr)?;

                Ok(OrderByExpr {
                    expr: new_expr,
                    asc: *asc,
                    nulls_first: *nulls_first,
                })
            }
        }
    }

    fn traverse_set_expr(&mut self, body: &SetExpr) -> Result<SetExpr, String> {
        match body {
            SetExpr::Values(Values { explicit_row, rows }) => Ok(SetExpr::Values(Values {
                explicit_row: *explicit_row,
                rows: rows
                    .into_iter()
                    .map(|row| {
                        row.into_iter()
                            .map(|expr| self.traverse_expr(expr))
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|e| e.to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            })),
            SetExpr::Select(select) => {
                let new_select = self.traverse_select(select)?;
                Ok(SetExpr::Select(new_select))
            }
            SetExpr::SetOperation { .. } => self.traverse_set_operation(body),

            SetExpr::Query(_) => Err("not implemented: SetExpr::Query".to_string()),
            SetExpr::Insert(_) => Err("not implemented: SetExpr::Insert".to_string()),
            SetExpr::Update(_) => Err("not implemented: SetExpr::Update".to_string()),
            SetExpr::Table(_) => Err("not implemented: SetExpr::Table".to_string()),
        }
    }

    fn traverse_set_operation(&mut self, body: &SetExpr) -> Result<SetExpr, String> {
        match body {
            SetExpr::SetOperation {
                left,
                op,
                right,
                set_quantifier,
            } => {
                self.scopes.push(Scope::new());
                let new_left = self.traverse_set_expr(left)?;
                self.scopes.pop();

                self.scopes.push(Scope::new());
                let new_right = self.traverse_set_expr(right)?;
                self.scopes.pop();

                Ok(SetExpr::SetOperation {
                    left: Box::new(new_left),
                    op: *op,
                    right: Box::new(new_right),
                    set_quantifier: *set_quantifier,
                })
            }
            _ => {
                unreachable!("Expected a SetOperation SetExpr");
            }
        }
    }

    fn traverse_table_with_joins(
        &mut self,
        TableWithJoins { relation, joins }: &TableWithJoins,
    ) -> Result<TableWithJoins, String> {
        let new_relation = self.traverse_table_factor(relation)?;

        let new_join = joins
            .iter()
            .map(|join| self.traverse_join(join))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(TableWithJoins {
            relation: new_relation,
            joins: new_join,
        })
    }

    fn traverse_join(&mut self, join: &Join) -> Result<Join, String> {
        match join {
            Join {
                join_operator,
                relation,
            } => {
                let new_relation = self.traverse_table_factor(relation)?;

                let new_join_operator = self.traverse_join_operator(join_operator)?;

                Ok(Join {
                    join_operator: new_join_operator,
                    relation: new_relation,
                })
            }
        }
    }

    fn traverse_join_operator(
        &mut self,
        join_operator: &JoinOperator,
    ) -> Result<JoinOperator, String> {
        match join_operator {
            JoinOperator::Inner(constraint) => {
                let new_constraint = self.traverse_join_constraint(constraint)?;
                Ok(JoinOperator::Inner(new_constraint))
            }
            JoinOperator::LeftOuter(constraint) => {
                let new_constraint = self.traverse_join_constraint(constraint)?;
                Ok(JoinOperator::LeftOuter(new_constraint))
            }
            JoinOperator::RightOuter(constraint) => {
                let new_constraint = self.traverse_join_constraint(constraint)?;
                Ok(JoinOperator::RightOuter(new_constraint))
            }
            JoinOperator::FullOuter(constraint) => {
                let new_constraint = self.traverse_join_constraint(constraint)?;
                Ok(JoinOperator::FullOuter(new_constraint))
            }
            JoinOperator::CrossJoin => Ok(JoinOperator::CrossJoin),
            JoinOperator::LeftSemi(constraint) => {
                let new_constraint = self.traverse_join_constraint(constraint)?;
                Ok(JoinOperator::LeftSemi(new_constraint))
            }
            JoinOperator::RightSemi(constraint) => {
                let new_constraint = self.traverse_join_constraint(constraint)?;
                Ok(JoinOperator::RightSemi(new_constraint))
            }
            JoinOperator::LeftAnti(constraint) => {
                let new_constraint = self.traverse_join_constraint(constraint)?;
                Ok(JoinOperator::LeftAnti(new_constraint))
            }
            JoinOperator::RightAnti(constraint) => {
                let new_constraint = self.traverse_join_constraint(constraint)?;
                Ok(JoinOperator::RightAnti(new_constraint))
            }
            JoinOperator::CrossApply => Ok(JoinOperator::CrossApply),
            JoinOperator::OuterApply => Ok(JoinOperator::OuterApply),
        }
    }

    fn traverse_join_constraint(
        &mut self,
        constraint: &JoinConstraint,
    ) -> Result<JoinConstraint, String> {
        match constraint {
            JoinConstraint::On(expr) => {
                let new_expr = self.traverse_expr(expr)?;
                Ok(JoinConstraint::On(new_expr))
            }
            JoinConstraint::Natural => Ok(JoinConstraint::Natural),
            JoinConstraint::None => Ok(JoinConstraint::None),

            JoinConstraint::Using(_) => Err("not implemented: JoinConstraint::Using".to_string()),
        }
    }

    fn traverse_table_factor(&mut self, relation: &TableFactor) -> Result<TableFactor, String> {
        match relation {
            TableFactor::Table {
                alias,
                args,
                name: table_name,
                partitions,
                version,
                with_hints,
            } => {
                if args.is_some() {
                    return Err("not implemented: TableFactor::Table::args".to_string());
                }

                if !partitions.is_empty() {
                    return Err("not implemented: TableFactor::Table::partitions".to_string());
                }

                if version.is_some() {
                    return Err("not implemented: TableFactor::Table::version".to_string());
                }

                if !with_hints.is_empty() {
                    return Err("not implemented: TableFactor::Table::with_hints".to_string());
                }

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

                Ok(TableFactor::Table {
                    alias: alias.clone(),
                    name: ObjectName(get_qualified_values_table_identifiers(&db_reference)),

                    args: None,
                    partitions: vec![],
                    version: None,
                    with_hints: vec![],
                })
            }
            TableFactor::Derived { .. } => self.traverse_table_factor_derived(relation),

            TableFactor::TableFunction { .. } => {
                Err("not implemented: TableFactor::TableFunction".to_string())
            }
            TableFactor::Function { .. } => {
                Err("not implemented: TableFactor::Function".to_string())
            }
            TableFactor::UNNEST { .. } => Err("not implemented: TableFactor::UNNEST".to_string()),
            TableFactor::JsonTable { .. } => {
                Err("not implemented: TableFactor::JsonTable".to_string())
            }
            TableFactor::NestedJoin { .. } => {
                Err("not implemented: TableFactor::NestedJoin".to_string())
            }
            TableFactor::Pivot { .. } => Err("not implemented: TableFactor::Pivot".to_string()),
            TableFactor::Unpivot { .. } => Err("not implemented: TableFactor::Unpivot".to_string()),
        }
    }

    fn traverse_table_factor_derived(
        &mut self,
        relation: &TableFactor,
    ) -> Result<TableFactor, String> {
        match relation {
            TableFactor::Derived {
                lateral,
                subquery,
                alias,
            } => {
                let new_subquery = self.traverse_ast_query(subquery)?;

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

                Ok(TableFactor::Derived {
                    lateral: *lateral,
                    subquery: new_subquery,
                    alias: alias.clone(),
                })
            }
            _ => {
                unreachable!("Expected a Derived TableFactor");
            }
        }
    }

    fn traverse_select_tables(
        &mut self,
        tables: &Vec<TableWithJoins>,
    ) -> Result<Vec<TableWithJoins>, String> {
        let new_tables = tables
            .iter()
            .map(|t| self.traverse_table_with_joins(t))
            .collect::<Result<Vec<TableWithJoins>, String>>()?;

        Ok(new_tables)
    }

    fn traverse_select(&mut self, select: &Box<Select>) -> Result<Box<Select>, String> {
        let Select {
            cluster_by,
            distinct,
            distribute_by,
            from,
            group_by,
            having,
            into,
            lateral_views,
            named_window,
            projection,
            qualify,
            selection,
            sort_by,
            top,
        } = select.as_ref();

        if !cluster_by.is_empty() {
            return Err("not implemented: Select::cluster_by".to_string());
        }

        if distinct.is_some() {
            return Err("not implemented: Select::distinct".to_string());
        }

        if !distribute_by.is_empty() {
            return Err("not implemented: Select::distribute_by".to_string());
        }

        if having.is_some() {
            return Err("not implemented: Select::having".to_string());
        }

        if into.is_some() {
            return Err("not implemented: Select::into".to_string());
        }

        if !lateral_views.is_empty() {
            return Err("not implemented: Select::lateral_views".to_string());
        }

        if !named_window.is_empty() {
            return Err("not implemented: Select::named_window".to_string());
        }

        if qualify.is_some() {
            return Err("not implemented: Select::qualify".to_string());
        }

        if !sort_by.is_empty() {
            // TODO: Just a thought. If this is ever released to the public, maybe make these error messages friendlier and encourage/facilitate contribution
            return Err("not implemented: Select::sort_by".to_string());
        }

        if top.is_some() {
            return Err("not implemented: Select::top".to_string());
        }

        let new_from = self.traverse_select_tables(from)?;

        let new_projection = projection
            .iter()
            .map(|item| self.traverse_select_item(item))
            .collect::<Result<Vec<_>, _>>()?;

        let new_selection = selection
            .as_ref()
            .map(|expr| self.traverse_expr(expr))
            .transpose()?;

        let new_group_by = match group_by {
            GroupByExpr::Expressions(expressions) => {
                let new_expressions = expressions
                    .iter()
                    .map(|expr| self.traverse_expr(expr))
                    .collect::<Result<Vec<_>, _>>()?;

                GroupByExpr::Expressions(new_expressions)
            }
            GroupByExpr::All => return Err("not implemented: GroupByExpr::All".to_string()),
        };

        Ok(Box::new(Select {
            from: new_from,
            group_by: new_group_by,
            projection: new_projection,
            selection: new_selection,

            cluster_by: vec![],
            distinct: None,
            distribute_by: vec![],
            having: None,
            into: None,
            lateral_views: vec![],
            named_window: vec![],
            qualify: None,
            sort_by: vec![],
            top: None,
        }))
    }

    fn traverse_select_item(&mut self, select_item: &SelectItem) -> Result<SelectItem, String> {
        match select_item {
            SelectItem::UnnamedExpr(expr) => Ok(SelectItem::UnnamedExpr(self.traverse_expr(expr)?)),
            SelectItem::ExprWithAlias { expr, alias } => {
                let new_expr = self.traverse_expr(expr)?;

                Ok(SelectItem::ExprWithAlias {
                    expr: new_expr,
                    alias: alias.clone(),
                })
            }
            SelectItem::Wildcard(wildcard_options) => {
                let new_wildcard_options =
                    self.traverse_wildcard_additional_options(wildcard_options)?;
                Ok(SelectItem::Wildcard(new_wildcard_options))
            }

            SelectItem::QualifiedWildcard(_, _) => {
                Err("not implemented: SelectItem::QualifiedWildcard".to_string())
            }
        }
    }

    fn traverse_wildcard_additional_options(
        &mut self,
        WildcardAdditionalOptions {
            opt_except,
            opt_exclude,
            opt_rename,
            opt_replace,
        }: &WildcardAdditionalOptions,
    ) -> Result<WildcardAdditionalOptions, String> {
        if opt_except.is_some() {
            return Err("not implemented: WildcardAdditionalOptions::opt_except".to_string());
        }

        if opt_exclude.is_some() {
            return Err("not implemented: WildcardAdditionalOptions::opt_exclude".to_string());
        }

        if opt_rename.is_some() {
            return Err("not implemented: WildcardAdditionalOptions::opt_rename".to_string());
        }

        if opt_replace.is_some() {
            return Err("not implemented: WildcardAdditionalOptions::opt_replace".to_string());
        }

        Ok(WildcardAdditionalOptions {
            opt_except: None,
            opt_exclude: None,
            opt_rename: None,
            opt_replace: None,
        })
    }

    fn traverse_expr(&mut self, expr: &Expr) -> Result<Expr, String> {
        match expr {
            Expr::Value(value) => Ok(Expr::Value(self.traverse_value(value)?)),
            Expr::Function(func) => Ok(Expr::Function(self.traverse_function(func)?)),
            Expr::CompoundIdentifier(identifiers) => {
                let (table_reference, column_name) =
                    extract_binary_identifiers(identifiers, "expression")?;

                let table_replacement_identifiers =
                    get_replacement_identifiers_for_table(&self.scopes, &table_reference)?;

                let mut new_identifiers = table_replacement_identifiers.clone();
                new_identifiers.push(Ident::new(&column_name));

                Ok(Expr::CompoundIdentifier(new_identifiers))
            }
            Expr::BinaryOp { left, op, right } => {
                let new_left = self.traverse_expr(left)?;
                let new_right = self.traverse_expr(right)?;
                Ok(Expr::BinaryOp {
                    left: Box::new(new_left),
                    op: op.clone(),
                    right: Box::new(new_right),
                })
            }
            Expr::Identifier(_) => Ok(expr.clone()),
            Expr::InSubquery { .. } => self.traverse_in_subquery(expr),
            Expr::IsNull(expr) => Ok(Expr::IsNull(Box::new(self.traverse_expr(expr)?))),
            Expr::Nested(nested_expr) => {
                Ok(Expr::Nested(Box::new(self.traverse_expr(nested_expr)?)))
            }

            Expr::JsonAccess { .. } => Err("Unhandled syntax: Expr::JsonAccess".to_string()),
            Expr::CompositeAccess { .. } => {
                Err("Unhandled syntax: Expr::CompositeAccess".to_string())
            }
            Expr::IsFalse(_) => Err("Unhandled syntax: Expr::IsFalse".to_string()),
            Expr::IsNotFalse(_) => Err("Unhandled syntax: Expr::IsNotFalse".to_string()),
            Expr::InList { .. } => Err("Unhandled syntax: Expr::InList".to_string()),
            Expr::InUnnest { .. } => Err("Unhandled syntax: Expr::InUnnest".to_string()),
            Expr::Between { .. } => Err("Unhandled syntax: Expr::Between".to_string()),
            Expr::Like { .. } => Err("Unhandled syntax: Expr::Like".to_string()),
            Expr::ILike { .. } => Err("Unhandled syntax: Expr::ILike".to_string()),
            Expr::SimilarTo { .. } => Err("Unhandled syntax: Expr::SimilarTo".to_string()),
            Expr::RLike { .. } => Err("Unhandled syntax: Expr::RLike".to_string()),
            Expr::AnyOp { .. } => Err("Unhandled syntax: Expr::AnyOp".to_string()),
            Expr::AllOp { .. } => Err("Unhandled syntax: Expr::AllOp".to_string()),
            Expr::UnaryOp { .. } => Err("Unhandled syntax: Expr::UnaryOp".to_string()),
            Expr::Convert { .. } => Err("Unhandled syntax: Expr::Convert".to_string()),
            Expr::Cast { .. } => Err("Unhandled syntax: Expr::Cast".to_string()),
            Expr::TryCast { .. } => Err("Unhandled syntax: Expr::TryCast".to_string()),
            Expr::SafeCast { .. } => Err("Unhandled syntax: Expr::SafeCast".to_string()),
            Expr::AtTimeZone { .. } => Err("Unhandled syntax: Expr::AtTimeZone".to_string()),
            Expr::Extract { .. } => Err("Unhandled syntax: Expr::Extract".to_string()),
            Expr::Ceil { .. } => Err("Unhandled syntax: Expr::Ceil".to_string()),
            Expr::Floor { .. } => Err("Unhandled syntax: Expr::Floor".to_string()),
            Expr::Position { .. } => Err("Unhandled syntax: Expr::Position".to_string()),
            Expr::Substring { .. } => Err("Unhandled syntax: Expr::Substring".to_string()),
            Expr::IsTrue(_) => Err("Unhandled syntax: Expr::IsTrue".to_string()),
            Expr::IsNotTrue(_) => Err("Unhandled syntax: Expr::IsNotTrue".to_string()),
            Expr::IsNotNull(_) => Err("Unhandled syntax: Expr::IsNotNull".to_string()),
            Expr::IsUnknown(_) => Err("Unhandled syntax: Expr::IsUnknown".to_string()),
            Expr::IsNotUnknown(_) => Err("Unhandled syntax: Expr::IsNotUnknown".to_string()),
            Expr::IsDistinctFrom(_, _) => Err("Unhandled syntax: Expr::IsDistinctFrom".to_string()),
            Expr::IsNotDistinctFrom(_, _) => {
                Err("Unhandled syntax: Expr::IsNotDistinctFrom".to_string())
            }
            Expr::Trim { .. } => Err("Unhandled syntax: Expr::Trim".to_string()),
            Expr::Overlay { .. } => Err("Unhandled syntax: Expr::Overlay".to_string()),
            Expr::Collate { .. } => Err("Unhandled syntax: Expr::Collate".to_string()),
            Expr::IntroducedString { .. } => {
                Err("Unhandled syntax: Expr::IntroducedString".to_string())
            }
            Expr::TypedString { .. } => Err("Unhandled syntax: Expr::TypedString".to_string()),
            Expr::MapAccess { .. } => Err("Unhandled syntax: Expr::MapAccess".to_string()),
            Expr::AggregateExpressionWithFilter { .. } => {
                Err("Unhandled syntax: Expr::AggregateExpressionWithFilter".to_string())
            }
            Expr::Case { .. } => Err("Unhandled syntax: Expr::Case".to_string()),
            Expr::Exists { .. } => Err("Unhandled syntax: Expr::Exists".to_string()),
            Expr::Subquery(_) => Err("Unhandled syntax: Expr::Subquery".to_string()),
            Expr::ArraySubquery(_) => Err("Unhandled syntax: Expr::ArraySubquery".to_string()),
            Expr::ListAgg(_) => Err("Unhandled syntax: Expr::ListAgg".to_string()),
            Expr::ArrayAgg(_) => Err("Unhandled syntax: Expr::ArrayAgg".to_string()),
            Expr::GroupingSets(_) => Err("Unhandled syntax: Expr::GroupingSets".to_string()),
            Expr::Cube(_) => Err("Unhandled syntax: Expr::Cube".to_string()),
            Expr::Rollup(_) => Err("Unhandled syntax: Expr::Rollup".to_string()),
            Expr::Tuple(_) => Err("Unhandled syntax: Expr::Tuple".to_string()),
            Expr::Struct { .. } => Err("Unhandled syntax: Expr::Struct".to_string()),
            Expr::Named { .. } => Err("Unhandled syntax: Expr::Named".to_string()),
            Expr::ArrayIndex { .. } => Err("Unhandled syntax: Expr::ArrayIndex".to_string()),
            Expr::Array(_) => Err("Unhandled syntax: Expr::Array".to_string()),
            Expr::Interval(_) => Err("Unhandled syntax: Expr::Interval".to_string()),
            Expr::MatchAgainst { .. } => Err("Unhandled syntax: Expr::MatchAgainst".to_string()),
            Expr::Wildcard => Err("Unhandled syntax: Expr::Wildcard".to_string()),
            Expr::QualifiedWildcard(_) => {
                Err("Unhandled syntax: Expr::QualifiedWildcard".to_string())
            }
        }
    }

    fn traverse_in_subquery(&mut self, in_subquery: &Expr) -> Result<Expr, String> {
        match in_subquery {
            Expr::InSubquery {
                expr,
                negated,
                subquery,
            } => {
                let new_expr = self.traverse_expr(expr)?;

                let new_subquery = self.traverse_ast_query(subquery)?;

                Ok(Expr::InSubquery {
                    expr: Box::new(new_expr),
                    negated: *negated,
                    subquery: new_subquery,
                })
            }
            _ => unreachable!("Expected an InSubquery expression"),
        }
    }

    fn traverse_function(&mut self, func: &Function) -> Result<Function, String> {
        let Function {
            args,
            distinct,
            filter,
            name,
            null_treatment,
            order_by,
            over,
            // "special" apparently means the function's parentheses are omitted
            special,
        } = func;

        if *distinct {
            return Err("Unhandled syntax: Function::distinct".to_string());
        }

        if filter.is_some() {
            return Err("Unhandled syntax: Function::filter".to_string());
        }

        if null_treatment.is_some() {
            return Err("Unhandled syntax: Function::null_treatment".to_string());
        }

        if !order_by.is_empty() {
            return Err("Unhandled syntax: Function::order_by".to_string());
        }

        if over.is_some() {
            return Err("Unhandled syntax: Function::over".to_string());
        }

        validate_function_name(name)?;

        Ok(Function {
            name: name.clone(),
            special: *special,
            args: args
                .into_iter()
                .map(|arg| self.traverse_function_arg(arg))
                .collect::<Result<Vec<_>, _>>()?,

            distinct: false,
            filter: None,
            over: None,
            null_treatment: None,
            order_by: vec![],
        })
    }

    fn traverse_function_arg(&mut self, function_arg: &FunctionArg) -> Result<FunctionArg, String> {
        match function_arg {
            FunctionArg::Unnamed(function_arg_expr) => Ok(FunctionArg::Unnamed(
                self.traverse_function_arg_expr(function_arg_expr)?,
            )),

            FunctionArg::Named { .. } => Err("Unhandled syntax: FunctionArg::Named".to_string()),
        }
    }

    fn traverse_function_arg_expr(
        &mut self,
        function_arg_expr: &FunctionArgExpr,
    ) -> Result<FunctionArgExpr, String> {
        match function_arg_expr {
            FunctionArgExpr::Expr(expr) => Ok(FunctionArgExpr::Expr(self.traverse_expr(expr)?)),
            FunctionArgExpr::Wildcard => Ok(FunctionArgExpr::Wildcard),

            FunctionArgExpr::QualifiedWildcard(_) => {
                Err("Unhandled syntax: FunctionArgExpr::QualifiedWildcard".to_string())
            }
        }
    }

    fn traverse_value(&mut self, value: &Value) -> Result<Value, String> {
        match value {
            Value::Placeholder(s) => {
                if s == "?" {
                    Ok(value.clone())
                } else {
                    Err(format!("Unhandled value: Value::Placeholder({})", s))
                }
            }
            Value::SingleQuotedString(_) => Ok(value.clone()),
            Value::Number(_, _) => Ok(value.clone()),
            Value::Null => Ok(value.clone()),

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
        }
    }
}

fn translate_sql(query: &str) -> Result<TranslatedQuery<Vec<String>>, String> {
    let dialect = SQLiteDialect {};

    let ast = Parser::parse_sql(&dialect, query).map_err(|e| e.to_string())?;

    // println!("AST: {:#?}", ast);

    let mut path_convertor = PathConvertor::new();

    let query = path_convertor.traverse(ast)?;

    Ok(TranslatedQuery {
        databases: path_convertor.database_names,
        query: query.iter().map(|s| s.to_string()).collect(),
    })
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "RustResult<TranslatedQuery, string>")]
    pub type JsTranslatedQueryResult;
}

#[wasm_bindgen(typescript_custom_section)]
const TS_RUST_RESULT: &'static str = r#"
export type TranslatedQuery = {
    databases: Map<string, string>;
    query: string[];
};
"#;

#[wasm_bindgen(js_name = translateSql)]
pub fn translate_sql_wasm(query: &str) -> Result<JsTranslatedQueryResult, JsValue> {
    let result = translate_sql(query);
    serde_wasm_bindgen::to_value(&result)
        .map_err(|err| JsValue::from_str(&format!("{:?}", err)))
        .map(|x| x.into())
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

fn extract_unary_identifier(identifiers: &Vec<Ident>, object_type: &str) -> Result<String, String> {
    if identifiers.len() != 1 {
        return Err(format!(
            "Expected 1 identifiers for the {} name, got: {:?}",
            object_type,
            identifiers
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
        ));
    }

    Ok(identifiers[0].value.clone())
}

fn extract_binary_identifiers(
    identifiers: &Vec<Ident>,
    object_type: &str,
) -> Result<(String, String), String> {
    if identifiers.len() != 2 {
        return Err(format!(
            "Expected 1 identifiers for the {} name, got: {:?}",
            object_type,
            identifiers
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
        ));
    }

    Ok((identifiers[0].value.clone(), identifiers[1].value.clone()))
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

#[cfg(test)]
mod tests {
    use sqlparser::ast::{
        CharacterLength, ColumnDef, ColumnOption, ColumnOptionDef, ConstraintCharacteristics,
        DataType, Ident, ObjectName, Query, ReferentialAction,
    };

    use super::*;

    #[test]
    fn build_sql_test() {
        let blah = CreateTable {
            auto_increment_offset: Some(8),
            clone: None,
            cluster_by: None,
            collation: Some("utf8mb4_general_ci".to_string()),
            columns: vec![
                ColumnDef {
                    data_type: DataType::Varchar(Some(CharacterLength::IntegerLength {
                        length: 255,
                        unit: None,
                    })),
                    name: Ident::new("id"),
                    collation: None,
                    options: vec![ColumnOptionDef {
                        name: Some(Ident::new("primary".to_string())),
                        option: ColumnOption::ForeignKey {
                            foreign_table: ObjectName(vec![Ident::new("dwaynee")]),
                            referred_columns: vec![Ident::new("id")],
                            on_delete: Some(ReferentialAction::Cascade),
                            on_update: Some(ReferentialAction::Cascade),
                            characteristics: Some(ConstraintCharacteristics {
                                deferrable: Some(true),
                                initially: None,
                                enforced: None,
                            }),
                        },
                    }],
                },
                ColumnDef {
                    data_type: DataType::Varchar(Some(CharacterLength::IntegerLength {
                        length: 255,
                        unit: None,
                    })),
                    name: Ident::new("name"),
                    collation: None,
                    options: vec![ColumnOptionDef {
                        name: Some(Ident::new("primary".to_string())),
                        option: ColumnOption::Null,
                    }],
                },
            ],
            comment: Some("This is not a table".to_string()),
            constraints: vec![/* TableConstraint::Unique {
                name: Some(Ident::new("unique".to_string())),
                columns: vec![Ident::new("id")],
                is_primary: false,
                characteristics: Some(ConstraintCharacteristics {
                    deferrable: Some(true),
                    initially: None,
                    enforced: None,
                }),
            } */],
            default_charset: Some("utf8mb4".to_string()),
            engine: None,
            external: true,
            file_format: Some(ast::FileFormat::AVRO),
            global: Some(true),
            hive_distribution: HiveDistributionStyle::NONE,
            hive_formats: None,
            if_not_exists: false,
            like: None,
            location: Some("s3://bucket/prefix".to_string()),
            name: ObjectName(vec![Ident::new("dwayne")]),
            on_cluster: None,
            on_commit: Some(ast::OnCommit::Drop),
            options: None,
            or_replace: false,
            order_by: None,
            partition_by: None,
            query: Some(Box::new(Query {
                body: Box::new(SetExpr::Select(Box::new(Select {
                    distinct: None,
                    top: None,
                    projection: vec![SelectItem::Wildcard(WildcardAdditionalOptions {
                        opt_exclude: None,
                        opt_except: None,
                        opt_rename: None,
                        opt_replace: None,
                    })],
                    into: None,
                    from: vec![],
                    lateral_views: vec![],
                    selection: Some(Expr::Value(Value::Number("1".to_string(), true))),
                    group_by: GroupByExpr::Expressions(vec![]),
                    cluster_by: vec![],
                    distribute_by: vec![],
                    sort_by: vec![],
                    having: None,
                    named_window: vec![],
                    qualify: None,
                }))),
                with: None,
                order_by: vec![],
                limit: None,
                limit_by: vec![],
                offset: None,
                fetch: None,
                locks: vec![],
                for_clause: None,
            })),
            strict: true,
            table_properties: vec![],
            temporary: false,
            transient: false,
            with_options: vec![],
            without_rowid: true,
        };

        let compiled = blah.to_string();

        println!("Compiled: {}", compiled);
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn create_table_disallow_compound_identifier() {
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

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(
                err,
                "Expected 1 identifiers for the table name, got: [\"x\", \"y\"]"
            );
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
    }

    #[test]
    fn unsupported_statement_type() {
        let sql = r#"close my_eyes;"#;

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(err, "not implemented: Statement::Close");
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("sasas".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("/dev/null".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("etc/passwd".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/things".to_string(), "main".to_string()),
                    ("~/books/thongs".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![
                    [
                        &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                        "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                        "\"externalLink\" TEXT NOT NULL, ",
                        "\"externalLinkTitle\" TEXT NOT NULL, ",
                        "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                        "\"imageUrl\" TEXT NOT NULL, ",
                        "\"subtitle\" TEXT NULL, ",
                        "\"title\" TEXT NOT NULL",
                        ")",
                    ]
                    .join(""),
                    [
                        &format!("CREATE TABLE IF NOT EXISTS db1.{} (", VALUES_TABLE_NAME),
                        "\"title\" TEXT NOT NULL",
                        ")"
                    ]
                    .join("")
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn create_index() {
        let sql = r#"CREATE INDEX "scoreIndex" ON "~/books/eloScores" (score)"#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/eloScores".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![format!(
                    "CREATE INDEX main.{}scoreIndex ON {}(score)",
                    _VALUES_TABLE_INDEX_PREFIX, VALUES_TABLE_NAME
                )]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn create_index_compound_name() {
        let sql = r#"CREATE INDEX a.b ON "~/books/eloScores" (score)"#;

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(
                err,
                "Expected 1 identifiers for the index name, got: [\"a\", \"b\"]"
            );
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
    }

    #[test]
    fn create_index_compound_table_name() {
        let sql = r#"CREATE INDEX a ON x.y (score)"#;

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(
                err,
                "Expected 1 identifiers for the table name, got: [\"x\", \"y\"]"
            );
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
    }

    #[test]
    fn insert() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP)
        "#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/matches".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![
                    [
                        r#"INSERT INTO main.table_contents ("id", "loserId", "winnerId", "matchDate") VALUES "#,
                        r#"(?, ?, DATETIME('now'), CURRENT_TIMESTAMP)"#
                    ].join(""),
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn parser_error() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP('derp))
        "#;

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(
                err,
                "sql parser error: Unterminated string literal at Line: 3, Column 62"
            );
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![
                    [
                        "SELECT",
                        r#" "things".id, "things".title, "things".subtitle, "things".image_url, "things".external_link,"#,
                        r#" "things".external_link_title, "things".excluded"#,
                        r#" FROM main.table_contents AS "things""#,
                        r#" WHERE "things".id = ?"#,
                        " LIMIT 1",
                    ].join(""),
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![[
                        "SELECT",
                        " main.table_contents.id, main.table_contents.title, main.table_contents.subtitle,",
                        " main.table_contents.image_url, main.table_contents.external_link,",
                        " main.table_contents.external_link_title, main.table_contents.excluded",
                        " FROM main.table_contents",
                        " WHERE main.table_contents.id = ?",
                        " LIMIT 1",
                    ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/eloScores".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![[
                    "SELECT",
                    r#" "winner".score AS "winnerScore", "loser".score AS "loserScore""#,
                    r#" FROM main.table_contents AS "winner""#,
                    r#" JOIN main.table_contents AS "loser""#,
                    r#" WHERE "winner".thing_id = ? AND "loser".thing_id = ?"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn select_null() {
        let sql = "SELECT null";

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(databases.is_empty(), true,);

            assert_eq!(query, vec!["SELECT NULL".to_string()]);
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn update() {
        let sql = r#"UPDATE "~/books/eloScores" SET "score" = ? WHERE "thingId" = ?"#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/eloScores".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![
                    [r#"UPDATE main.table_contents SET "score" = ? WHERE "thingId" = ?"#,].join(""),
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/matches".to_string(), "main".to_string()),
                    ("~/books/eloScores".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![[
                    "SELECT",
                    r#" loserId, winnerId"#,
                    r#" FROM main.table_contents"#,
                    r#" WHERE loserId IN ("#,
                    r#"SELECT thingId"#,
                    r#" FROM db1.table_contents"#,
                    r#" ORDER BY score DESC, thingId DESC"#,
                    r#" LIMIT 15"#,
                    r#")"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/things".to_string(), "main".to_string()),
                    ("~/books/matches".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![[
                    "SELECT",
                    r#" "sq".id, "sq".num_matches"#,
                    r#" FROM ("#,
                    r#"SELECT "books".id, 0 AS "num_matches""#,
                    r#" FROM main.table_contents AS "books""#,
                    r#" LEFT JOIN db1.table_contents AS "matches""#,
                    r#" ON ("#,
                    r#""books".id = "matches".winner_id"#,
                    r#" OR "books".id = "matches".loser_id"#,
                    r#")"#,
                    r#" WHERE "matches".loser_id IS NULL"#,
                    r#" UNION ALL"#,
                    r#" SELECT winner_id AS "id", 1 AS "num_matches""#,
                    r#" FROM db1.table_contents"#,
                    r#" UNION ALL"#,
                    r#" SELECT loser_id AS "id", 1 AS "num_matches""#,
                    r#" FROM db1.table_contents"#,
                    r#") AS "sq""#,
                    r#" GROUP BY "sq".id"#,
                    r#" ORDER BY sum("sq".num_matches) ASC"#,
                    r#" LIMIT ?"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/eloScores".to_string(), "main".to_string()),
                    ("~/books/things".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![[
                    "SELECT",
                    r#" COUNT(*) AS "rank0""#,
                    r#" FROM main.table_contents AS "eloScores""#,
                    r#" JOIN main.table_contents AS "comparisonScore""#,
                    r#" ON "eloScores".thingId = ?"#,
                    r#" JOIN db1.table_contents AS "books""#,
                    r#" ON "books".id = "eloScores".thingId"#,
                    r#" WHERE"#,
                    r#" "books".excluded = ?"#,
                    r#" AND ("#,
                    r#""eloScores".score > "comparisonScore".score"#,
                    r#" OR ("#,
                    r#""eloScores".score = "comparisonScore".score"#,
                    r#" AND "eloScores".thingId > "comparisonScore".thingId"#,
                    r#")"#,
                    r#")"#,
                    r#" LIMIT ? OFFSET ?"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn and_another() {
        let sql = r#"
            select "id"
            from (
                select
                    "~/books/things"."id" as "id",
                    0 as "num_matches" from "~/books/things"
                left join "~/books/matches" on (
                    "~/books/matches"."winner_id" = "~/books/things"."id"
                    or "~/books/matches"."loser_id" = "~/books/things"."id"
                )
                where "~/books/matches"."loser_id" is null

                union all

                select "winner_id" as "id", 1 as "num_matches" from "~/books/matches"

                union all

                select "loser_id" as "id", 1 as "num_matches" from "~/books/matches"
            ) as "sq"
            group by "id"
            order by sum("num_matches")
            limit ?
        "#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/things".to_string(), "main".to_string()),
                    ("~/books/matches".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![[
                    r#"SELECT "id""#,
                    r#" FROM ("#,
                    r#"SELECT"#,
                    r#" main.table_contents.id AS "id","#,
                    r#" 0 AS "num_matches" FROM main.table_contents"#,
                    r#" LEFT JOIN db1.table_contents ON ("#,
                    r#"db1.table_contents.winner_id = main.table_contents.id"#,
                    r#" OR db1.table_contents.loser_id = main.table_contents.id"#,
                    r#")"#,
                    r#" WHERE db1.table_contents.loser_id IS NULL"#,
                    r#" UNION ALL"#,
                    r#" SELECT "winner_id" AS "id", 1 AS "num_matches" FROM db1.table_contents"#,
                    r#" UNION ALL"#,
                    r#" SELECT "loser_id" AS "id", 1 AS "num_matches" FROM db1.table_contents"#,
                    r#") AS "sq""#,
                    r#" GROUP BY "id""#,
                    r#" ORDER BY sum("num_matches")"#,
                    r#" LIMIT ?"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn select_wildcard() {
        let sql = r#"select * from "~/heyy""#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/heyy".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![[r#"SELECT *"#, r#" FROM main.table_contents"#,].join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn drop_table() {
        let sql = r#"drop table "~/heyy""#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/heyy".to_string(), "main".to_string()),]),
            );

            assert_eq!(query, vec![[r#"DROP TABLE main.table_contents"#,].join("")]);
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn drop_multiple_tables() {
        let sql = r#"drop table "~/heyy", "~/okokok""#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/heyy".to_string(), "main".to_string()),
                    ("~/okokok".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![[r#"DROP TABLE main.table_contents, db1.table_contents"#,].join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn on_conflict_do_nothing() {
        let sql = r#"
            insert into "~/my-data-scraper/reelgood/shows-and-movies"
            ("format", "isWatched", "name", "url", "imageUrl")
            values (?, ?, ?, ?, ?)
            on conflict do nothing
        "#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([(
                    "~/my-data-scraper/reelgood/shows-and-movies".to_string(),
                    "main".to_string()
                ),]),
            );

            assert_eq!(
                query,
                vec![[
                    r#"INSERT INTO main.table_contents"#,
                    r#" ("format", "isWatched", "name", "url", "imageUrl")"#,
                    r#" VALUES (?, ?, ?, ?, ?)"#,
                    r#" ON CONFLICT DO NOTHING"#
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let translate_result = translate_sql(sql);

        let TranslatedQuery { databases, query } = translate_result.unwrap();

        assert_eq!(
            databases,
            HashMap::from([
                ("~/books/non-things".to_string(), "main".to_string()),
                ("~/books/things".to_string(), "db1".to_string())
            ]),
        );

        assert_eq!(
            query,
            vec![[
                r#"SELECT "~/books/things".id, "~/books/things".title"#,
                r#" FROM main.table_contents AS "~/books/things""#,
                r#" UNION "#,
                r#"SELECT db1.table_contents.id, db1.table_contents.title"#,
                r#" FROM db1.table_contents"#,
            ]
            .join("")]
        );
    }
}
