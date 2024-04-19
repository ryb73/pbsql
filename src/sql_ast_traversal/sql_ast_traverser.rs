use sqlparser::ast::{
    self, Assignment, Expr, Function, FunctionArg, FunctionArgExpr, Ident, Join, JoinConstraint,
    JoinOperator, Offset, OnConflict, OnInsert, OrderByExpr, Select, SelectItem, SetExpr,
    Statement::{self, CreateIndex, CreateTable, Insert},
    TableFactor, TableWithJoins, Value, WildcardAdditionalOptions,
};

use super::ast_views::{
    CreateIndexStatementViewMutable, CreateTableStatementViewMutable, DropStatementViewMutable,
    InSubqueryExprViewMutable, InsertStatementViewMutable, SetOperationViewMutable,
    TableFactorDerivedViewMut, TableFactorTableViewMut, UpdateStatementViewMutable,
};

pub type VisitResult = Result<(), String>;
pub type TraversalResult = Result<(), String>;

pub trait SqlAstTraverser<Error = String> {
    fn pre_visit_create_table(
        &mut self,
        _create_table: &mut CreateTableStatementViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_create_table(
        &mut self,
        _create_table: &mut CreateTableStatementViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_statement(&mut self, _statement: &mut Statement) -> VisitResult {
        Ok(())
    }

    fn post_visit_statement(&mut self, _statement: &mut Statement) -> VisitResult {
        Ok(())
    }

    fn pre_visit_drop(&mut self, _drop: &mut DropStatementViewMutable) -> VisitResult {
        Ok(())
    }

    fn post_visit_drop(&mut self, _drop: &mut DropStatementViewMutable) -> VisitResult {
        Ok(())
    }

    fn pre_visit_update(&mut self, _update: &mut UpdateStatementViewMutable) -> VisitResult {
        Ok(())
    }

    fn post_visit_update(&mut self, _update: &mut UpdateStatementViewMutable) -> VisitResult {
        Ok(())
    }

    fn pre_visit_assignment(&mut self, _assignment: &mut Assignment) -> VisitResult {
        Ok(())
    }

    fn post_visit_assignment(&mut self, _assignment: &mut Assignment) -> VisitResult {
        Ok(())
    }

    fn pre_visit_create_index(
        &mut self,
        _create_index: &mut CreateIndexStatementViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_create_index(
        &mut self,
        _create_index: &mut CreateIndexStatementViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_insert(&mut self, _insert: &mut InsertStatementViewMutable) -> VisitResult {
        Ok(())
    }

    fn post_visit_insert(&mut self, _insert: &mut InsertStatementViewMutable) -> VisitResult {
        Ok(())
    }

    fn pre_visit_on_insert(&mut self, _on_insert: &mut Option<OnInsert>) -> VisitResult {
        Ok(())
    }

    fn post_visit_on_insert(&mut self, _on_insert: &mut Option<OnInsert>) -> VisitResult {
        Ok(())
    }

    fn pre_visit_on_conflict(&mut self, _on_conflict: &mut OnConflict) -> VisitResult {
        Ok(())
    }

    fn post_visit_on_conflict(&mut self, _on_conflict: &mut OnConflict) -> VisitResult {
        Ok(())
    }

    fn pre_visit_ast_query(&mut self, _query: &mut Box<ast::Query>) -> VisitResult {
        Ok(())
    }

    fn post_visit_ast_query(&mut self, _query: &mut Box<ast::Query>) -> VisitResult {
        Ok(())
    }

    fn pre_visit_offset(&mut self, _offset: &mut Offset) -> VisitResult {
        Ok(())
    }

    fn post_visit_offset(&mut self, _offset: &mut Offset) -> VisitResult {
        Ok(())
    }

    fn pre_visit_order_by_expr(&mut self, _expr: &mut OrderByExpr) -> VisitResult {
        Ok(())
    }

    fn post_visit_order_by_expr(&mut self, _expr: &mut OrderByExpr) -> VisitResult {
        Ok(())
    }

    fn pre_visit_set_expr(&mut self, _body: &mut SetExpr) -> VisitResult {
        Ok(())
    }

    fn post_visit_set_expr(&mut self, _body: &mut SetExpr) -> VisitResult {
        Ok(())
    }

    fn pre_visit_set_operation_left(&mut self, _body: &mut SetOperationViewMutable) -> VisitResult {
        Ok(())
    }

    fn pre_visit_set_operation_right(
        &mut self,
        _body: &mut SetOperationViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_set_operation(&mut self, _body: &mut SetOperationViewMutable) -> VisitResult {
        Ok(())
    }

    fn pre_visit_table_with_joins(
        &mut self,
        _table_with_joins: &mut TableWithJoins,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_table_with_joins(
        &mut self,
        _table_with_joins: &mut TableWithJoins,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_join(&mut self, _join: &mut Join) -> VisitResult {
        Ok(())
    }

    fn post_visit_join(&mut self, _join: &mut Join) -> VisitResult {
        Ok(())
    }

    fn pre_visit_join_operator(&mut self, _join_operator: &mut JoinOperator) -> VisitResult {
        Ok(())
    }

    fn post_visit_join_operator(&mut self, _join_operator: &mut JoinOperator) -> VisitResult {
        Ok(())
    }

    fn pre_visit_join_constraint(&mut self, _constraint: &mut JoinConstraint) -> VisitResult {
        Ok(())
    }

    fn post_visit_join_constraint(&mut self, _constraint: &mut JoinConstraint) -> VisitResult {
        Ok(())
    }

    fn pre_visit_table_factor(&mut self, _relation: &mut TableFactor) -> VisitResult {
        Ok(())
    }

    fn post_visit_table_factor(&mut self, _relation: &mut TableFactor) -> VisitResult {
        Ok(())
    }

    fn pre_visit_table_factor_derived(
        &mut self,
        _relation: &mut TableFactorDerivedViewMut,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_table_factor_derived(
        &mut self,
        _relation: &mut TableFactorDerivedViewMut,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_table_factor_table(
        &mut self,
        _relation: &mut TableFactorTableViewMut,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_table_factor_table(
        &mut self,
        _relation: &mut TableFactorTableViewMut,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_select_tables(&mut self, _tables: &mut Vec<TableWithJoins>) -> VisitResult {
        Ok(())
    }

    fn post_visit_select_tables(&mut self, _tables: &mut Vec<TableWithJoins>) -> VisitResult {
        Ok(())
    }

    fn pre_visit_select(&mut self, _select: &mut Box<Select>) -> VisitResult {
        Ok(())
    }

    fn post_visit_select(&mut self, _select: &mut Box<Select>) -> VisitResult {
        Ok(())
    }

    fn pre_visit_select_item(&mut self, _select_item: &mut SelectItem) -> VisitResult {
        Ok(())
    }

    fn post_visit_select_item(&mut self, _select_item: &mut SelectItem) -> VisitResult {
        Ok(())
    }

    fn pre_visit_expr(&mut self, _expr: &mut Expr) -> VisitResult {
        Ok(())
    }

    fn post_visit_expr(&mut self, _expr: &mut Expr) -> VisitResult {
        Ok(())
    }

    fn pre_visit_compound_identifier(&mut self, _identifiers: &mut Vec<Ident>) -> VisitResult {
        Ok(())
    }

    fn post_visit_compound_identifier(&mut self, _identifiers: &mut Vec<Ident>) -> VisitResult {
        Ok(())
    }

    fn pre_visit_in_subquery(
        &mut self,
        _in_subquery: &mut InSubqueryExprViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_in_subquery(
        &mut self,
        _in_subquery: &mut InSubqueryExprViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_function(&mut self, _func: &mut Function) -> VisitResult {
        Ok(())
    }

    fn post_visit_function(&mut self, _func: &mut Function) -> VisitResult {
        Ok(())
    }

    fn pre_visit_function_arg(&mut self, _function_arg: &mut FunctionArg) -> VisitResult {
        Ok(())
    }

    fn post_visit_function_arg(&mut self, _function_arg: &mut FunctionArg) -> VisitResult {
        Ok(())
    }

    fn pre_visit_function_arg_expr(
        &mut self,
        _function_arg_expr: &mut FunctionArgExpr,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_function_arg_expr(
        &mut self,
        _function_arg_expr: &mut FunctionArgExpr,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_wildcard_additional_options(
        &mut self,
        _wildcard_additional_options: &mut WildcardAdditionalOptions,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_wildcard_additional_options(
        &mut self,
        _wildcard_additional_options: &mut WildcardAdditionalOptions,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_value(&mut self, _value: &mut Value) -> VisitResult {
        Ok(())
    }

    fn post_visit_value(&mut self, _value: &mut Value) -> VisitResult {
        Ok(())
    }

    fn visit_value_placeholder(&mut self, _value: &mut String) -> VisitResult {
        Ok(())
    }

    fn traverse(&mut self, ast: &mut Vec<Statement>) -> TraversalResult {
        ast.into_iter()
            .map(|s| self.traverse_statement(s))
            .collect()
    }

    fn traverse_statement(&mut self, statement: &mut Statement) -> TraversalResult {
        self.pre_visit_statement(statement)?;

        match statement {
            CreateTable { .. } => self.traverse_create_table(&mut statement.try_into().unwrap()),
            CreateIndex { .. } => self.traverse_create_index(&mut statement.try_into().unwrap()),
            Insert { .. } => self.traverse_insert(&mut statement.try_into().unwrap()),
            Statement::Update { .. } => self.traverse_update(&mut statement.try_into().unwrap()),
            Statement::Query(query) => self.traverse_ast_query(query),
            Statement::Drop { .. } => self.traverse_drop(&mut statement.try_into().unwrap()),

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
        }?;

        self.post_visit_statement(statement)
    }

    fn traverse_drop(&mut self, drop: &mut DropStatementViewMutable) -> TraversalResult {
        // TODO: pre and post not necessary
        self.pre_visit_drop(drop)?;
        self.post_visit_drop(drop)
    }

    fn traverse_update(&mut self, update: &mut UpdateStatementViewMutable) -> TraversalResult {
        self.pre_visit_update(update)?;

        let UpdateStatementViewMutable {
            assignments,
            from,
            returning,
            selection,
            table,
        } = update;

        if returning.is_some() {
            return Err("not implemented: Statement::Update::returning".to_string());
        }

        self.traverse_table_with_joins(table)?;

        from.as_mut()
            .map(|f| self.traverse_table_with_joins(f))
            .transpose()?;

        assignments
            .iter_mut()
            .map(|assignment| self.traverse_assignment(assignment))
            .collect::<Result<Vec<_>, _>>()?;

        selection
            .as_mut()
            .map(|s| self.traverse_expr(s))
            .transpose()?;

        self.post_visit_update(update)
    }

    fn traverse_assignment(&mut self, assignment: &mut Assignment) -> TraversalResult {
        self.pre_visit_assignment(assignment)?;

        let Assignment { id: _, value } = assignment;
        self.traverse_expr(value)?;

        self.post_visit_assignment(assignment)
    }

    fn traverse_create_table(
        &mut self,
        create_table: &mut CreateTableStatementViewMutable,
    ) -> TraversalResult {
        self.pre_visit_create_table(create_table)?;

        let CreateTableStatementViewMutable {
            auto_increment_offset: _,
            clone: _,
            cluster_by: _,
            collation: _,
            columns: _,
            comment: _,
            constraints,
            default_charset: _,
            engine: _,
            external: _,
            file_format: _,
            global: _,
            hive_distribution: _,
            hive_formats: _,
            if_not_exists: _,
            like: _,
            location: _,
            name: _,
            on_cluster: _,
            on_commit,
            options,
            or_replace: _,
            order_by: _,
            partition_by,
            query,
            strict: _,
            table_properties,
            temporary: _,
            transient: _,
            with_options,
            without_rowid: _,
        } = create_table;

        if !constraints.is_empty() {
            return Err("not implemented: CreateTable::constraints".to_string());
        }

        if on_commit.is_some() {
            return Err("not implemented: CreateTable::on_commit".to_string());
        }

        if options.is_some() {
            return Err("not implemented: CreateTable::options".to_string());
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

        if !with_options.is_empty() {
            return Err("not implemented: CreateTable::with_options".to_string());
        }

        self.post_visit_create_table(create_table)
    }

    fn traverse_create_index(
        &mut self,
        create_index: &mut CreateIndexStatementViewMutable,
    ) -> TraversalResult {
        self.pre_visit_create_index(create_index)?;

        let CreateIndexStatementViewMutable {
            columns,
            name: _,
            table_name: _,
            unique: _,
            if_not_exists: _,
            nulls_distinct: _,
            concurrently: _,
            include: _,
            predicate,
            using: _,
        } = create_index;

        if predicate.is_some() {
            return Err("not implemented: CreateIndex::predicate".to_string());
        }

        columns
            .iter_mut()
            .map(|c| self.traverse_order_by_expr(c))
            .collect::<Result<Vec<_>, _>>()?;

        self.post_visit_create_index(create_index)
    }

    fn traverse_insert(&mut self, insert: &mut InsertStatementViewMutable) -> TraversalResult {
        self.pre_visit_insert(insert)?;

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
            source,
            after_columns: _,
            on,
            partitioned,
            priority: _,
            returning,
        } = insert;

        if partitioned.is_some() {
            return Err("not implemented: Insert::partitioned".to_string());
        }

        if returning.is_some() {
            return Err("not implemented: Insert::returning".to_string());
        }

        source
            .as_mut()
            .map(|s| self.traverse_ast_query(s))
            .unwrap_or(Ok(()))?;

        self.traverse_on_insert(on)?;

        self.post_visit_insert(insert)
    }

    fn traverse_on_insert(&mut self, on_insert: &mut Option<OnInsert>) -> TraversalResult {
        self.pre_visit_on_insert(on_insert)?;

        match on_insert {
            Some(OnInsert::OnConflict(on_conflict)) => self.traverse_on_conflict(on_conflict),
            None => Ok(()),

            Some(OnInsert::DuplicateKeyUpdate(..)) => {
                Err("not implemented: OnInsert::DuplicateKeyUpdate".to_string())
            }

            &mut Some(_) => Err("Unrecognized OnInsert variant".to_string()),
        }?;

        self.post_visit_on_insert(on_insert)
    }

    fn traverse_on_conflict(&mut self, on_conflict: &mut OnConflict) -> TraversalResult;
    fn traverse_ast_query(&mut self, query: &mut Box<ast::Query>) -> TraversalResult;
    fn traverse_offset(&mut self, offset: &mut Offset) -> TraversalResult;
    fn traverse_order_by_expr(&mut self, expr: &mut OrderByExpr) -> TraversalResult;
    fn traverse_set_expr(&mut self, body: &mut SetExpr) -> TraversalResult;
    fn traverse_set_operation(&mut self, body: &mut SetOperationViewMutable) -> TraversalResult;
    fn traverse_table_with_joins(
        &mut self,
        table_with_joins: &mut TableWithJoins,
    ) -> TraversalResult;
    fn traverse_join(&mut self, join: &mut Join) -> TraversalResult;
    fn traverse_join_operator(&mut self, join_operator: &mut JoinOperator) -> TraversalResult;
    fn traverse_join_constraint(&mut self, constraint: &mut JoinConstraint) -> TraversalResult;
    fn traverse_table_factor(&mut self, relation: &mut TableFactor) -> TraversalResult;
    fn traverse_table_factor_derived(
        &mut self,
        relation: &mut TableFactorDerivedViewMut,
    ) -> TraversalResult;
    fn traverse_table_factor_table(
        &mut self,
        relation: &mut TableFactorTableViewMut,
    ) -> TraversalResult;
    fn traverse_select_tables(&mut self, tables: &mut Vec<TableWithJoins>) -> TraversalResult;
    fn traverse_select(&mut self, select: &mut Box<Select>) -> TraversalResult;
    fn traverse_select_item(&mut self, select_item: &mut SelectItem) -> TraversalResult;
    fn traverse_expr(&mut self, expr: &mut Expr) -> TraversalResult;
    fn traverse_compound_identifier(&mut self, identifiers: &mut Vec<Ident>) -> TraversalResult;
    fn traverse_in_subquery(
        &mut self,
        in_subquery: &mut InSubqueryExprViewMutable,
    ) -> TraversalResult;
    fn traverse_function(&mut self, func: &mut Function) -> TraversalResult;
    fn traverse_function_arg(&mut self, function_arg: &mut FunctionArg) -> TraversalResult;
    fn traverse_function_arg_expr(
        &mut self,
        function_arg_expr: &mut FunctionArgExpr,
    ) -> TraversalResult;
    fn traverse_wildcard_additional_options(
        &mut self,
        wildcard_additional_options: &mut WildcardAdditionalOptions,
    ) -> TraversalResult;
    fn traverse_value(&mut self, value: &mut Value) -> TraversalResult;
    fn traverse_value_placeholder(&mut self, value: &mut String) -> TraversalResult;
}
