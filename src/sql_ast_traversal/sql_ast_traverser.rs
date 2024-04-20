use sqlparser::ast::{
    self, Assignment, Expr, Function, FunctionArg, FunctionArgExpr, GroupByExpr, Ident, Join,
    JoinConstraint, JoinOperator, Offset, OnConflict, OnConflictAction, OnInsert, OrderByExpr,
    Select, SelectItem, SetExpr,
    Statement::{self, CreateIndex, CreateTable, Insert},
    TableFactor, TableWithJoins, Value, Values, WildcardAdditionalOptions,
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

    fn pre_visit_set_operation_left(
        &mut self,
        _set_operation: &mut SetOperationViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn pre_visit_set_operation_right(
        &mut self,
        _set_operation: &mut SetOperationViewMutable,
    ) -> VisitResult {
        Ok(())
    }

    fn post_visit_set_operation(
        &mut self,
        _set_operation: &mut SetOperationViewMutable,
    ) -> VisitResult {
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

    fn traverse_on_conflict(&mut self, on_conflict: &mut OnConflict) -> TraversalResult {
        self.pre_visit_on_conflict(on_conflict)?;

        let OnConflict {
            action,
            conflict_target: _,
        } = on_conflict;

        match action {
            OnConflictAction::DoNothing => Ok(()),

            OnConflictAction::DoUpdate(_) => {
                Err("not implemented: OnConflictAction::DoUpdate".to_string())
            }
        }?;

        self.post_visit_on_conflict(on_conflict)
    }

    fn traverse_ast_query(&mut self, query: &mut Box<ast::Query>) -> TraversalResult {
        self.pre_visit_ast_query(query)?;

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
        } = query.as_mut();

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

        self.traverse_set_expr(body)?;

        order_by
            .iter_mut()
            .map(|expr| self.traverse_order_by_expr(expr))
            .collect::<Result<Vec<_>, _>>()?;

        offset
            .as_mut()
            .map(|o| self.traverse_offset(o))
            .transpose()?;

        limit.as_mut().map(|l| self.traverse_expr(l)).transpose()?;

        self.post_visit_ast_query(query)
    }

    fn traverse_offset(&mut self, offset: &mut Offset) -> TraversalResult {
        self.pre_visit_offset(offset)?;

        let Offset { value, rows: _ } = offset;
        self.traverse_expr(value)?;

        self.post_visit_offset(offset)
    }

    fn traverse_order_by_expr(&mut self, order_by_expr: &mut OrderByExpr) -> TraversalResult {
        self.pre_visit_order_by_expr(order_by_expr)?;

        let OrderByExpr {
            expr,
            asc: _,
            nulls_first: _,
        } = order_by_expr;
        self.traverse_expr(expr)?;

        self.post_visit_order_by_expr(order_by_expr)
    }

    fn traverse_set_expr(&mut self, set_expr: &mut SetExpr) -> TraversalResult {
        self.pre_visit_set_expr(set_expr)?;

        match set_expr {
            SetExpr::Values(Values {
                explicit_row: _,
                rows,
            }) => {
                rows.into_iter()
                    .map(|row| {
                        row.into_iter()
                            .map(|expr| self.traverse_expr(expr))
                            .collect::<Result<Vec<_>, _>>()
                            .map_err(|e| e.to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(())
            }
            SetExpr::Select(select) => self.traverse_select(select),
            SetExpr::SetOperation { .. } => {
                self.traverse_set_operation(&mut set_expr.try_into().unwrap())
            }

            SetExpr::Query(_) => Err("not implemented: SetExpr::Query".to_string()),
            SetExpr::Insert(_) => Err("not implemented: SetExpr::Insert".to_string()),
            SetExpr::Update(_) => Err("not implemented: SetExpr::Update".to_string()),
            SetExpr::Table(_) => Err("not implemented: SetExpr::Table".to_string()),
        }?;

        self.post_visit_set_expr(set_expr)
    }

    fn traverse_set_operation(
        &mut self,
        set_operation: &mut SetOperationViewMutable,
    ) -> TraversalResult {
        self.pre_visit_set_operation_left(set_operation)?;

        self.traverse_set_expr(set_operation.left)?;

        self.pre_visit_set_operation_right(set_operation)?;

        self.traverse_set_expr(set_operation.right)?;

        self.post_visit_set_operation(set_operation)
    }

    fn traverse_table_with_joins(
        &mut self,
        table_with_joins: &mut TableWithJoins,
    ) -> TraversalResult {
        self.pre_visit_table_with_joins(table_with_joins)?;

        let TableWithJoins { relation, joins } = table_with_joins;

        self.traverse_table_factor(relation)?;

        joins
            .iter_mut()
            .map(|join| self.traverse_join(join))
            .collect::<Result<Vec<_>, _>>()?;

        self.post_visit_table_with_joins(table_with_joins)
    }

    fn traverse_join(&mut self, join: &mut Join) -> TraversalResult {
        self.pre_visit_join(join)?;

        match join {
            Join {
                join_operator,
                relation,
            } => {
                self.traverse_table_factor(relation)?;

                self.traverse_join_operator(join_operator)
            }
        }?;

        self.post_visit_join(join)
    }

    fn traverse_join_operator(&mut self, join_operator: &mut JoinOperator) -> TraversalResult {
        self.pre_visit_join_operator(join_operator)?;

        match join_operator {
            JoinOperator::Inner(constraint) => self.traverse_join_constraint(constraint),
            JoinOperator::LeftOuter(constraint) => self.traverse_join_constraint(constraint),
            JoinOperator::RightOuter(constraint) => self.traverse_join_constraint(constraint),
            JoinOperator::FullOuter(constraint) => self.traverse_join_constraint(constraint),
            JoinOperator::LeftSemi(constraint) => self.traverse_join_constraint(constraint),
            JoinOperator::RightSemi(constraint) => self.traverse_join_constraint(constraint),
            JoinOperator::LeftAnti(constraint) => self.traverse_join_constraint(constraint),
            JoinOperator::RightAnti(constraint) => self.traverse_join_constraint(constraint),

            JoinOperator::CrossApply => Ok(()),
            JoinOperator::CrossJoin => Ok(()),
            JoinOperator::OuterApply => Ok(()),
        }?;

        self.post_visit_join_operator(join_operator)
    }

    fn traverse_join_constraint(&mut self, constraint: &mut JoinConstraint) -> TraversalResult {
        self.pre_visit_join_constraint(constraint)?;

        match constraint {
            JoinConstraint::On(expr) => self.traverse_expr(expr),
            JoinConstraint::Natural => Ok(()),
            JoinConstraint::None => Ok(()),
            JoinConstraint::Using(_) => Ok(()),
        }?;

        self.post_visit_join_constraint(constraint)
    }

    fn traverse_table_factor(&mut self, relation: &mut TableFactor) -> TraversalResult {
        self.pre_visit_table_factor(relation)?;

        match relation {
            TableFactor::Table { .. } => {
                self.traverse_table_factor_table(&mut relation.try_into().unwrap())
            }
            TableFactor::Derived { .. } => {
                self.traverse_table_factor_derived(&mut relation.try_into().unwrap())
            }

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
        }?;

        self.post_visit_table_factor(relation)
    }

    fn traverse_table_factor_derived(
        &mut self,
        derived_table_factor_view: &mut TableFactorDerivedViewMut,
    ) -> TraversalResult {
        self.pre_visit_table_factor_derived(derived_table_factor_view)?;

        let TableFactorDerivedViewMut {
            lateral: _,
            subquery,
            alias: _,
        } = derived_table_factor_view;

        self.traverse_ast_query(subquery)?;

        self.post_visit_table_factor_derived(derived_table_factor_view)
    }

    fn traverse_table_factor_table(
        &mut self,
        relation: &mut TableFactorTableViewMut,
    ) -> TraversalResult {
        self.pre_visit_table_factor_table(relation)?;

        let TableFactorTableViewMut {
            alias: _,
            args,
            name: _,
            partitions: _,
            version,
            with_hints,
        } = relation;

        if args.is_some() {
            return Err("not implemented: TableFactor::Table::args".to_string());
        }

        if version.is_some() {
            return Err("not implemented: TableFactor::Table::version".to_string());
        }

        if !with_hints.is_empty() {
            return Err("not implemented: TableFactor::Table::with_hints".to_string());
        }

        self.post_visit_table_factor_table(relation)
    }

    fn traverse_select_tables(&mut self, tables: &mut Vec<TableWithJoins>) -> TraversalResult {
        self.pre_visit_select_tables(tables)?;

        tables
            .iter_mut()
            .map(|t| self.traverse_table_with_joins(t))
            .collect::<Result<Vec<_>, String>>()?;

        self.post_visit_select_tables(tables)
    }

    fn traverse_select(&mut self, select: &mut Box<Select>) -> TraversalResult {
        self.pre_visit_select(select)?;

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
        } = select.as_mut();

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

        self.traverse_select_tables(from)?;

        projection
            .iter_mut()
            .map(|item| self.traverse_select_item(item))
            .collect::<Result<Vec<_>, _>>()?;

        selection
            .as_mut()
            .map(|expr| self.traverse_expr(expr))
            .transpose()?;

        match group_by {
            GroupByExpr::Expressions(expressions) => {
                expressions
                    .iter_mut()
                    .map(|expr| self.traverse_expr(expr))
                    .collect::<Result<Vec<_>, _>>()?;
            }
            GroupByExpr::All => return Err("not implemented: GroupByExpr::All".to_string()),
        };

        self.post_visit_select(select)
    }

    fn traverse_select_item(&mut self, select_item: &mut SelectItem) -> TraversalResult {
        self.pre_visit_select_item(select_item)?;

        match select_item {
            SelectItem::UnnamedExpr(expr) => self.traverse_expr(expr),
            SelectItem::ExprWithAlias { expr, alias: _ } => self.traverse_expr(expr),
            SelectItem::Wildcard(wildcard_options) => {
                self.traverse_wildcard_additional_options(wildcard_options)
            }

            SelectItem::QualifiedWildcard(_, _) => {
                Err("not implemented: SelectItem::QualifiedWildcard".to_string())
            }
        }?;

        self.post_visit_select_item(select_item)
    }

    fn traverse_expr(&mut self, expr: &mut Expr) -> TraversalResult {
        self.pre_visit_expr(expr)?;

        match expr {
            Expr::Value(value) => self.traverse_value(value),
            Expr::Function(func) => self.traverse_function(func),
            Expr::CompoundIdentifier(identifiers) => self.traverse_compound_identifier(identifiers),
            Expr::BinaryOp { left, op: _, right } => {
                self.traverse_expr(left)?;
                self.traverse_expr(right)
            }
            Expr::Identifier(_) => Ok(()),
            Expr::InSubquery { .. } => self.traverse_in_subquery(&mut expr.try_into().unwrap()),
            Expr::IsNull(expr) => self.traverse_expr(expr),
            Expr::Nested(nested_expr) => self.traverse_expr(nested_expr),

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
        }?;

        self.post_visit_expr(expr)
    }

    fn traverse_compound_identifier(&mut self, identifiers: &mut Vec<Ident>) -> TraversalResult {
        // TODO: pre and post not necessary
        self.pre_visit_compound_identifier(identifiers)?;
        self.post_visit_compound_identifier(identifiers)
    }

    fn traverse_in_subquery(
        &mut self,
        in_subquery: &mut InSubqueryExprViewMutable,
    ) -> TraversalResult {
        self.pre_visit_in_subquery(in_subquery)?;

        let InSubqueryExprViewMutable {
            expr,
            negated: _,
            subquery,
        } = in_subquery;

        self.traverse_expr(expr)?;

        self.traverse_ast_query(subquery)?;

        self.post_visit_in_subquery(in_subquery)
    }

    fn traverse_function(&mut self, func: &mut Function) -> TraversalResult {
        self.pre_visit_function(func)?;

        let Function {
            args,
            distinct: _,
            filter,
            name,
            null_treatment: _,
            order_by,
            over,
            // "special" apparently means the function's parentheses are omitted
            special: _,
        } = func;

        if filter.is_some() {
            return Err("Unhandled syntax: Function::filter".to_string());
        }

        if !order_by.is_empty() {
            return Err("Unhandled syntax: Function::order_by".to_string());
        }

        if over.is_some() {
            return Err("Unhandled syntax: Function::over".to_string());
        }

        args.iter_mut()
            .map(|arg| self.traverse_function_arg(arg))
            .collect::<Result<Vec<_>, _>>()?;

        self.post_visit_function(func)
    }

    fn traverse_function_arg(&mut self, function_arg: &mut FunctionArg) -> TraversalResult {
        self.pre_visit_function_arg(function_arg)?;

        match function_arg {
            FunctionArg::Unnamed(function_arg_expr) => {
                self.traverse_function_arg_expr(function_arg_expr)
            }

            FunctionArg::Named { .. } => Err("Unhandled syntax: FunctionArg::Named".to_string()),
        }?;

        self.post_visit_function_arg(function_arg)
    }

    fn traverse_function_arg_expr(
        &mut self,
        function_arg_expr: &mut FunctionArgExpr,
    ) -> TraversalResult {
        self.pre_visit_function_arg_expr(function_arg_expr)?;

        match function_arg_expr {
            FunctionArgExpr::Expr(expr) => self.traverse_expr(expr),
            FunctionArgExpr::Wildcard => Ok(()),
            FunctionArgExpr::QualifiedWildcard(_) => Ok(()),
        }?;

        self.post_visit_function_arg_expr(function_arg_expr)
    }

    fn traverse_wildcard_additional_options(
        &mut self,
        wildcard_additional_options: &mut WildcardAdditionalOptions,
    ) -> TraversalResult {
        self.pre_visit_wildcard_additional_options(wildcard_additional_options)?;

        let WildcardAdditionalOptions {
            opt_except,
            opt_exclude,
            opt_rename,
            opt_replace,
        } = wildcard_additional_options;

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

        self.post_visit_wildcard_additional_options(wildcard_additional_options)
    }

    fn traverse_value(&mut self, value: &mut Value) -> TraversalResult;
    fn traverse_value_placeholder(&mut self, value: &mut String) -> TraversalResult;
}
