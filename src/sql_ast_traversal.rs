mod ast_views;
mod sql_ast_traverser;

use self::{
    ast_views::{
        CreateIndexStatementViewMutable, CreateTableStatementViewMutable, DropStatementViewMutable,
        InSubqueryExprViewMutable, InsertStatementViewMutable, SetOperationViewMutable,
        TableFactorDerivedViewMut, TableFactorTableViewMut, UpdateStatementViewMutable,
    },
    sql_ast_traverser::{TraversalResult, VisitResult},
};
use super::{VALUES_TABLE_INDEX_PREFIX, VALUES_TABLE_NAME};
use sqlparser::ast::{
    self, Assignment, Expr, Function, FunctionArg, FunctionArgExpr, GroupByExpr,
    HiveDistributionStyle, HiveFormat, Ident, Join, JoinConstraint, JoinOperator, ObjectName,
    ObjectType, Offset, OnConflict, OnConflictAction, OnInsert, OrderByExpr, Select, SelectItem,
    SetExpr, TableAlias, TableFactor, TableWithJoins, Value, Values, WildcardAdditionalOptions,
};
use std::collections::HashMap;
use typed_path::Utf8UnixPathBuf;

pub use sql_ast_traverser::SqlAstTraverser;

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

    fn post_visit_create_table(
        &mut self,
        create_table: &mut CreateTableStatementViewMutable,
    ) -> VisitResult {
        let db_reference = convert_path_to_database(create_table.name, &mut self.database_names)?;

        *create_table.name = ObjectName(get_qualified_values_table_identifiers(&db_reference));

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
        validate_function_name(&func.name)
    }

    fn visit_value_placeholder(&mut self, value: &mut String) -> VisitResult {
        if value == "?" {
            Ok(())
        } else {
            Err(format!("Unhandled value: Value::Placeholder({})", value))
        }
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
            if_not_exists: _,
            name: _,
            columns: _,
            auto_increment_offset: _,
            clone,
            cluster_by,
            collation: _,
            comment,
            constraints,
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
            on_commit,
            options,
            or_replace: _,
            order_by,
            partition_by,
            query,
            strict: _,
            table_properties,
            temporary,
            transient,
            with_options,
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

        if !constraints.is_empty() {
            return Err("not implemented: CreateTable::constraints".to_string());
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

        if **temporary {
            return Err("not implemented: CreateTable::temporary".to_string());
        }

        if **transient {
            return Err("not implemented CreateTable::transient".to_string());
        }

        if !with_options.is_empty() {
            return Err("not implemented: CreateTable::with_options".to_string());
        }

        if **without_rowid {
            return Err("not implemented: CreateTable::without_rowid".to_string());
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
            name,
            table_name: _,
            unique: _,
            if_not_exists: _,
            nulls_distinct: _,
            concurrently,
            include,
            predicate,
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

        if predicate.is_some() {
            return Err("not implemented: CreateIndex::predicate".to_string());
        }

        if using.is_some() {
            return Err("not implemented: CreateIndex::using".to_string());
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
            after_columns,
            on,
            partitioned,
            priority,
            returning,
        } = insert;

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
            conflict_target,
        } = on_conflict;

        if conflict_target.is_some() {
            return Err("not implemented: OnConflict::conflict_target".to_string());
        }

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

    fn traverse_set_expr(&mut self, body: &mut SetExpr) -> TraversalResult {
        self.pre_visit_set_expr(body)?;

        match body {
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
                self.traverse_set_operation(&mut body.try_into().unwrap())
            }

            SetExpr::Query(_) => Err("not implemented: SetExpr::Query".to_string()),
            SetExpr::Insert(_) => Err("not implemented: SetExpr::Insert".to_string()),
            SetExpr::Update(_) => Err("not implemented: SetExpr::Update".to_string()),
            SetExpr::Table(_) => Err("not implemented: SetExpr::Table".to_string()),
        }?;

        self.post_visit_set_expr(body)
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

            JoinConstraint::Using(_) => Err("not implemented: JoinConstraint::Using".to_string()),
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

    fn traverse_table_factor_table(
        &mut self,
        relation: &mut TableFactorTableViewMut,
    ) -> TraversalResult {
        self.pre_visit_table_factor_table(relation)?;

        let TableFactorTableViewMut {
            alias: _,
            args,
            name: _,
            partitions,
            version,
            with_hints,
        } = relation;

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

        self.post_visit_table_factor_table(relation)
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

    fn traverse_compound_identifier(&mut self, identifiers: &mut Vec<Ident>) -> TraversalResult {
        // TODO: pre and post not necessary
        self.pre_visit_compound_identifier(identifiers)?;
        self.post_visit_compound_identifier(identifiers)
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
            distinct,
            filter,
            name,
            null_treatment,
            order_by,
            over,
            // "special" apparently means the function's parentheses are omitted
            special: _,
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

            FunctionArgExpr::QualifiedWildcard(_) => {
                Err("Unhandled syntax: FunctionArgExpr::QualifiedWildcard".to_string())
            }
        }?;

        self.post_visit_function_arg_expr(function_arg_expr)
    }

    fn traverse_value(&mut self, value: &mut Value) -> TraversalResult {
        self.pre_visit_value(value)?;

        match value {
            Value::Placeholder(s) => self.traverse_value_placeholder(s),
            Value::SingleQuotedString(_) => Ok(()),
            Value::Number(_, _) => Ok(()),
            Value::Null => Ok(()),

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

        self.post_visit_value(value)
    }

    fn traverse_value_placeholder(&mut self, value: &mut String) -> TraversalResult {
        self.visit_value_placeholder(value)
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
