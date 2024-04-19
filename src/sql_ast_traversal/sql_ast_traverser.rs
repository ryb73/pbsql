use sqlparser::ast::{
    self, Assignment, Expr, Function, FunctionArg, FunctionArgExpr, Ident, Join, JoinConstraint,
    JoinOperator, Offset, OnConflict, OnInsert, OrderByExpr, Select, SelectItem, SetExpr,
    Statement, TableFactor, TableWithJoins, Value, WildcardAdditionalOptions,
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

    fn new() -> Self;
    fn traverse(&mut self, ast: &mut Vec<Statement>) -> TraversalResult;
    fn traverse_statement(&mut self, statement: &mut Statement) -> TraversalResult;
    fn traverse_drop(&mut self, drop: &mut DropStatementViewMutable) -> TraversalResult;
    fn traverse_update(&mut self, update: &mut UpdateStatementViewMutable) -> TraversalResult;
    fn traverse_assignment(&mut self, assignment: &mut Assignment) -> TraversalResult;
    fn traverse_create_table(
        &mut self,
        create_table: &mut CreateTableStatementViewMutable,
    ) -> TraversalResult;
    fn traverse_create_index(
        &mut self,
        create_index: &mut CreateIndexStatementViewMutable,
    ) -> TraversalResult;
    fn traverse_insert(&mut self, insert: &mut InsertStatementViewMutable) -> TraversalResult;
    fn traverse_on_insert(&mut self, on_insert: &mut Option<OnInsert>) -> TraversalResult;
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
