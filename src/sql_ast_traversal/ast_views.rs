use sqlparser::ast::{
    Assignment, ColumnDef, Expr, FileFormat, HiveDistributionStyle, HiveFormat, Ident,
    MysqlInsertPriority, ObjectName, ObjectType, OnCommit, OnInsert, OrderByExpr, Query,
    SelectItem, SetExpr, SetOperator, SetQuantifier, SqlOption, SqliteOnConflict, Statement,
    TableAlias, TableConstraint, TableFactor, TableWithJoins,
};

#[derive(Debug)]
pub struct DropStatementViewMutable<'a> {
    pub cascade: &'a mut bool,
    pub if_exists: &'a mut bool,
    pub names: &'a mut Vec<ObjectName>,
    pub object_type: &'a mut ObjectType,
    pub purge: &'a mut bool,
    pub restrict: &'a mut bool,
    pub temporary: &'a mut bool,
}

impl<'a> TryFrom<&'a mut Statement> for DropStatementViewMutable<'a> {
    type Error = String;

    fn try_from(statement: &'a mut Statement) -> Result<Self, Self::Error> {
        match statement {
            Statement::Drop {
                cascade,
                if_exists,
                names,
                object_type,
                purge,
                restrict,
                temporary,
            } => Ok(DropStatementViewMutable {
                cascade,
                if_exists,
                names,
                object_type,
                purge,
                restrict,
                temporary,
            }),
            _ => Err("Expected a Drop statement".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct UpdateStatementViewMutable<'a> {
    pub assignments: &'a mut Vec<Assignment>,
    pub from: &'a mut Option<TableWithJoins>,
    pub returning: &'a mut Option<Vec<SelectItem>>,
    pub selection: &'a mut Option<Expr>,
    pub table: &'a mut TableWithJoins,
}

impl<'a> TryFrom<&'a mut Statement> for UpdateStatementViewMutable<'a> {
    type Error = String;

    fn try_from(statement: &'a mut Statement) -> Result<Self, Self::Error> {
        match statement {
            Statement::Update {
                assignments,
                from,
                returning,
                selection,
                table,
            } => Ok(UpdateStatementViewMutable {
                assignments,
                from,
                returning,
                selection,
                table,
            }),
            _ => Err("Expected an Update statement".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct CreateIndexStatementViewMutable<'a> {
    pub columns: &'a mut Vec<OrderByExpr>,
    pub name: &'a mut Option<ObjectName>,
    pub table_name: &'a mut ObjectName,
    pub unique: &'a mut bool,
    pub if_not_exists: &'a mut bool,
    pub nulls_distinct: &'a mut Option<bool>,
    pub concurrently: &'a mut bool,
    pub include: &'a mut Vec<Ident>,
    pub predicate: &'a mut Option<Expr>,
    pub using: &'a mut Option<Ident>,
}

impl<'a> TryFrom<&'a mut Statement> for CreateIndexStatementViewMutable<'a> {
    type Error = String;

    fn try_from(statement: &'a mut Statement) -> Result<Self, Self::Error> {
        match statement {
            Statement::CreateIndex {
                columns,
                name,
                table_name,
                unique,
                if_not_exists,
                nulls_distinct,
                concurrently,
                include,
                predicate,
                using,
            } => Ok(CreateIndexStatementViewMutable {
                columns,
                name,
                table_name,
                unique,
                if_not_exists,
                nulls_distinct,
                concurrently,
                include,
                predicate,
                using,
            }),
            _ => Err("Expected a CreateIndex statement".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct CreateTableStatementViewMutable<'a> {
    pub if_not_exists: &'a mut bool,
    pub name: &'a mut ObjectName,
    pub columns: &'a mut Vec<ColumnDef>,
    pub auto_increment_offset: &'a mut Option<u32>,
    pub clone: &'a mut Option<ObjectName>,
    pub cluster_by: &'a mut Option<Vec<Ident>>,
    pub collation: &'a mut Option<String>,
    pub comment: &'a mut Option<String>,
    pub constraints: &'a mut Vec<TableConstraint>,
    pub default_charset: &'a mut Option<String>,
    pub engine: &'a mut Option<String>,
    pub external: &'a mut bool,
    pub file_format: &'a mut Option<FileFormat>,
    pub global: &'a mut Option<bool>,
    pub hive_distribution: &'a mut HiveDistributionStyle,
    pub hive_formats: &'a mut Option<HiveFormat>,
    pub like: &'a mut Option<ObjectName>,
    pub location: &'a mut Option<String>,
    pub on_cluster: &'a mut Option<String>,
    pub on_commit: &'a mut Option<OnCommit>,
    pub options: &'a mut Option<Vec<SqlOption>>,
    pub or_replace: &'a mut bool,
    pub order_by: &'a mut Option<Vec<Ident>>,
    pub partition_by: &'a mut Option<Box<Expr>>,
    pub query: &'a mut Option<Box<Query>>,
    pub strict: &'a mut bool,
    pub table_properties: &'a mut Vec<SqlOption>,
    pub temporary: &'a mut bool,
    pub transient: &'a mut bool,
    pub with_options: &'a mut Vec<SqlOption>,
    pub without_rowid: &'a mut bool,
}

impl<'a> TryFrom<&'a mut Statement> for CreateTableStatementViewMutable<'a> {
    type Error = String;

    fn try_from(statement: &'a mut Statement) -> Result<Self, Self::Error> {
        match statement {
            Statement::CreateTable {
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
            } => Ok(CreateTableStatementViewMutable {
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
            }),
            _ => Err("Expected a CreateTable statement".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct InsertStatementViewMutable<'a> {
    pub after_columns: &'a mut Vec<Ident>,
    pub columns: &'a mut Vec<Ident>,
    pub ignore: &'a mut bool,
    pub into: &'a mut bool,
    pub on: &'a mut Option<OnInsert>,
    pub or: &'a mut Option<SqliteOnConflict>,
    pub overwrite: &'a mut bool,
    pub partitioned: &'a mut Option<Vec<Expr>>,
    pub priority: &'a mut Option<MysqlInsertPriority>,
    pub replace_into: &'a mut bool,
    pub returning: &'a mut Option<Vec<SelectItem>>,
    pub source: &'a mut Option<Box<Query>>,
    pub table_alias: &'a mut Option<Ident>,
    pub table_name: &'a mut ObjectName,
    pub table: &'a mut bool,
}

impl<'a> TryFrom<&'a mut Statement> for InsertStatementViewMutable<'a> {
    type Error = String;

    fn try_from(statement: &'a mut Statement) -> Result<Self, Self::Error> {
        match statement {
            Statement::Insert {
                after_columns,
                columns,
                ignore,
                into,
                on,
                or,
                overwrite,
                partitioned,
                priority,
                replace_into,
                returning,
                source,
                table_alias,
                table_name,
                table,
            } => Ok(InsertStatementViewMutable {
                after_columns,
                columns,
                ignore,
                into,
                on,
                or,
                overwrite,
                partitioned,
                priority,
                replace_into,
                returning,
                source,
                table_alias,
                table_name,
                table,
            }),
            _ => Err("Expected an Insert statement".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct SetOperationViewMutable<'a> {
    pub left: &'a mut Box<SetExpr>,
    pub op: &'a mut SetOperator,
    pub right: &'a mut Box<SetExpr>,
    pub set_quantifier: &'a mut SetQuantifier,
}

impl<'a> TryFrom<&'a mut SetExpr> for SetOperationViewMutable<'a> {
    type Error = String;

    fn try_from(set_expr: &'a mut SetExpr) -> Result<Self, Self::Error> {
        match set_expr {
            SetExpr::SetOperation {
                left,
                op,
                right,
                set_quantifier,
            } => Ok(SetOperationViewMutable {
                left,
                op,
                right,
                set_quantifier,
            }),
            _ => Err("Expected a SetOperation".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct InSubqueryExprViewMutable<'a> {
    pub expr: &'a mut Box<Expr>,
    pub negated: &'a mut bool,
    pub subquery: &'a mut Box<Query>,
}

impl<'a> TryFrom<&'a mut Expr> for InSubqueryExprViewMutable<'a> {
    type Error = String;

    fn try_from(expr: &'a mut Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::InSubquery {
                expr,
                negated,
                subquery,
            } => Ok(InSubqueryExprViewMutable {
                expr,
                negated,
                subquery,
            }),
            _ => Err("Expected an InSubquery expression".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct DerivedTableFactorViewMutable<'a> {
    pub alias: &'a mut Option<TableAlias>,
    pub lateral: &'a mut bool,
    pub subquery: &'a mut Box<Query>,
}

impl<'a> TryFrom<&'a mut TableFactor> for DerivedTableFactorViewMutable<'a> {
    type Error = String;

    fn try_from(table_factor: &'a mut TableFactor) -> Result<Self, Self::Error> {
        match table_factor {
            TableFactor::Derived {
                alias,
                lateral,
                subquery,
            } => Ok(DerivedTableFactorViewMutable {
                alias,
                lateral,
                subquery,
            }),
            _ => Err("Expected a DerivedTableFactor".to_string()),
        }
    }
}
