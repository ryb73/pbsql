use sqlparser::ast::{
    Assignment, ColumnDef, Expr, FileFormat, FunctionArg, HiveDistributionStyle, HiveFormat, Ident,
    MysqlInsertPriority, ObjectName, ObjectType, OnCommit, OnInsert, OrderByExpr, Query,
    SelectItem, SetExpr, SetOperator, SetQuantifier, SqlOption, SqliteOnConflict, Statement,
    TableAlias, TableConstraint, TableFactor, TableVersion, TableWithJoins,
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
pub struct TableFactorDerivedViewMut<'a> {
    pub alias: &'a mut Option<TableAlias>,
    pub lateral: &'a mut bool,
    pub subquery: &'a mut Box<Query>,
}

impl<'a> TryFrom<&'a mut TableFactor> for TableFactorDerivedViewMut<'a> {
    type Error = String;

    fn try_from(table_factor: &'a mut TableFactor) -> Result<Self, Self::Error> {
        match table_factor {
            TableFactor::Derived {
                alias,
                lateral,
                subquery,
            } => Ok(TableFactorDerivedViewMut {
                alias,
                lateral,
                subquery,
            }),
            _ => Err("Expected a DerivedTableFactor".to_string()),
        }
    }
}

#[derive(Debug)]
pub struct TableFactorTableViewMut<'a> {
    pub alias: &'a mut Option<TableAlias>,
    pub args: &'a mut Option<Vec<FunctionArg>>,
    pub name: &'a mut ObjectName,
    pub partitions: &'a mut Vec<Ident>,
    pub version: &'a mut Option<TableVersion>,
    pub with_hints: &'a mut Vec<Expr>,
}

impl<'a> TryFrom<&'a mut TableFactor> for TableFactorTableViewMut<'a> {
    type Error = String;

    fn try_from(table_factor: &'a mut TableFactor) -> Result<Self, Self::Error> {
        match table_factor {
            TableFactor::Table {
                alias,
                args,
                name,
                partitions,
                version,
                with_hints,
            } => Ok(TableFactorTableViewMut {
                alias,
                args,
                name,
                partitions,
                version,
                with_hints,
            }),
            _ => Err("Expected a TableFactor".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod drop {
        use super::*;

        mod try_from {
            use sqlparser::ast::CloseCursor;

            use super::*;

            #[test]
            fn good() {
                let mut statement = Statement::Drop {
                    cascade: true,
                    if_exists: true,
                    names: vec![ObjectName(vec![Ident::new("table")])],
                    object_type: ObjectType::Table,
                    purge: true,
                    restrict: true,
                    temporary: true,
                };

                let view = DropStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    DropStatementViewMutable {
                        cascade: true,
                        if_exists: true,
                        names: [
                            ObjectName(
                                [
                                    Ident {
                                        value: "table",
                                        quote_style: None,
                                    },
                                ],
                            ),
                        ],
                        object_type: Table,
                        purge: true,
                        restrict: true,
                        temporary: true,
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut statement = Statement::Close {
                    cursor: CloseCursor::All,
                };

                let view = DropStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected a Drop statement",
                )
                "###);
            }
        }
    }

    mod update {
        use super::*;

        mod try_from {
            use sqlparser::ast::{CloseCursor, Value};

            use super::*;

            #[test]
            fn good() {
                let mut statement = Statement::Update {
                    assignments: vec![Assignment {
                        id: vec![Ident::new("id")],
                        value: Expr::Value(Value::SingleQuotedString("1".to_string())),
                    }],
                    from: None,
                    returning: None,
                    selection: None,
                    table: TableWithJoins {
                        joins: vec![],
                        relation: TableFactor::Table {
                            alias: None,
                            args: None,
                            name: ObjectName(vec![Ident::new("table")]),
                            partitions: vec![],
                            version: None,
                            with_hints: vec![],
                        },
                    },
                };

                let view = UpdateStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    UpdateStatementViewMutable {
                        assignments: [
                            Assignment {
                                id: [
                                    Ident {
                                        value: "id",
                                        quote_style: None,
                                    },
                                ],
                                value: Value(
                                    SingleQuotedString(
                                        "1",
                                    ),
                                ),
                            },
                        ],
                        from: None,
                        returning: None,
                        selection: None,
                        table: TableWithJoins {
                            relation: Table {
                                name: ObjectName(
                                    [
                                        Ident {
                                            value: "table",
                                            quote_style: None,
                                        },
                                    ],
                                ),
                                alias: None,
                                args: None,
                                with_hints: [],
                                version: None,
                                partitions: [],
                            },
                            joins: [],
                        },
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut statement = Statement::Close {
                    cursor: CloseCursor::All,
                };

                let view = UpdateStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected an Update statement",
                )
                "###);
            }
        }
    }

    mod create_index {
        use super::*;

        mod try_from {
            use sqlparser::ast::{CloseCursor, Value};

            use super::*;

            #[test]
            fn good() {
                let mut statement = Statement::CreateIndex {
                    columns: vec![OrderByExpr {
                        expr: Expr::Value(Value::SingleQuotedString("id".to_string())),
                        asc: None,
                        nulls_first: None,
                    }],
                    name: Some(ObjectName(vec![Ident::new("index")])),
                    table_name: ObjectName(vec![Ident::new("table")]),
                    unique: true,
                    if_not_exists: true,
                    nulls_distinct: None,
                    concurrently: true,
                    include: vec![Ident::new("id")],
                    predicate: None,
                    using: None,
                };

                let view = CreateIndexStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    CreateIndexStatementViewMutable {
                        columns: [
                            OrderByExpr {
                                expr: Value(
                                    SingleQuotedString(
                                        "id",
                                    ),
                                ),
                                asc: None,
                                nulls_first: None,
                            },
                        ],
                        name: Some(
                            ObjectName(
                                [
                                    Ident {
                                        value: "index",
                                        quote_style: None,
                                    },
                                ],
                            ),
                        ),
                        table_name: ObjectName(
                            [
                                Ident {
                                    value: "table",
                                    quote_style: None,
                                },
                            ],
                        ),
                        unique: true,
                        if_not_exists: true,
                        nulls_distinct: None,
                        concurrently: true,
                        include: [
                            Ident {
                                value: "id",
                                quote_style: None,
                            },
                        ],
                        predicate: None,
                        using: None,
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut statement = Statement::Close {
                    cursor: CloseCursor::All,
                };

                let view = CreateIndexStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected a CreateIndex statement",
                )
                "###);
            }
        }
    }

    mod create_table {
        use super::*;

        mod try_from {
            use sqlparser::ast::CloseCursor;

            use super::*;

            #[test]
            fn good() {
                let mut statement = Statement::CreateTable {
                    if_not_exists: true,
                    name: ObjectName(vec![Ident::new("table")]),
                    columns: vec![ColumnDef {
                        name: Ident::new("id"),
                        data_type: sqlparser::ast::DataType::Int(None),
                        collation: None,
                        options: vec![],
                    }],
                    auto_increment_offset: None,
                    clone: None,
                    cluster_by: None,
                    collation: None,
                    comment: None,
                    constraints: vec![],
                    default_charset: None,
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
                    or_replace: false,
                    order_by: None,
                    partition_by: None,
                    query: None,
                    strict: false,
                    table_properties: vec![],
                    temporary: false,
                    transient: false,
                    with_options: vec![],
                    without_rowid: false,
                };

                let view = CreateTableStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    CreateTableStatementViewMutable {
                        if_not_exists: true,
                        name: ObjectName(
                            [
                                Ident {
                                    value: "table",
                                    quote_style: None,
                                },
                            ],
                        ),
                        columns: [
                            ColumnDef {
                                name: Ident {
                                    value: "id",
                                    quote_style: None,
                                },
                                data_type: Int(
                                    None,
                                ),
                                collation: None,
                                options: [],
                            },
                        ],
                        auto_increment_offset: None,
                        clone: None,
                        cluster_by: None,
                        collation: None,
                        comment: None,
                        constraints: [],
                        default_charset: None,
                        engine: None,
                        external: false,
                        file_format: None,
                        global: None,
                        hive_distribution: NONE,
                        hive_formats: None,
                        like: None,
                        location: None,
                        on_cluster: None,
                        on_commit: None,
                        options: None,
                        or_replace: false,
                        order_by: None,
                        partition_by: None,
                        query: None,
                        strict: false,
                        table_properties: [],
                        temporary: false,
                        transient: false,
                        with_options: [],
                        without_rowid: false,
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut statement = Statement::Close {
                    cursor: CloseCursor::All,
                };

                let view = CreateTableStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected a CreateTable statement",
                )
                "###);
            }
        }
    }

    mod insert {
        use super::*;

        mod try_from {
            use sqlparser::ast::CloseCursor;

            use super::*;

            #[test]
            fn good() {
                let mut statement = Statement::Insert {
                    after_columns: vec![Ident::new("id")],
                    columns: vec![Ident::new("id")],
                    ignore: false,
                    into: true,
                    on: None,
                    or: None,
                    overwrite: false,
                    partitioned: None,
                    priority: None,
                    replace_into: false,
                    returning: None,
                    source: None,
                    table_alias: None,
                    table_name: ObjectName(vec![Ident::new("table")]),
                    table: false,
                };

                let view = InsertStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    InsertStatementViewMutable {
                        after_columns: [
                            Ident {
                                value: "id",
                                quote_style: None,
                            },
                        ],
                        columns: [
                            Ident {
                                value: "id",
                                quote_style: None,
                            },
                        ],
                        ignore: false,
                        into: true,
                        on: None,
                        or: None,
                        overwrite: false,
                        partitioned: None,
                        priority: None,
                        replace_into: false,
                        returning: None,
                        source: None,
                        table_alias: None,
                        table_name: ObjectName(
                            [
                                Ident {
                                    value: "table",
                                    quote_style: None,
                                },
                            ],
                        ),
                        table: false,
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut statement = Statement::Close {
                    cursor: CloseCursor::All,
                };

                let view = InsertStatementViewMutable::try_from(&mut statement);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected an Insert statement",
                )
                "###);
            }
        }
    }

    mod set_operation {
        use super::*;

        mod try_from {
            use sqlparser::ast::{GroupByExpr, Select, Values};

            use super::*;

            #[test]
            fn good() {
                let mut set_expr = SetExpr::SetOperation {
                    left: Box::new(SetExpr::Select(Box::new(Select {
                        distinct: None,
                        top: None,
                        projection: vec![],
                        into: None,
                        from: vec![],
                        lateral_views: vec![],
                        selection: None,
                        group_by: GroupByExpr::Expressions(vec![]),
                        cluster_by: vec![],
                        distribute_by: vec![],
                        sort_by: vec![],
                        having: None,
                        named_window: vec![],
                        qualify: None,
                    }))),
                    op: SetOperator::Union,
                    right: Box::new(SetExpr::Select(Box::new(Select {
                        distinct: None,
                        top: None,
                        projection: vec![],
                        into: None,
                        from: vec![],
                        lateral_views: vec![],
                        selection: None,
                        group_by: GroupByExpr::Expressions(vec![]),
                        cluster_by: vec![],
                        distribute_by: vec![],
                        sort_by: vec![],
                        having: None,
                        named_window: vec![],
                        qualify: None,
                    }))),
                    set_quantifier: SetQuantifier::Distinct,
                };

                let view = SetOperationViewMutable::try_from(&mut set_expr);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    SetOperationViewMutable {
                        left: Select(
                            Select {
                                distinct: None,
                                top: None,
                                projection: [],
                                into: None,
                                from: [],
                                lateral_views: [],
                                selection: None,
                                group_by: Expressions(
                                    [],
                                ),
                                cluster_by: [],
                                distribute_by: [],
                                sort_by: [],
                                having: None,
                                named_window: [],
                                qualify: None,
                            },
                        ),
                        op: Union,
                        right: Select(
                            Select {
                                distinct: None,
                                top: None,
                                projection: [],
                                into: None,
                                from: [],
                                lateral_views: [],
                                selection: None,
                                group_by: Expressions(
                                    [],
                                ),
                                cluster_by: [],
                                distribute_by: [],
                                sort_by: [],
                                having: None,
                                named_window: [],
                                qualify: None,
                            },
                        ),
                        set_quantifier: Distinct,
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut set_expr = SetExpr::Values(Values {
                    explicit_row: false,
                    rows: vec![],
                });

                let view = SetOperationViewMutable::try_from(&mut set_expr);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected a SetOperation",
                )
                "###);
            }
        }
    }

    mod in_subquery_expr {
        use super::*;

        mod try_from {
            use super::*;

            #[test]
            fn good() {
                let mut expr = Expr::InSubquery {
                    expr: Box::new(Expr::Value(sqlparser::ast::Value::SingleQuotedString(
                        "id".to_string(),
                    ))),
                    negated: false,
                    subquery: Box::new(Query {
                        body: Box::new(SetExpr::Values(sqlparser::ast::Values {
                            explicit_row: false,
                            rows: vec![],
                        })),
                        fetch: None,
                        for_clause: None,
                        limit_by: vec![],
                        limit: None,
                        locks: vec![],
                        offset: None,
                        order_by: vec![],
                        with: None,
                    }),
                };

                let view = InSubqueryExprViewMutable::try_from(&mut expr);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    InSubqueryExprViewMutable {
                        expr: Value(
                            SingleQuotedString(
                                "id",
                            ),
                        ),
                        negated: false,
                        subquery: Query {
                            with: None,
                            body: Values(
                                Values {
                                    explicit_row: false,
                                    rows: [],
                                },
                            ),
                            order_by: [],
                            limit: None,
                            limit_by: [],
                            offset: None,
                            fetch: None,
                            locks: [],
                            for_clause: None,
                        },
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut expr =
                    Expr::Value(sqlparser::ast::Value::SingleQuotedString("id".to_string()));

                let view = InSubqueryExprViewMutable::try_from(&mut expr);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected an InSubquery expression",
                )
                "###);
            }
        }
    }

    mod table_factor_derived {
        use super::*;

        mod try_from {
            use super::*;

            #[test]
            fn good() {
                let mut table_factor = TableFactor::Derived {
                    alias: None,
                    lateral: false,
                    subquery: Box::new(Query {
                        body: Box::new(SetExpr::Values(sqlparser::ast::Values {
                            explicit_row: false,
                            rows: vec![],
                        })),
                        fetch: None,
                        for_clause: None,
                        limit_by: vec![],
                        limit: None,
                        locks: vec![],
                        offset: None,
                        order_by: vec![],
                        with: None,
                    }),
                };

                let view = TableFactorDerivedViewMut::try_from(&mut table_factor);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    TableFactorDerivedViewMut {
                        alias: None,
                        lateral: false,
                        subquery: Query {
                            with: None,
                            body: Values(
                                Values {
                                    explicit_row: false,
                                    rows: [],
                                },
                            ),
                            order_by: [],
                            limit: None,
                            limit_by: [],
                            offset: None,
                            fetch: None,
                            locks: [],
                            for_clause: None,
                        },
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut table_factor = TableFactor::Table {
                    alias: None,
                    args: None,
                    name: ObjectName(vec![Ident::new("table")]),
                    partitions: vec![],
                    version: None,
                    with_hints: vec![],
                };

                let view = TableFactorDerivedViewMut::try_from(&mut table_factor);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected a DerivedTableFactor",
                )
                "###);
            }
        }
    }

    mod table_factor_table {
        use super::*;

        mod try_from {
            use super::*;

            #[test]
            fn good() {
                let mut table_factor = TableFactor::Table {
                    alias: None,
                    args: None,
                    name: ObjectName(vec![Ident::new("table")]),
                    partitions: vec![],
                    version: None,
                    with_hints: vec![],
                };

                let view = TableFactorTableViewMut::try_from(&mut table_factor);
                insta::assert_debug_snapshot!(view, @r###"
                Ok(
                    TableFactorTableViewMut {
                        alias: None,
                        args: None,
                        name: ObjectName(
                            [
                                Ident {
                                    value: "table",
                                    quote_style: None,
                                },
                            ],
                        ),
                        partitions: [],
                        version: None,
                        with_hints: [],
                    },
                )
                "###);
            }

            #[test]
            fn bad() {
                let mut table_factor = TableFactor::Derived {
                    alias: None,
                    lateral: false,
                    subquery: Box::new(Query {
                        body: Box::new(SetExpr::Values(sqlparser::ast::Values {
                            explicit_row: false,
                            rows: vec![],
                        })),
                        fetch: None,
                        for_clause: None,
                        limit_by: vec![],
                        limit: None,
                        locks: vec![],
                        offset: None,
                        order_by: vec![],
                        with: None,
                    }),
                };

                let view = TableFactorTableViewMut::try_from(&mut table_factor);
                insta::assert_debug_snapshot!(view, @r###"
                Err(
                    "Expected a TableFactor",
                )
                "###);
            }
        }
    }
}
