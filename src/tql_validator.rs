use super::sql_ast_traversal::{
    ast_views::{
        CreateIndexStatementViewMutable, CreateTableStatementViewMutable, DropStatementViewMutable,
        InsertStatementViewMutable, TableFactorTableViewMut,
    },
    helpers::extract_unary_identifier,
    traverser::VisitResult,
};
use crate::sql_ast_traversal::traverser::SqlAstTraverser;
use sqlparser::ast::{
    self, Assignment, Function, FunctionArgExpr, HiveDistributionStyle, HiveFormat, JoinConstraint,
    ObjectName, ObjectType, OnConflict, Value,
};

pub struct TqlValidator;

impl TqlValidator {
    pub fn new() -> Self {
        Self
    }
}

impl SqlAstTraverser for TqlValidator {
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
            name,
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

        let ObjectName(name_identifiers) = name;
        extract_unary_identifier(name_identifiers, "table")?;

        Ok(())
    }

    fn pre_visit_create_index(
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

        let ObjectName(name_identifiers) = name.as_ref().unwrap();
        extract_unary_identifier(name_identifiers, "index")?;

        let ObjectName(table_name_identifiers) = table_name;
        extract_unary_identifier(table_name_identifiers, "table")?;

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

    fn pre_visit_table_factor_table(
        &mut self,
        relation: &mut TableFactorTableViewMut,
    ) -> VisitResult {
        let TableFactorTableViewMut {
            alias: _,
            args: _,
            name: _,
            partitions,
            version,
            with_hints,
        } = relation;

        if !partitions.is_empty() {
            return Err("not implemented: TableFactor::Table::partitions".to_string());
        }

        if version.is_some() {
            return Err("not implemented: TableFactor::Table::version".to_string());
        }

        if !with_hints.is_empty() {
            return Err("not implemented: TableFactor::Table::with_hints".to_string());
        }

        Ok(())
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
            ObjectType::Table | ObjectType::View | ObjectType::Index => {}
            ObjectType::Schema => Err("not implemented: Statement::Drop::Schema".to_string())?,
            ObjectType::Role => Err("not implemented: Statement::Drop::Role".to_string())?,
            ObjectType::Sequence => Err("not implemented: Statement::Drop::Sequence".to_string())?,
            ObjectType::Stage => Err("not implemented: Statement::Drop::Stage".to_string())?,
        }

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
            action,
            conflict_target,
        } = on_conflict;

        match action {
            ast::OnConflictAction::DoNothing => {}
            ast::OnConflictAction::DoUpdate(_) => {
                Err("not implemented: OnConflictAction::DoUpdate".to_string())?
            }
        }

        if conflict_target.is_some() {
            return Err("not implemented: OnConflict::conflict_target".to_string());
        }

        Ok(())
    }

    fn pre_visit_join_constraint(&mut self, constraint: &mut JoinConstraint) -> VisitResult {
        match constraint {
            JoinConstraint::On(_) | JoinConstraint::None | JoinConstraint::Natural => {}
            JoinConstraint::Using(_) => Err("not implemented: JoinConstraint::Using".to_string())?,
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

    fn pre_visit_function(&mut self, func: &mut Function) -> VisitResult {
        validate_function_name(&func.name)?;
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

#[cfg(test)]
mod tests {
    use sqlparser::{dialect::SQLiteDialect, parser::Parser};

    use crate::common::split_by_line_and_trim_spaces;

    use super::*;

    fn validate_sql(query: &str) -> Result<(), String> {
        let dialect = SQLiteDialect {};

        let mut ast = Parser::parse_sql(&dialect, query).map_err(|e| e.to_string())?;

        let mut validator = TqlValidator::new();

        validator.traverse(&mut ast)?;

        Ok(())
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn unsupported_statement_type() {
        let sql = r#"close my_eyes;"#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn create_index() {
        let sql = r#"CREATE INDEX "scoreIndex" ON "~/books/eloScores" (score)"#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn create_index_compound_name() {
        let sql = r#"CREATE INDEX a.b ON "~/books/eloScores" (score)"#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn create_index_compound_table_name() {
        let sql = r#"CREATE INDEX a ON x.y (score)"#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn insert() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP)
        "#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn insert_from_select() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            SELECT "~/books/matches"."id" || '2', "loserId", "~/books/matches"."winnerId", "matchDate" FROM "~/books/matches"
        "#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn parser_error() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP('derp))
        "#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn select_null() {
        let sql = "SELECT null";

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn update() {
        let sql = r#"UPDATE "~/books/eloScores" SET "score" = ? WHERE "thingId" = ?"#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn unsupported_function() {
        let sql = "SELECT badfunc()";

        let result = validate_sql(sql);
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn select_wildcard() {
        let sql = r#"select * from "~/heyy""#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn drop_table() {
        let sql = r#"drop table "~/heyy""#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn drop_multiple_tables() {
        let sql = r#"drop table "~/heyy", "~/okokok""#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn on_conflict_do_nothing() {
        let sql = r#"
            insert into "~/my-data-scraper/reelgood/shows-and-movies"
            ("format", "isWatched", "name", "url", "imageUrl")
            values (?, ?, ?, ?, ?)
            on conflict do nothing
        "#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
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

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    #[test]
    fn unknown_table_reference() {
        let sql = r#"
            select "~/books/things".id, "~/books/things".title
            from "~/books/non-things" as "books"
        "#;

        let translate_result = validate_sql(sql);
        insta::assert_yaml_snapshot!((translate_result, split_by_line_and_trim_spaces(sql)));
    }

    mod table_factor {
        use super::*;
        mod table {
            use super::*;

            #[test]
            fn duplicate_table() {
                let sql = r#"
                    select "~/books/things".id, "~/books/things".title
                    from "~/books/things"
                    join "~/books/things"
                "#;

                let translate_result = validate_sql(sql);
                insta::assert_yaml_snapshot!((
                    translate_result,
                    split_by_line_and_trim_spaces(sql)
                ));
            }

            #[test]
            fn duplicate_alias() {
                let sql = r#"
                    select "~/books/things".id, "~/books/things".title
                    from "~/books/things"
                    join "~/books/non-things" as "~/books/things"
                "#;

                let translate_result = validate_sql(sql);
                insta::assert_yaml_snapshot!((
                    translate_result,
                    split_by_line_and_trim_spaces(sql)
                ));
            }
        }
    }
}
