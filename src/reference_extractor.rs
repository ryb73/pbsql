use super::sql_ast_traversal::{
    ast_views::{
        CreateIndexStatementViewMutable, CreateTableStatementViewMutable, DropStatementViewMutable,
        InsertStatementViewMutable, TableFactorTableViewMut,
    },
    helpers::extract_unary_identifier,
    traverser::VisitResult,
};
use crate::sql_ast_traversal::traverser::SqlAstTraverser;
use sqlparser::ast::{Assignment, ObjectName, ObjectType};
use std::collections::HashSet;

#[derive(serde::Serialize, serde::Deserialize, Debug)]
pub struct ReferenceExtractor {
    pub indices: HashSet<ObjectName>,
    pub relations: HashSet<ObjectName>,
}

impl ReferenceExtractor {
    pub fn _new() -> Self {
        ReferenceExtractor {
            indices: HashSet::new(),
            relations: HashSet::new(),
        }
    }
}

impl SqlAstTraverser for ReferenceExtractor {
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

        for name in names.iter() {
            self.relations.insert(name.clone());
        }

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
        self.relations.insert(create_table.name.clone());

        Ok(())
    }

    fn post_visit_create_index(
        &mut self,
        create_index: &mut CreateIndexStatementViewMutable,
    ) -> VisitResult {
        if let Some(name) = &create_index.name {
            self.indices.insert(name.clone());
        }

        self.relations.insert(create_index.table_name.clone());

        Ok(())
    }

    fn post_visit_insert(&mut self, insert: &mut InsertStatementViewMutable) -> VisitResult {
        self.relations.insert(insert.table_name.clone());

        Ok(())
    }

    fn post_visit_table_factor_table(
        &mut self,
        relation: &mut TableFactorTableViewMut,
    ) -> VisitResult {
        let TableFactorTableViewMut {
            alias: _,
            args: _,
            name: table_name,
            partitions: _,
            version: _,
            with_hints: _,
        } = relation;

        self.relations.insert(table_name.clone());

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use sqlparser::{dialect::SQLiteDialect, parser::Parser};
    use std::collections::BTreeSet;

    use super::*;

    #[derive(serde::Serialize, serde::Deserialize, Debug)]
    struct Report {
        indices: BTreeSet<ObjectName>,
        relations: BTreeSet<ObjectName>,
        original_sql: Vec<String>,
    }

    fn translate_sql(query: &str) -> Result<Report, String> {
        let dialect = SQLiteDialect {};

        let mut ast = Parser::parse_sql(&dialect, query).map_err(|e| e.to_string())?;

        let mut reference_extractor = ReferenceExtractor::_new();

        reference_extractor.traverse(&mut ast)?;

        Ok(Report {
            indices: reference_extractor.indices.iter().cloned().collect(),
            relations: reference_extractor.relations.iter().cloned().collect(),
            original_sql: query.split('\n').map(|s| s.to_string()).collect(),
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
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
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn unsupported_statement_type() {
        let sql = r#"close my_eyes;"#;

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_index() {
        let sql = r#"CREATE INDEX "scoreIndex" ON "~/books/eloScores" (score)"#;

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_index_compound_name() {
        let sql = r#"CREATE INDEX a.b ON "~/books/eloScores" (score)"#;

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn create_index_compound_table_name() {
        let sql = r#"CREATE INDEX a ON x.y (score)"#;

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn insert() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP)
        "#;

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn insert_from_select() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            SELECT "~/books/matches"."id" || '2', "loserId", "~/books/matches"."winnerId", "matchDate" FROM "~/books/matches"
        "#;

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn parser_error() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP('derp))
        "#;

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn select_null() {
        let sql = "SELECT null";

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn update() {
        let sql = r#"UPDATE "~/books/eloScores" SET "score" = ? WHERE "thingId" = ?"#;

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
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
        insta::assert_yaml_snapshot!(translate_result);
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
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn select_wildcard() {
        let sql = r#"select * from "~/heyy""#;

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn drop_table() {
        let sql = r#"drop table "~/heyy""#;

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }

    #[test]
    fn drop_multiple_tables() {
        let sql = r#"drop table "~/heyy", "~/okokok""#;

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
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

        let translate_result = translate_sql(sql);
        insta::assert_yaml_snapshot!(translate_result);
    }
}
