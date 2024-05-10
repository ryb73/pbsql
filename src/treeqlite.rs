use super::translate_sql;
use super::TranslatedQuery;
use std::collections::HashMap;
use typed_path::Utf8UnixPathBuf;

struct Config {
    root_path: Utf8UnixPathBuf,
}

pub struct TreeQLiteExecutor {
    config: Config,
}

impl TreeQLiteExecutor {
    pub const fn new(config: Config) -> Self {
        Self { config }
    }

    fn convert_db_path_to_fs_path(&self, path: &str) -> Result<String, String> {
        if !path.starts_with("~/") {
            return Err(format!("Path must start with ~/: {}", path));
        }

        let relative_path = path.replacen("~/", "./", 1);

        Ok(self
            .config
            .root_path
            .join(relative_path)
            .normalize()
            .to_string())
    }

    pub fn execute_query(&self, sql: &str) -> Result<HashMap<String, String>, String> {
        let TranslatedQuery {
            databases,
            query: _translated_query,
        } = translate_sql(sql)?;

        let db_paths_by_reference = databases
            .iter()
            .map(|(k, v)| (v.to_owned(), k.to_owned()))
            .collect::<HashMap<_, _>>();

        let fs_paths_by_reference = db_paths_by_reference
            .into_iter()
            .map(|(k, v)| Ok((k, self.convert_db_path_to_fs_path(&v)?)))
            .collect::<Result<HashMap<_, _>, String>>()?;

        let _sqlite_connection = if let Some(main_path) = fs_paths_by_reference.get("main") {
            rusqlite::Connection::open(main_path)
        } else {
            rusqlite::Connection::open_in_memory()
        }
        .map_err(|e| format!("Error opening SQLite connection: {}", e))?;

        Ok(fs_paths_by_reference)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod convert_db_path_to_fs_path {
        use super::*;

        #[test]
        fn simple() {
            let executor = TreeQLiteExecutor::new(Config {
                root_path: Utf8UnixPathBuf::from("/home/user"),
            });

            let fs_path = executor
                .convert_db_path_to_fs_path("~/books/things")
                .unwrap();

            assert_eq!(fs_path, "/home/user/books/things");
        }
    }

    mod execute_query {
        use super::*;
        use crate::common::split_by_line_and_trim_spaces;
        use std::collections::BTreeMap;

        #[derive(serde::Serialize)]
        struct Report {
            db_paths_by_reference: Result<BTreeMap<String, String>, String>,
            original_query: Vec<String>,
        }

        fn generate_report(sql: &str) -> Report {
            let executor = TreeQLiteExecutor::new(Config {
                root_path: Utf8UnixPathBuf::from("./test-db"),
            });

            let result = executor.execute_query(sql);

            let original_query = split_by_line_and_trim_spaces(sql);

            Report {
                db_paths_by_reference: result.map(|v| v.into_iter().collect()),
                original_query,
            }
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn unsupported_statement_type() {
            let sql = r#"close my_eyes;"#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn create_index() {
            let sql = r#"CREATE INDEX "scoreIndex" ON "~/books/eloScores" (score)"#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn create_index_compound_name() {
            let sql = r#"CREATE INDEX a.b ON "~/books/eloScores" (score)"#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn create_index_compound_table_name() {
            let sql = r#"CREATE INDEX a ON x.y (score)"#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn insert() {
            let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP)
        "#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn insert_from_select() {
            let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            SELECT "~/books/matches"."id" || '2', "loserId", "~/books/matches"."winnerId", "matchDate" FROM "~/books/matches"
        "#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn parser_error() {
            let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP('derp))
        "#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn select_null() {
            let sql = "SELECT null";

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn update() {
            let sql = r#"UPDATE "~/books/eloScores" SET "score" = ? WHERE "thingId" = ?"#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn unsupported_function() {
            let sql = "SELECT badfunc()";

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn select_wildcard() {
            let sql = r#"select * from "~/heyy""#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn drop_table() {
            let sql = r#"drop table "~/heyy""#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn drop_multiple_tables() {
            let sql = r#"drop table "~/heyy", "~/okokok""#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn on_conflict_do_nothing() {
            let sql = r#"
            insert into "~/my-data-scraper/reelgood/shows-and-movies"
            ("format", "isWatched", "name", "url", "imageUrl")
            values (?, ?, ?, ?, ?)
            on conflict do nothing
        "#;

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
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

            let report = generate_report(sql);
            insta::assert_yaml_snapshot!(report);
        }
    }
}
