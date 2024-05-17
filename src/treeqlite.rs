use super::translate_sql;
use super::TranslatedQuery;
use rusqlite::{OpenFlags, Params};
use serde::Serialize;
use std::{collections::HashMap, fmt::Debug, fs::create_dir_all, path::PathBuf};
use typed_path::Utf8NativePathBuf;

pub struct Config {
    root_path: Utf8NativePathBuf,
}

pub struct TreeQLiteExecutor {
    config: Config,
}

/// "Unqualified" means that the filename doesn't have an extension (e.g. .tql.sqlite3)
#[derive(Debug, Serialize)]
pub struct UnqualifiedPath(String);

impl From<String> for UnqualifiedPath {
    fn from(s: String) -> Self {
        Self(s)
    }
}

fn resolve_path(path: &UnqualifiedPath) -> Result<String, String> {
    let mut new_path = PathBuf::from(path.0.clone());

    let file_name = new_path
        .file_name()
        .ok_or(format!("Path has no filename: {}", path.0))?
        .to_str()
        .ok_or("Error converting input path to string")?;

    new_path.set_file_name(file_name.to_owned() + ".tql.sqlite3");

    let result = new_path
        .to_str()
        .ok_or("Error converting output path to string")?
        .to_owned();

    if let Some(parent) = new_path.parent() {
        create_dir_all(parent).map_err(|e| format!("Error creating parent directories: {}", e))?;
    }

    Ok(result)
}

impl TreeQLiteExecutor {
    pub const fn new(config: Config) -> Self {
        Self { config }
    }

    /// "Unqualified" means that the filename doesn't have an extension (e.g. .tql.sqlite3)
    fn convert_db_path_to_unqualified_fs_path(
        &self,
        path: &str,
    ) -> Result<UnqualifiedPath, String> {
        if !path.starts_with("~/") {
            return Err(format!("Path must start with ~/: {}", path));
        }

        let typed_db_path = Utf8NativePathBuf::from(path);

        let mut joined_path = self.config.root_path.clone();

        for component in typed_db_path.components().skip(1) {
            joined_path.push(component);
        }

        Ok(joined_path.to_string().into())
    }

    fn translate_query_and_resolve_paths(
        &self,
        sql: &str,
    ) -> Result<(String, HashMap<String, String>), String> {
        let TranslatedQuery {
            databases,
            query: mut translated_query,
        } = translate_sql(sql)?;

        if translated_query.len() > 1 {
            return Err("Only one statement is allowed".to_owned());
        }

        let translated_query = translated_query.pop().ok_or("Query cannot be empty")?;

        let db_paths_by_reference = databases
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<HashMap<_, _>>();

        let qualified_fs_paths_by_reference = db_paths_by_reference
            .into_iter()
            .map(|(k, v)| {
                let unqualified_path = self.convert_db_path_to_unqualified_fs_path(&v)?;
                let qualified_path = resolve_path(&unqualified_path)?;
                Ok((k, qualified_path))
            })
            .collect::<Result<HashMap<_, _>, String>>()?;

        Ok((translated_query, qualified_fs_paths_by_reference))
    }

    pub fn execute_query<P: Params>(&self, sql: &str, params: P) -> Result<usize, String> {
        let (translated_query, qualified_fs_paths_by_reference) =
            self.translate_query_and_resolve_paths(sql)?;

        let sqlite_connection =
            if let Some(main_path) = qualified_fs_paths_by_reference.get("main") {
                rusqlite::Connection::open_with_flags(
                    main_path,
                    OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_CREATE,
                )
            } else {
                rusqlite::Connection::open_in_memory()
            }
            .map_err(|e| format!("Error opening SQLite connection: {}", e))?;

        sqlite_connection
            .pragma_update(None, "journal_mode", "WAL")
            .map_err(|e| format!("Error setting journal mode: {}", e))?;

        for (k, v) in qualified_fs_paths_by_reference.iter() {
            if k != "main" {
                sqlite_connection
                    .execute("ATTACH DATABASE ? AS ?", &[v, k])
                    .map_err(|e| format!("Error attaching database: {}", e))?;
            }
        }

        let mut prepared_statement = sqlite_connection
            .prepare(&translated_query)
            .map_err(|e| format!("Error preparing statement: {}", e))?;

        prepared_statement
            .execute(params)
            .map_err(|e| format!("Error executing statement: {}", e))
    }
}

#[cfg(test)]
mod tests {
    use sqlformat::FormatOptions;

    use super::*;

    fn dump_sqlite_file(path: &str) -> Result<String, String> {
        // TODO: since using sqlite3 here, don't use bundled rusqlite
        let output = std::process::Command::new("sqlite3")
            .arg(path)
            .arg(".dump")
            .output()
            .map_err(|e| format!("Error running sqlite3: {}", e))?;

        if !output.status.success() {
            return Err(format!(
                "Error running sqlite3: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        let dump = String::from_utf8(output.stdout)
            .map_err(|e| format!("Error converting output to string: {}", e))?;

        let formatted_dump = sqlformat::format(
            &dump,
            &sqlformat::QueryParams::None,
            FormatOptions::default(),
        );

        Ok(formatted_dump)
    }

    mod convert_db_path_to_fs_path {
        use super::*;

        #[test]
        fn simple() {
            let executor = TreeQLiteExecutor::new(Config {
                root_path: Utf8NativePathBuf::from("/home/user"),
            });

            let fs_path = executor
                .convert_db_path_to_unqualified_fs_path("~/books/things")
                .unwrap();

            assert_eq!(fs_path.0, "/home/user/books/things");
        }
    }

    mod execute_query {
        use super::*;
        use crate::common::split_by_line_and_trim_spaces;
        use rusqlite::params;
        use std::{collections::BTreeMap, fs::remove_dir_all, path::Path};
        use typed_path::Utf8NativePathBuf;

        #[derive(Serialize)]
        struct Report {
            dumps_by_path: BTreeMap<String, Vec<String>>,
            rows_changed_or_error: Result<usize, String>,
            original_query: Vec<String>,
            params: String,
        }

        fn make_executor(test_path: &str) -> TreeQLiteExecutor {
            let root_path = Utf8NativePathBuf::from("./test-db").join(test_path);

            // Delete the path to start fresh
            if AsRef::<Path>::as_ref(&root_path).exists() {
                remove_dir_all(&root_path).unwrap();
            }

            TreeQLiteExecutor::new(Config { root_path })
        }

        fn generate_report<P: Params + Serialize>(
            executor: TreeQLiteExecutor,
            sql: &str,
            params: P,
        ) -> Report {
            let params_json = serde_json::to_string(&params).unwrap();

            let rows_changed_or_error = executor.execute_query(sql, params);

            let (_, db_paths_by_reference) = executor
                .translate_query_and_resolve_paths(sql)
                .unwrap_or(("".to_owned(), HashMap::new()));

            let dumps_by_path = db_paths_by_reference
                .into_values()
                .map(|path| {
                    let dump = dump_sqlite_file(&path).unwrap();
                    let dump_lines = dump.split("\n").map(|s| s.to_string()).collect::<Vec<_>>();
                    (path, dump_lines)
                })
                .collect();

            let original_query = split_by_line_and_trim_spaces(sql);

            Report {
                dumps_by_path,
                original_query,
                params: params_json,
                rows_changed_or_error,
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
                );
            "#;

            let report = generate_report(make_executor("create_table"), sql, ());
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

            let report = generate_report(make_executor("create_table_using_parent"), sql, ());
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

            let report = generate_report(make_executor("create_table_using_curdir"), sql, ());
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

            let report = generate_report(
                make_executor("create_table_disallow_compound_identifier"),
                sql,
                (),
            );
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn unsupported_statement_type() {
            let sql = r#"close my_eyes;"#;

            let report = generate_report(make_executor("unsupported_statement_type"), sql, ());
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

            let report = generate_report(make_executor("create_table_outside_home"), sql, ());
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

            let report = generate_report(make_executor("create_table_root"), sql, ());
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

            let report = generate_report(
                make_executor("create_table_outside_home_using_parent"),
                sql,
                (),
            );
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn create_index() {
            let executor = make_executor("create_index");

            executor
                .execute_query(
                    r#"
                        CREATE TABLE IF NOT EXISTS "~/books/eloScores" (
                            thing_id INTEGER PRIMARY KEY NOT NULL,
                            score INT NOT NULL,
                        );
                    "#,
                    (),
                )
                .unwrap();

            let sql = r#"CREATE INDEX "scoreIndex" ON "~/books/eloScores" (score)"#;

            let report = generate_report(executor, sql, ());
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn create_index_compound_name() {
            let sql = r#"CREATE INDEX a.b ON "~/books/eloScores" (score)"#;

            let report = generate_report(make_executor("create_index_compound_name"), sql, ());
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn create_index_compound_table_name() {
            let sql = r#"CREATE INDEX a ON x.y (score)"#;

            let report =
                generate_report(make_executor("create_index_compound_table_name"), sql, ());
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn insert() {
            let executor = make_executor("insert");

            executor
                .execute_query(
                    r#"
                        CREATE TABLE IF NOT EXISTS "~/books/matches" (
                            "id" TEXT PRIMARY KEY NOT NULL,
                            "loserId" TEXT NOT NULL,
                            "winnerId" TEXT NOT NULL,
                            "matchDate" TEXT NOT NULL
                        );
                    "#,
                    (),
                )
                .unwrap();

            let report = generate_report(
                executor,
                r#"
                    INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
                    VALUES (?, ?, ?, ?)
                "#,
                ["match1", "loser1", "winner1", "today"],
            );
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn insert_from_select() {
            let executor = make_executor("insert_from_select");

            executor
                .execute_query(
                    r#"
                        CREATE TABLE IF NOT EXISTS "~/books/matches" (
                            "id" TEXT PRIMARY KEY NOT NULL,
                            "loserId" TEXT NOT NULL,
                            "winnerId" TEXT NOT NULL,
                            "matchDate" TEXT NOT NULL
                        );
                    "#,
                    (),
                )
                .unwrap();

            executor
                .execute_query(
                    r#"
                        INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
                        VALUES
                            ("heyyuy", "loser", "weener", "today"),
                            ("459", "looser", "winner", "tomorrow")
                    "#,
                    (),
                )
                .unwrap();

            let report = generate_report(
                executor,
                r#"
                    INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
                    SELECT "~/books/matches"."id" || '2', "loserId", "~/books/matches"."winnerId", "matchDate" FROM "~/books/matches"
                "#,
                (),
            );
            insta::assert_yaml_snapshot!(report);
        }

        #[test]
        fn parser_error() {
            let sql = r#"
                INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
                VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP('derp))
            "#;

            let report = generate_report(make_executor("parser_error"), sql, ());
            insta::assert_yaml_snapshot!(report);
        }

        // #[test]
        // fn select1_with_alias() {
        //     let sql = r#"
        //     SELECT
        //         things.id, things.title, things.subtitle, things.image_url, things.external_link,
        //         things.external_link_title, things.excluded
        //     FROM "~/books/things" AS "things"
        //     WHERE things.id = ?
        //     LIMIT 1
        // "#;

        //     let report = generate_report(make_executor("select1_with_alias"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn select1() {
        //     let sql = r#"
        //     SELECT
        //         "~/books/things".id, "~/books/things".title, "~/books/things".subtitle,
        //         "~/books/things".image_url, "~/books/things".external_link,
        //         "~/books/things".external_link_title, "~/books/things".excluded
        //     FROM "~/books/things"
        //     WHERE "~/books/things".id = ?
        //     LIMIT 1
        // "#;

        //     let report = generate_report(make_executor("select1"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn select_with_join() {
        //     let sql = r#"
        //     SELECT
        //         winner.score AS "winnerScore", loser.score AS "loserScore"
        //     FROM "~/books/eloScores" AS "winner"
        //     INNER JOIN "~/books/eloScores" AS "loser"
        //     WHERE
        //         winner.thing_id = ?
        //         AND loser.thing_id = ?
        // "#;

        //     let report = generate_report(make_executor("select_with_join"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn select_null() {
        //     let sql = "SELECT null";

        //     let report = generate_report(make_executor("select_null"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn update() {
        //     let sql = r#"UPDATE "~/books/eloScores" SET "score" = ? WHERE "thingId" = ?"#;

        //     let report = generate_report(make_executor("update"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn subquery() {
        //     let sql = r#"
        //     SELECT
        //         loserId, winnerId
        //     FROM "~/books/matches"
        //     WHERE loserId IN (
        //         SELECT thingId
        //         FROM "~/books/eloScores"
        //         ORDER BY score DESC, thingId DESC
        //         LIMIT 15
        //     )
        // "#;

        //     let report = generate_report(make_executor("subquery"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn union_in_subquery() {
        //     let sql = r#"
        //     SELECT
        //         sq.id, sq.num_matches
        //     FROM (
        //         SELECT books.id, 0 AS "num_matches"
        //         FROM "~/books/things" AS "books"
        //         LEFT JOIN "~/books/matches" AS "matches"
        //         ON (
        //             books.id = matches.winner_id
        //             OR books.id = matches.loser_id
        //         )
        //         WHERE matches.loser_id IS NULL

        //         UNION ALL

        //         SELECT winner_id AS "id", 1 AS "num_matches"
        //         FROM "~/books/matches"

        //         UNION ALL

        //         SELECT loser_id AS "id", 1 AS "num_matches"
        //         FROM "~/books/matches"
        //     ) AS "sq"
        //     GROUP BY sq.id
        //     ORDER BY sum(sq.num_matches) ASC
        //     LIMIT ?
        // "#;

        //     let report = generate_report(make_executor("union_in_subquery"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn unsupported_function() {
        //     let sql = "SELECT badfunc()";

        //     let report = generate_report(make_executor("unsupported_function"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn another_select() {
        //     let sql = r#"
        //     SELECT
        //         COUNT(*) AS "rank0"
        //     FROM "~/books/eloScores" AS "eloScores"
        //     JOIN "~/books/eloScores" AS "comparisonScore"
        //         ON eloScores.thingId = ?
        //     JOIN "~/books/things" AS "books"
        //         ON books.id = eloScores.thingId
        //     WHERE
        //         books.excluded = ?
        //         AND (
        //             eloScores.score > comparisonScore.score
        //             OR (
        //                 eloScores.score = comparisonScore.score
        //                 AND eloScores.thingId > comparisonScore.thingId
        //             )
        //         )
        //     LIMIT ? OFFSET ?
        // "#;

        //     let report = generate_report(make_executor("another_select"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn and_another() {
        //     let sql = r#"
        //         select "id"
        //         from (
        //             select
        //                 "~/books/things"."id" as "id",
        //                 0 as "num_matches" from "~/books/things"
        //             left join "~/books/matches" on (
        //                 "~/books/matches"."winner_id" = "~/books/things"."id"
        //                 or "~/books/matches"."loser_id" = "~/books/things"."id"
        //             )
        //             where "~/books/matches"."loser_id" is null

        //             union all

        //             select "winner_id" as "id", 1 as "num_matches" from "~/books/matches"

        //             union all

        //             select "loser_id" as "id", 1 as "num_matches" from "~/books/matches"
        //         ) as "sq"
        //         group by "id"
        //         order by sum("num_matches")
        //         limit ?
        //     "#;

        //     let report = generate_report(make_executor("and_another"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn select_wildcard() {
        //     let sql = r#"select * from "~/heyy""#;

        //     let report = generate_report(make_executor("select_wildcard"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        // #[test]
        // fn drop_table() {
        //     let sql = r#"
        //         create table "~/heyy" ("id" text primary key not null);
        //         drop table "~/heyy"
        //     "#;

        //     let report = generate_report(make_executor("drop_table"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }

        #[test]
        fn on_conflict_do_nothing() {
            let executor = make_executor("on_conflict_do_nothing");

            executor
                .execute_query(
                    r#"
                        CREATE TABLE IF NOT EXISTS "~/my-data-scraper/reelgood/shows-and-movies" (
                            "format" TEXT NOT NULL,
                            "isWatched" BOOLEAN NOT NULL,
                            "name" TEXT NOT NULL,
                            "url" TEXT PRIMARY KEY NOT NULL,
                            "imageUrl" TEXT NOT NULL
                        );
                    "#,
                    (),
                )
                .unwrap();

            executor
                .execute_query(
                    r#"
                        INSERT INTO "~/my-data-scraper/reelgood/shows-and-movies"
                        ("format", "isWatched", "name", "url", "imageUrl")
                        VALUES (?, ?, ?, ?, ?)
                    "#,
                    params![
                        "movie",
                        true,
                        "The Matrix",
                        "https://reelgood.com/movie/the-matrix",
                        "https://reelgood.com/movie/the-matrix/image",
                    ],
                )
                .unwrap();

            let report = generate_report(
                executor,
                r#"
                    insert into "~/my-data-scraper/reelgood/shows-and-movies"
                    ("format", "isWatched", "name", "url", "imageUrl")
                    values (?, ?, ?, ?, ?)
                    on conflict do nothing
                "#,
                (
                    "moovie",
                    false,
                    "overriiiiiide",
                    "https://reelgood.com/movie/the-matrix",
                    "https://reelgood.com/movie/the-matrix/no-image",
                ),
            );
            insta::assert_yaml_snapshot!(report);
        }

        // #[test]
        // fn union_separate_scopes() {
        //     let sql = r#"
        //     select "~/books/things".id, "~/books/things".title
        //     from "~/books/non-things" as "~/books/things"

        //     union

        //     select "~/books/things".id, "~/books/things".title
        //     from "~/books/things"
        // "#;

        //     let report = generate_report(make_executor("union_separate_scopes"), sql);
        //     insta::assert_yaml_snapshot!(report);
        // }
    }
}
