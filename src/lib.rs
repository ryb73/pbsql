mod common;
mod object_name_replacer;
mod path_convertor;
mod reference_extractor;
mod sql_ast_traversal;
mod tql_validator;

use object_name_replacer::{ObjectNameReplacer, ObjectNamesToReplace};
use path_convertor::DatabaseNamesByPath;
use reference_extractor::ReferenceExtractor;
use serde::{Deserialize, Serialize};
use sql_ast_traversal::{helpers::extract_unary_identifier, traverser::SqlAstTraverser};
use sqlparser::{ast::ObjectName, dialect::SQLiteDialect, parser::Parser};
use std::collections::{BTreeMap, HashMap, HashSet};
use tql_validator::TqlValidator;
use tsify::Tsify;
use typed_path::Utf8UnixPathBuf;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

#[wasm_bindgen(typescript_custom_section)]
const TS_RUST_RESULT: &'static str = r#"
export type RustResult<O, E> = { Ok: O } | { Err: E };
"#;

#[derive(Tsify, Serialize, Deserialize)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub struct RustResult2<O, E> {
    ok: Option<O>,
    err: Option<E>,
}

impl<O, E> From<Result<O, E>> for RustResult2<O, E> {
    fn from(result: Result<O, E>) -> Self {
        match result {
            Ok(ok) => RustResult2 {
                ok: Some(ok),
                err: None,
            },
            Err(err) => RustResult2 {
                ok: None,
                err: Some(err),
            },
        }
    }
}

const VALUES_TABLE_NAME: &str = "table_contents";
const VALUES_TABLE_INDEX_PREFIX: &str = "tbl_";

#[derive(Debug, Serialize)]
struct TranslatedQuery<Query> {
    databases: DatabaseNamesByPath,
    query: Query,
}

struct RelationResolver {
    num_relations: usize,
}

impl RelationResolver {
    fn new() -> Self {
        Self { num_relations: 0 }
    }

    fn get_next_relation_identifiers(&mut self) -> [String; 2] {
        let current_num = self.num_relations;

        self.num_relations += 1;

        if current_num == 0 {
            return ["main".to_string(), VALUES_TABLE_NAME.to_string()];
        }

        [
            "db".to_string() + &current_num.to_string(),
            VALUES_TABLE_NAME.to_string(),
        ]
    }
}

fn translate_sql(query: &str) -> Result<TranslatedQuery<Vec<String>>, String> {
    let dialect = SQLiteDialect {};

    let mut ast = Parser::parse_sql(&dialect, query).map_err(|e| e.to_string())?;

    let mut validator = TqlValidator::new();
    validator.traverse(&mut ast)?;

    let mut extractor = ReferenceExtractor::new();
    extractor.traverse(&mut ast)?;

    let relations_by_path = group_relations_by_path(extractor.relations)?;

    let mut relation_resolver = RelationResolver::new();

    let new_identifiers_by_path = relations_by_path
        .keys()
        .map(|path| (path, relation_resolver.get_next_relation_identifiers()))
        .collect::<HashMap<_, _>>();

    let relations_to_replace = relations_by_path
        .iter()
        .map(|(path, relations_referring_to_path)| {
            let new_identifiers_for_path = new_identifiers_by_path.get(path).unwrap();

            relations_referring_to_path
                .into_iter()
                .map(|ObjectName(old_identifiers)| {
                    (
                        old_identifiers
                            .into_iter()
                            .map(|i| i.value.to_owned())
                            .collect(),
                        new_identifiers_for_path.to_vec(),
                    )
                })
        })
        .flatten()
        .collect::<HashMap<Vec<String>, Vec<String>>>();

    let indices_to_replace = determine_new_index_names(&extractor.indices)?;

    let object_names_to_replace = ObjectNamesToReplace {
        indices_to_replace,
        relations_to_replace,
    };
    let mut object_name_replacer = ObjectNameReplacer::new(&object_names_to_replace);

    object_name_replacer.traverse(&mut ast)?;

    // println!("Output AST: {:#?}", ast);

    let database_names = new_identifiers_by_path
        .into_iter()
        .map(|(path, new_identifiers)| (path.to_string(), new_identifiers[0].to_owned()))
        .collect();

    Ok(TranslatedQuery {
        databases: database_names,
        query: ast.iter().map(|s| s.to_string()).collect(),
    })
}

fn determine_new_index_names(
    indices: &HashSet<ObjectName>,
) -> Result<HashMap<String, String>, String> {
    indices
        .iter()
        .map(|ObjectName(identifiers)| {
            let index_name = extract_unary_identifier(identifiers, "index")?.to_string();

            let new_index_name = format!("{}{}", VALUES_TABLE_INDEX_PREFIX, index_name);

            Ok((index_name, new_index_name))
        })
        .collect()
}

fn normalize_path(path: &ObjectName) -> Result<String, String> {
    let ObjectName(identifiers) = path;

    let path_string = extract_unary_identifier(identifiers, "relation")?;

    let path_buf = Utf8UnixPathBuf::from(path_string);
    let path_buf = path_buf.normalize();

    Ok(path_buf.to_string())
}

// Return a BTreePath so that the iteration order is deterministic, just for testing purposes.
// I figure N won't be high so the performance hit relative to a HashMap is negligible.
fn group_relations_by_path(
    relations: HashSet<ObjectName>,
) -> Result<BTreeMap<String, Vec<ObjectName>>, String> {
    let mut relations_by_path = BTreeMap::new();

    for relation in relations {
        let path = normalize_path(&relation)?;

        let entry = relations_by_path.entry(path).or_insert_with(Vec::new);
        entry.push(relation);
    }

    Ok(relations_by_path)
}

pub fn extract_references(sql: &str) -> Result<ExtractedReferences, String> {
    let dialect = SQLiteDialect {};

    let mut ast = Parser::parse_sql(&dialect, sql).map_err(|e| e.to_string())?;

    let mut extractor = ReferenceExtractor::new();
    extractor.traverse(&mut ast)?;

    Ok(extractor.into())
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi, from_wasm_abi)]
pub struct ExtractedReferences {
    pub relations: Vec<Vec<String>>,
    pub indices: Vec<Vec<String>>,
}

impl From<ReferenceExtractor> for ExtractedReferences {
    fn from(extractor: ReferenceExtractor) -> Self {
        Self {
            relations: extractor
                .relations
                .into_iter()
                .map(|ObjectName(identifiers)| identifiers.into_iter().map(|i| i.value).collect())
                .collect(),
            indices: extractor
                .indices
                .into_iter()
                .map(|ObjectName(identifiers)| identifiers.into_iter().map(|i| i.value).collect())
                .collect(),
        }
    }
}

#[wasm_bindgen(js_name = extractReferences)]
pub fn extract_references_wasm(sql: &str) -> RustResult2<ExtractedReferences, String> {
    extract_references(sql).into()
}

fn validate_tql_result(sql: &str) -> Result<(), String> {
    let dialect = SQLiteDialect {};

    let mut ast = Parser::parse_sql(&dialect, sql).map_err(|e| e.to_string())?;

    let mut validator = TqlValidator::new();
    validator.traverse(&mut ast)?;

    Ok(())
}

#[wasm_bindgen(getter_with_clone)]
pub struct ValidationError {
    pub message: String,
}

#[wasm_bindgen(js_name = validateTql)]
pub fn validate_tql(sql: &str) -> Option<ValidationError> {
    validate_tql_result(sql)
        .err()
        .map(|e| ValidationError { message: e })
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "RustResult<TranslatedQuery, string>")]
    pub type JsTranslatedQueryResult;
}

#[wasm_bindgen(typescript_custom_section)]
const TS_RUST_RESULT: &'static str = r#"
export type TranslatedQuery = {
    databases: Map<string, string>;
    query: string[];
};
"#;

#[wasm_bindgen(js_name = translateSql)]
pub fn translate_sql_wasm(query: &str) -> Result<JsTranslatedQueryResult, JsValue> {
    let result = translate_sql(query);
    serde_wasm_bindgen::to_value(&result)
        .map_err(|err| JsValue::from_str(&format!("{:?}", err)))
        .map(|x| x.into())
}

#[cfg(test)]
mod tests {
    use self::common::split_by_line_and_trim_spaces;
    use sqlparser::ast::{
        Expr, GroupByExpr, Ident, ObjectName, Query, Select, SelectItem, SetExpr,
        Statement::{self},
        Value, WildcardAdditionalOptions,
    };

    use super::*;

    #[test]
    fn build_sql_test() {
        let blah = Statement::Insert {
            or: None,
            ignore: false,
            into: true,
            table_name: ObjectName(vec![Ident::new("table")]),
            table_alias: Some(Ident::new("t")),
            columns: vec![Ident::new("a")],
            overwrite: false,
            source: Some(Box::new(Query {
                body: Box::new(SetExpr::Select(Box::new(Select {
                    distinct: None,
                    top: None,
                    projection: vec![SelectItem::Wildcard(WildcardAdditionalOptions {
                        opt_exclude: None,
                        opt_except: None,
                        opt_rename: None,
                        opt_replace: None,
                    })],
                    into: None,
                    from: vec![],
                    lateral_views: vec![],
                    selection: Some(Expr::Value(Value::Number("1".to_string(), true))),
                    group_by: GroupByExpr::Expressions(vec![]),
                    cluster_by: vec![],
                    distribute_by: vec![],
                    sort_by: vec![],
                    having: None,
                    named_window: vec![],
                    qualify: None,
                }))),
                with: None,
                order_by: vec![],
                limit: None,
                limit_by: vec![],
                offset: None,
                fetch: None,
                locks: vec![],
                for_clause: None,
            })),
            partitioned: None,
            after_columns: vec![],
            table: false,
            on: None,
            returning: None,
            replace_into: false,
            priority: None,
        };

        let compiled = blah.to_string();

        println!("Compiled: {}", compiled);
    }

    mod translator {
        use super::*;
        use sqlformat::{FormatOptions, QueryParams};
        use std::collections::BTreeMap;

        #[derive(serde::Serialize)]
        struct Report {
            database_names: Option<BTreeMap<String, String>>,
            translated_query: Result<Vec<Vec<String>>, String>,
            original_query: Vec<String>,
        }

        fn generate_report(sql: &str) -> Report {
            let translate_result = translate_sql(sql);

            let original_query = split_by_line_and_trim_spaces(sql);

            if let Ok(TranslatedQuery { databases, query }) = &translate_result {
                Report {
                    database_names: Some(
                        databases
                            .into_iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .collect(),
                    ),
                    translated_query: Ok(query
                        .into_iter()
                        .map(|s| {
                            split_by_line_and_trim_spaces(&sqlformat::format(
                                &s,
                                &QueryParams::None,
                                FormatOptions::default(),
                            ))
                        })
                        .collect()),
                    original_query,
                }
            } else {
                Report {
                    database_names: None,
                    translated_query: Err(translate_result.unwrap_err()),
                    original_query,
                }
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
