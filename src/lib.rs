mod common;
mod object_name_replacer;
mod path_convertor;
mod reference_extractor;
mod sql_ast_traversal;
mod tql_validator;

use object_name_replacer::{ObjectNameReplacer, ObjectNamesToReplace};
use path_convertor::DatabaseNamesByPath;
use reference_extractor::ReferenceExtractor;
use serde::Serialize;
use sql_ast_traversal::{helpers::extract_unary_identifier, traverser::SqlAstTraverser};
use sqlparser::{ast::ObjectName, dialect::SQLiteDialect, parser::Parser};
use std::collections::{BTreeMap, HashMap, HashSet};
use tql_validator::TqlValidator;
use typed_path::Utf8UnixPathBuf;
use wasm_bindgen::{prelude::wasm_bindgen, JsValue};

#[wasm_bindgen(typescript_custom_section)]
const TS_RUST_RESULT: &'static str = r#"
export type RustResult<O, E> = { Ok: O } | { Err: E };
"#;

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
    use sqlparser::ast::{
        Expr, GroupByExpr, Ident, ObjectName, Query, Select, SelectItem, SetExpr,
        Statement::{self},
        Value, WildcardAdditionalOptions,
    };
    use std::collections::HashMap;

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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Err(err) = translate_result {
            assert_eq!(
                err,
                "Expected 1 identifiers for the table name, got: [\"x\", \"y\"]"
            );
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
    }

    #[test]
    fn unsupported_statement_type() {
        let sql = r#"close my_eyes;"#;

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(err, "not implemented: Statement::Close");
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("sasas".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("/dev/null".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("etc/passwd".to_string(), "main".to_string())]),
            );

            assert_eq!(
                query,
                vec![[
                    &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                    "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                    "\"externalLink\" TEXT NOT NULL, ",
                    "\"externalLinkTitle\" TEXT NOT NULL, ",
                    "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                    "\"imageUrl\" TEXT NOT NULL, ",
                    "\"subtitle\" TEXT NULL, ",
                    "\"title\" TEXT NOT NULL",
                    ")"
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/things".to_string(), "main".to_string()),
                    ("~/books/thongs".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![
                    [
                        &format!("CREATE TABLE IF NOT EXISTS main.{} (", VALUES_TABLE_NAME),
                        "\"excluded\" BOOLEAN NOT NULL DEFAULT false, ",
                        "\"externalLink\" TEXT NOT NULL, ",
                        "\"externalLinkTitle\" TEXT NOT NULL, ",
                        "\"id\" TEXT PRIMARY KEY NOT NULL, ",
                        "\"imageUrl\" TEXT NOT NULL, ",
                        "\"subtitle\" TEXT NULL, ",
                        "\"title\" TEXT NOT NULL",
                        ")",
                    ]
                    .join(""),
                    [
                        &format!("CREATE TABLE IF NOT EXISTS db1.{} (", VALUES_TABLE_NAME),
                        "\"title\" TEXT NOT NULL",
                        ")"
                    ]
                    .join("")
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn create_index() {
        let sql = r#"CREATE INDEX "scoreIndex" ON "~/books/eloScores" (score)"#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/eloScores".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![format!(
                    "CREATE INDEX main.{}scoreIndex ON {}(score)",
                    VALUES_TABLE_INDEX_PREFIX, VALUES_TABLE_NAME
                )]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn create_index_compound_name() {
        let sql = r#"CREATE INDEX a.b ON "~/books/eloScores" (score)"#;

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(
                err,
                "Expected 1 identifiers for the index name, got: [\"a\", \"b\"]"
            );
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
    }

    #[test]
    fn create_index_compound_table_name() {
        let sql = r#"CREATE INDEX a ON x.y (score)"#;

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(
                err,
                "Expected 1 identifiers for the table name, got: [\"x\", \"y\"]"
            );
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
    }

    #[test]
    fn insert() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP)
        "#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/matches".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![
                    [
                        r#"INSERT INTO main.table_contents ("id", "loserId", "winnerId", "matchDate") "#,
                        r#"VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP)"#
                    ].join(""),
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn insert_from_select() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            SELECT "~/books/matches"."id" || '2', "loserId", "~/books/matches"."winnerId", "matchDate" FROM "~/books/matches"
        "#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/matches".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![
                    [
                        r#"INSERT INTO main.table_contents ("id", "loserId", "winnerId", "matchDate") "#,
                        r#"SELECT main.table_contents.id || '2', "loserId", main.table_contents.winnerId, "matchDate" FROM main.table_contents"#
                    ].join(""),
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn parser_error() {
        let sql = r#"
            INSERT INTO "~/books/matches" ("id", "loserId", "winnerId", "matchDate")
            VALUES (?, ?, DATETIME('now'), CURRENT_TIMESTAMP('derp))
        "#;

        let translate_result = translate_sql(sql);

        if let Err(err) = translate_result {
            assert_eq!(
                err,
                "sql parser error: Unterminated string literal at Line: 3, Column 62"
            );
        } else {
            panic!("Expected error, got: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![
                    [
                        "SELECT",
                        r#" "things".id, "things".title, "things".subtitle, "things".image_url, "things".external_link,"#,
                        r#" "things".external_link_title, "things".excluded"#,
                        r#" FROM main.table_contents AS "things""#,
                        r#" WHERE "things".id = ?"#,
                        " LIMIT 1",
                    ].join(""),
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/things".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![[
                        "SELECT",
                        " main.table_contents.id, main.table_contents.title, main.table_contents.subtitle,",
                        " main.table_contents.image_url, main.table_contents.external_link,",
                        " main.table_contents.external_link_title, main.table_contents.excluded",
                        " FROM main.table_contents",
                        " WHERE main.table_contents.id = ?",
                        " LIMIT 1",
                    ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/eloScores".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![[
                    "SELECT",
                    r#" "winner".score AS "winnerScore", "loser".score AS "loserScore""#,
                    r#" FROM main.table_contents AS "winner""#,
                    r#" JOIN main.table_contents AS "loser""#,
                    r#" WHERE "winner".thing_id = ? AND "loser".thing_id = ?"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn select_null() {
        let sql = "SELECT null";

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(databases.is_empty(), true,);

            assert_eq!(query, vec!["SELECT NULL".to_string()]);
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn update() {
        let sql = r#"UPDATE "~/books/eloScores" SET "score" = ? WHERE "thingId" = ?"#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/books/eloScores".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![
                    [r#"UPDATE main.table_contents SET "score" = ? WHERE "thingId" = ?"#,].join(""),
                ]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/eloScores".to_string(), "main".to_string()),
                    ("~/books/matches".to_string(), "db1".to_string()),
                ]),
            );

            assert_eq!(
                query,
                vec![[
                    "SELECT",
                    r#" loserId, winnerId"#,
                    r#" FROM db1.table_contents"#,
                    r#" WHERE loserId IN ("#,
                    r#"SELECT thingId"#,
                    r#" FROM main.table_contents"#,
                    r#" ORDER BY score DESC, thingId DESC"#,
                    r#" LIMIT 15"#,
                    r#")"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/matches".to_string(), "main".to_string()),
                    ("~/books/things".to_string(), "db1".to_string()),
                ]),
            );

            assert_eq!(
                query,
                vec![[
                    "SELECT",
                    r#" "sq".id, "sq".num_matches"#,
                    r#" FROM ("#,
                    r#"SELECT "books".id, 0 AS "num_matches""#,
                    r#" FROM db1.table_contents AS "books""#,
                    r#" LEFT JOIN main.table_contents AS "matches""#,
                    r#" ON ("#,
                    r#""books".id = "matches".winner_id"#,
                    r#" OR "books".id = "matches".loser_id"#,
                    r#")"#,
                    r#" WHERE "matches".loser_id IS NULL"#,
                    r#" UNION ALL"#,
                    r#" SELECT winner_id AS "id", 1 AS "num_matches""#,
                    r#" FROM main.table_contents"#,
                    r#" UNION ALL"#,
                    r#" SELECT loser_id AS "id", 1 AS "num_matches""#,
                    r#" FROM main.table_contents"#,
                    r#") AS "sq""#,
                    r#" GROUP BY "sq".id"#,
                    r#" ORDER BY sum("sq".num_matches) ASC"#,
                    r#" LIMIT ?"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn unsupported_function() {
        let sql = "SELECT badfunc()";

        let msg = translate_sql(sql).unwrap_err();
        assert_eq!(msg, "Unsupported function: badfunc")
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/eloScores".to_string(), "main".to_string()),
                    ("~/books/things".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![[
                    "SELECT",
                    r#" COUNT(*) AS "rank0""#,
                    r#" FROM main.table_contents AS "eloScores""#,
                    r#" JOIN main.table_contents AS "comparisonScore""#,
                    r#" ON "eloScores".thingId = ?"#,
                    r#" JOIN db1.table_contents AS "books""#,
                    r#" ON "books".id = "eloScores".thingId"#,
                    r#" WHERE"#,
                    r#" "books".excluded = ?"#,
                    r#" AND ("#,
                    r#""eloScores".score > "comparisonScore".score"#,
                    r#" OR ("#,
                    r#""eloScores".score = "comparisonScore".score"#,
                    r#" AND "eloScores".thingId > "comparisonScore".thingId"#,
                    r#")"#,
                    r#")"#,
                    r#" LIMIT ? OFFSET ?"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/books/things".to_string(), "db1".to_string()),
                    ("~/books/matches".to_string(), "main".to_string()),
                ]),
            );

            assert_eq!(
                query,
                vec![[
                    r#"SELECT "id""#,
                    r#" FROM ("#,
                    r#"SELECT"#,
                    r#" db1.table_contents.id AS "id","#,
                    r#" 0 AS "num_matches" FROM db1.table_contents"#,
                    r#" LEFT JOIN main.table_contents ON ("#,
                    r#"main.table_contents.winner_id = db1.table_contents.id"#,
                    r#" OR main.table_contents.loser_id = db1.table_contents.id"#,
                    r#")"#,
                    r#" WHERE main.table_contents.loser_id IS NULL"#,
                    r#" UNION ALL"#,
                    r#" SELECT "winner_id" AS "id", 1 AS "num_matches" FROM main.table_contents"#,
                    r#" UNION ALL"#,
                    r#" SELECT "loser_id" AS "id", 1 AS "num_matches" FROM main.table_contents"#,
                    r#") AS "sq""#,
                    r#" GROUP BY "id""#,
                    r#" ORDER BY sum("num_matches")"#,
                    r#" LIMIT ?"#,
                ]
                .join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn select_wildcard() {
        let sql = r#"select * from "~/heyy""#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/heyy".to_string(), "main".to_string()),]),
            );

            assert_eq!(
                query,
                vec![[r#"SELECT *"#, r#" FROM main.table_contents"#,].join(""),]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn drop_table() {
        let sql = r#"drop table "~/heyy""#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([("~/heyy".to_string(), "main".to_string()),]),
            );

            assert_eq!(query, vec![[r#"DROP TABLE main.table_contents"#,].join("")]);
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
    }

    #[test]
    fn drop_multiple_tables() {
        let sql = r#"drop table "~/heyy", "~/okokok""#;

        let translate_result = translate_sql(sql);

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([
                    ("~/heyy".to_string(), "main".to_string()),
                    ("~/okokok".to_string(), "db1".to_string())
                ]),
            );

            assert_eq!(
                query,
                vec![[r#"DROP TABLE main.table_contents, db1.table_contents"#,].join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        if let Ok(TranslatedQuery { databases, query }) = translate_result {
            assert_eq!(
                databases,
                HashMap::from([(
                    "~/my-data-scraper/reelgood/shows-and-movies".to_string(),
                    "main".to_string()
                ),]),
            );

            assert_eq!(
                query,
                vec![[
                    r#"INSERT INTO main.table_contents"#,
                    r#" ("format", "isWatched", "name", "url", "imageUrl")"#,
                    r#" VALUES (?, ?, ?, ?, ?)"#,
                    r#" ON CONFLICT DO NOTHING"#
                ]
                .join("")]
            );
        } else {
            panic!("Unexpected result: {:?}", translate_result);
        }
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

        let TranslatedQuery { databases, query } = translate_result.unwrap();

        assert_eq!(
            databases,
            HashMap::from([
                ("~/books/non-things".to_string(), "main".to_string()),
                ("~/books/things".to_string(), "db1".to_string())
            ]),
        );

        assert_eq!(
            query,
            vec![[
                r#"SELECT "~/books/things".id, "~/books/things".title"#,
                r#" FROM main.table_contents AS "~/books/things""#,
                r#" UNION "#,
                r#"SELECT db1.table_contents.id, db1.table_contents.title"#,
                r#" FROM db1.table_contents"#,
            ]
            .join("")]
        );
    }
}
