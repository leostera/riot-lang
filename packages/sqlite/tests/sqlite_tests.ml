open Std
open Result.Syntax

module Row = Sqlx_driver.Row
module Test = Std.Test
module Value = Sqlx_driver.Value

let sqlite_error = Sqlite.Driver.error_to_string

let execute = fun db sql params ->
  let* stmt =
    Sqlite.Driver.prepare db sql
    |> Result.map_err ~fn:sqlite_error
  in
  Sqlite.Driver.execute stmt params
  |> Result.map_err ~fn:sqlite_error

let exec_unit = fun db sql params ->
  let* _result = execute db sql params in
  Ok ()

let query_one = fun db sql params ->
  let* result = execute db sql params in
  match Sqlite.Driver.fetch_row result with
  | Some row -> Ok row
  | None -> Error "expected one row"

let with_memory_db = fun fn -> Sqlite.Testing.with_db (Sqlite.Config.in_memory ()) fn

let expect_int = fun ~field ~expected row ->
  match Row.int field row with
  | Some actual when actual = expected -> Ok ()
  | actual ->
      Error (
        "expected " ^ field ^ " = " ^ Int.to_string expected ^ ", got " ^ (
          match actual with
          | Some value -> Int.to_string value
          | None -> "None"
        )
      )

let expect_string = fun ~field ~expected row ->
  match Row.string field row with
  | Some actual when String.equal actual expected -> Ok ()
  | actual ->
      Error (
        "expected " ^ field ^ " = " ^ expected ^ ", got " ^ (
          match actual with
          | Some value -> value
          | None -> "None"
        )
      )

let test_create_insert_query = fun _ctx ->
  with_memory_db
    (fun db ->
      let* () =
        exec_unit
          db
          "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, active INTEGER, note TEXT)"
          []
      in
      let* inserted =
        execute
          db
          "INSERT INTO users (name, active, note) VALUES (?, ?, ?)"
          [ Value.string "Ada"; Value.bool true; Value.null ]
      in
      Test.assert_equal ~expected:1 ~actual:(Sqlite.Driver.rows_affected inserted);
      let* row =
        query_one
          db
          "SELECT id, name, active, note FROM users WHERE name = ?"
          [ Value.string "Ada" ]
      in
      let* () = expect_int ~field:"id" ~expected:1 row in
      let* () = expect_string ~field:"name" ~expected:"Ada" row in
      let* () = expect_int ~field:"active" ~expected:1 row in
      match Row.get "note" row with
      | Some Value.Null -> Ok ()
      | _ -> Error "expected note to be NULL")

let test_fetch_row_advances = fun _ctx ->
  with_memory_db
    (fun db ->
      let* () = exec_unit db "CREATE TABLE nums (n INTEGER NOT NULL)" [] in
      let* _result = execute db "INSERT INTO nums (n) VALUES (1), (2), (3)" [] in
      let* result = execute db "SELECT n FROM nums ORDER BY n" [] in
      let first = Sqlite.Driver.fetch_row result in
      let second = Sqlite.Driver.fetch_row result in
      let third = Sqlite.Driver.fetch_row result in
      let fourth = Sqlite.Driver.fetch_row result in
      match (first, second, third, fourth) with
      | (Some r1, Some r2, Some r3, None) ->
          let* () = expect_int ~field:"n" ~expected:1 r1 in
          let* () = expect_int ~field:"n" ~expected:2 r2 in
          expect_int ~field:"n" ~expected:3 r3
      | _ -> Error "expected exactly three rows")

let test_transactions_rollback = fun _ctx ->
  with_memory_db
    (fun db ->
      let* () = exec_unit db "CREATE TABLE items (name TEXT NOT NULL)" [] in
      let* () =
        Sqlite.Driver.begin_transaction db
        |> Result.map_err ~fn:sqlite_error
      in
      let* () = exec_unit db "INSERT INTO items (name) VALUES (?)" [ Value.string "rolled back" ] in
      let* () =
        Sqlite.Driver.rollback db
        |> Result.map_err ~fn:sqlite_error
      in
      let* row = query_one db "SELECT COUNT(*) AS count FROM items" [] in
      expect_int ~field:"count" ~expected:0 row)

let test_testing_with_db_uses_temp_file = fun _ctx ->
  let config = Sqlite.Config.default (Path.v "ignored-by-testing.db") in
  Sqlite.Testing.with_db
    config
    (fun db ->
      let* () = exec_unit db "CREATE TABLE kv (key TEXT PRIMARY KEY, value TEXT)" [] in
      let* _result =
        execute
          db
          "INSERT INTO kv (key, value) VALUES (?, ?)"
          [ Value.string "a"; Value.string "b" ]
      in
      let* row = query_one db "SELECT value FROM kv WHERE key = ?" [ Value.string "a" ] in
      expect_string ~field:"value" ~expected:"b" row)

let test_sqlx_pool_smoke = fun _ctx ->
  let pool_config = { Sqlx.Config.default with pool_size = 1 } in
  match Sqlx.connect ~config:pool_config ~driver:(module Sqlite.Driver) (Sqlite.Config.in_memory ()) with
  | Error error -> Error (Sqlx.show_error error)
  | Ok pool ->
      let finish result =
        Sqlx.shutdown pool;
        result
      in
      let result =
        let* _rows =
          Sqlx.exec pool "CREATE TABLE logs (message TEXT NOT NULL)" []
          |> Result.map_err ~fn:Sqlx.show_error
        in
        let* rows =
          Sqlx.exec pool "INSERT INTO logs (message) VALUES (?)" [ Value.string "hello" ]
          |> Result.map_err ~fn:Sqlx.show_error
        in
        Test.assert_equal ~expected:1 ~actual:rows;
        let* cursor =
          Sqlx.query pool "SELECT message FROM logs" []
          |> Result.map_err ~fn:Sqlx.show_error
        in
        match Sqlx.Cursor.fetch_one cursor with
        | Some row -> expect_string ~field:"message" ~expected:"hello" row
        | None -> Error "expected one sqlx row"
      in
      finish result

let test_prepare_reports_sql_errors = fun _ctx ->
  with_memory_db
    (fun db ->
      match Sqlite.Driver.prepare db "SELECT FROM" with
      | Ok _ -> Error "expected invalid SQL to fail during prepare"
      | Error error ->
          let message = Sqlite.Driver.error_to_string error in
          if String.contains message "syntax" then
            Ok ()
          else
            Error ("expected syntax error, got: " ^ message))

let tests =
  Test.[
    case "sqlite create insert query" test_create_insert_query;
    case "sqlite fetch_row advances" test_fetch_row_advances;
    case "sqlite rollback restores state" test_transactions_rollback;
    case "sqlite testing with_db uses temp file" test_testing_with_db_uses_temp_file;
    case "sqlite works through sqlx pool" test_sqlx_pool_smoke;
    case "sqlite prepare reports SQL errors" test_prepare_reports_sql_errors;
  ]

let main ~args = Test.Cli.main ~name:"sqlite_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
