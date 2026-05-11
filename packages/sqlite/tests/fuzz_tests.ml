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

let query_one = fun db sql params ->
  let* result = execute db sql params in
  match Sqlite.Driver.fetch_row result with
  | Some row -> Ok row
  | None -> Error "expected one row"

let text_mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 1_024
  |> with_dictionary
    [ ""; "'"; "\""; "\x00"; "SELECT"; "sqlite"; "line\nbreak"; "unicode-like-\xc3\xa9"; ])

let bytes_mutator =
  Test.Fuzz.Mutator.(bytes
  |> with_max_len 1_024
  |> with_dictionary [ ""; "\x00"; "\xff"; "\x00\x01\x02"; String.make ~len:256 ~char:'x' ])

let test_text_parameter_roundtrip = fun _ctx input ->
  Sqlite.Testing.with_db
    (Sqlite.Config.in_memory ())
    (fun db ->
      let* row = query_one db "SELECT ? AS value" [ Value.string input ] in
      match Row.string "value" row with
      | Some actual when String.equal actual input -> Ok ()
      | Some _ -> Error "text parameter did not roundtrip"
      | None -> Error "text parameter returned non-text value")

let test_blob_parameter_roundtrip = fun _ctx input ->
  Sqlite.Testing.with_db
    (Sqlite.Config.in_memory ())
    (fun db ->
      let payload = IO.Bytes.from_string input in
      let* row = query_one db "SELECT ? AS value" [ Value.bytes payload ] in
      match Row.bytes "value" row with
      | Some actual when String.equal (IO.Bytes.to_string actual) input -> Ok ()
      | Some _ -> Error "blob parameter did not roundtrip"
      | None -> Error "blob parameter returned non-blob value")

let test_like_escape_parameter = fun _ctx input ->
  Sqlite.Testing.with_db
    (Sqlite.Config.in_memory ())
    (fun db ->
      let* _result = execute db "CREATE TABLE entries (body TEXT NOT NULL)" [] in
      let* _result = execute db "INSERT INTO entries (body) VALUES (?)" [ Value.string input ] in
      let* row =
        query_one db "SELECT COUNT(*) AS count FROM entries WHERE body = ?" [ Value.string input ]
      in
      match Row.int "count" row with
      | Some 1 -> Ok ()
      | Some count -> Error ("expected one matching row, got " ^ Int.to_string count)
      | None -> Error "missing count row")

let tests =
  Test.[
    fuzz
      "sqlite text parameter roundtrips arbitrary text"
      ~seeds:[ ""; "Ada"; "quote'"; "line\nbreak"; "\x00inside"; ]
      ~mutator:text_mutator
      test_text_parameter_roundtrip;
    fuzz
      "sqlite blob parameter roundtrips arbitrary bytes"
      ~seeds:[ ""; "\x00"; "\xff"; "\x00\xffsqlite"; ]
      ~mutator:bytes_mutator
      test_blob_parameter_roundtrip;
    fuzz
      "sqlite text equality stays parameterized"
      ~seeds:[ ""; "%"; "_"; "' OR 1=1 --"; "normal text"; ]
      ~mutator:text_mutator
      test_like_escape_parameter;
  ]

let main ~args = Test.Cli.main ~name:"sqlite_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
