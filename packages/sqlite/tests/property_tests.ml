open Std
open Propane
open Result.Syntax

module Row = Sqlx_driver.Row
module Test = Std.Test
module Value = Sqlx_driver.Value

let property_examples = 256

let property_config = {
  Property.default_config with
  test_count = property_examples;
  max_size = 64;
  seed = Some 3_031;
}

let assert_property = fun name property ->
  match Property.check ~config:property_config property with
  | Property.Success -> Ok ()
  | Property.Failure { counter_example; shrink_steps } ->
      Error (name
      ^ " failed\nCounter-example:\n"
      ^ counter_example
      ^ "\nShrink steps: "
      ^ Int.to_string shrink_steps)
  | Property.Error { exception_; backtrace } ->
      Error (name ^ " raised " ^ Exception.to_string exception_ ^ "\n" ^ backtrace)
  | Property.Assumption_violated -> Error (name ^ " exhausted assumptions")

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

let short_text =
  Arbitrary.make ~print:String.escaped Generator.(string_size (int_range 0 48) char_printable)

let small_int = Arbitrary.make ~print:Int.to_string Generator.(int_range (-100_000) 100_000)

let blob_text =
  let byte = Generator.map Char.from_int_unchecked (Generator.int_range 0 255) in
  Arbitrary.make ~print:String.escaped Generator.(string_size (int_range 0 48) byte)

let scalar_roundtrip =
  Property.for_all
    Arbitrary.(triple small_int short_text bool)
    (fun (number, label, enabled) ->
      Sqlite.Testing.with_db
        (Sqlite.Config.in_memory ())
        (fun db ->
          let* _result =
            execute
              db
              "CREATE TABLE samples (number INTEGER NOT NULL, label TEXT NOT NULL, enabled INTEGER NOT NULL)"
              []
          in
          let* _result =
            execute
              db
              "INSERT INTO samples (number, label, enabled) VALUES (?, ?, ?)"
              [ Value.int number; Value.string label; Value.bool enabled ]
          in
          let* row = query_one db "SELECT number, label, enabled FROM samples" [] in
          let enabled_int =
            if enabled then
              1
            else
              0
          in
          Ok (Row.int "number" row = Some number
          && Row.string "label" row = Some label
          && Row.int "enabled" row = Some enabled_int))
      |> Result.unwrap_or ~default:false)

let blob_roundtrip =
  Property.for_all
    blob_text
    (fun payload ->
      Sqlite.Testing.with_db
        (Sqlite.Config.in_memory ())
        (fun db ->
          let bytes = IO.Bytes.from_string payload in
          let* row = query_one db "SELECT ? AS payload" [ Value.bytes bytes ] in
          match Row.bytes "payload" row with
          | Some actual -> Ok (String.equal (IO.Bytes.to_string actual) payload)
          | None -> Ok false)
      |> Result.unwrap_or ~default:false)

let statement_reuse_roundtrip =
  Property.for_all
    Arbitrary.(pair small_int small_int)
    (fun (left, right) ->
      Sqlite.Testing.with_db
        (Sqlite.Config.in_memory ())
        (fun db ->
          let* stmt =
            Sqlite.Driver.prepare db "SELECT ? AS value"
            |> Result.map_err ~fn:sqlite_error
          in
          let* first =
            Sqlite.Driver.execute stmt [ Value.int left ]
            |> Result.map_err ~fn:sqlite_error
          in
          let* second =
            Sqlite.Driver.execute stmt [ Value.int right ]
            |> Result.map_err ~fn:sqlite_error
          in
          match (Sqlite.Driver.fetch_row first, Sqlite.Driver.fetch_row second) with
          | (Some first_row, Some second_row) ->
              Ok (Row.int "value" first_row = Some left && Row.int "value" second_row = Some right)
          | _ -> Ok false)
      |> Result.unwrap_or ~default:false)

let property_scalar_roundtrip = fun _ctx ->
  assert_property
    "sqlite scalar roundtrip"
    scalar_roundtrip

let property_blob_roundtrip = fun _ctx -> assert_property "sqlite blob roundtrip" blob_roundtrip

let property_statement_reuse = fun _ctx ->
  assert_property
    "sqlite statement reuse"
    statement_reuse_roundtrip

let tests =
  Test.[
    property
      "property: scalar values roundtrip"
      ~examples:property_examples
      property_scalar_roundtrip;
    property "property: blob values roundtrip" ~examples:property_examples property_blob_roundtrip;
    property
      "property: prepared statements are reusable"
      ~examples:property_examples
      property_statement_reuse;
  ]

let main ~args = Test.Cli.main ~name:"sqlite_property_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
