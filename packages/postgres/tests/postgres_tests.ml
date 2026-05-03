open Std
open Result.Syntax

let postgres_url () =
  match Env.get Env.String ~var:"POSTGRES_TEST_URL" with
  | Some url -> Some url
  | None -> (
      match Env.get Env.String ~var:"MULE_TEST_POSTGRES_URL" with
      | Some url -> Some url
      | None -> Env.get Env.String ~var:"HYPEKIT_POSTGRES_URL"
    )

let with_connection url fn =
  match Postgres.Config.from_string url with
  | Error message -> Error ("invalid postgres test url: " ^ message)
  | Ok config -> (
      match Postgres.Driver.connect config with
      | Error error -> Error (Postgres.Driver.error_to_string error)
      | Ok connection ->
          let result = fn connection in
          Postgres.Driver.close connection;
          result
    )

let query_one connection sql params =
  let* statement =
    Postgres.Driver.prepare connection sql
    |> Result.map_err ~fn:Postgres.Driver.error_to_string
  in
  let* result_set =
    Postgres.Driver.execute statement params
    |> Result.map_err ~fn:Postgres.Driver.error_to_string
  in
  match Postgres.Driver.fetch_row result_set with
  | Some row -> Ok row
  | None -> Error "expected one row"

let test_parameterized_query_roundtrips = fun _ctx url ->
  with_connection
    url
    (fun connection ->
      let* row =
        query_one
          connection
          "select $1::text as value, $2::int4 as n"
          [ Sqlx_driver.Value.string "hello"; Sqlx_driver.Value.int 42 ]
      in
      Test.assert_equal ~expected:(Some "hello") ~actual:(Sqlx_driver.Row.string "value" row);
      Test.assert_equal ~expected:(Some 42) ~actual:(Sqlx_driver.Row.int "n" row);
      Ok ())

let test_parameterized_query_handles_large_text = fun _ctx url ->
  with_connection
    url
    (fun connection ->
      let body = String.make ~len:20_000 ~char:'x' in
      let* row =
        query_one
          connection
          "select $1::text as body, length($1::text)::int4 as len"
          [ Sqlx_driver.Value.string body ]
      in
      Test.assert_equal ~expected:(Some body) ~actual:(Sqlx_driver.Row.string "body" row);
      Test.assert_equal ~expected:(Some 20_000) ~actual:(Sqlx_driver.Row.int "len" row);
      Ok ())

let live_case = fun name fn ->
  match postgres_url () with
  | None -> Test.skip ~size:Large name (fun _ctx -> Ok ())
  | Some url -> Test.case ~size:Large name (fun ctx -> fn ctx url)

let tests =
  Test.[
    live_case "parameterized query roundtrips" test_parameterized_query_roundtrips;
    live_case "parameterized query handles large text" test_parameterized_query_handles_large_text;
  ]

let main ~args = Test.Cli.main ~name:"postgres_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
