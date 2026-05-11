open Std

module Config = Sqlite__Config
module Driver = Sqlite__Driver

let temp_path = fun config tmpdir ->
  if String.equal (Path.to_string Config.(config.path)) ":memory:" then
    config
  else
    { config with path = Path.(tmpdir / Path.v "sqlite-test.db"); mode = Config.Create }

let with_db = fun config fn ->
  match Fs.with_tempdir
    ~prefix:"sqlite_test_"
    (fun tmpdir ->
      let config = temp_path config tmpdir in
      match Driver.connect config with
      | Error error -> Error (Driver.error_to_string error)
      | Ok db ->
          let result =
            try fn db with
            | exn -> Error ("SQLite test callback raised: " ^ Exception.to_string exn)
          in
          Driver.close db;
          result) with
  | Error error -> Error ("SQLite tempdir failed: " ^ IO.error_message error)
  | Ok result -> result
