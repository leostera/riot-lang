open Std
open Miniriot

type db_type = Sqlite | Postgres

let print_row row =
  let fields = Sqlx_driver.Row.fields row in
  List.iter
    (fun field ->
      match Sqlx_driver.Row.get field row with
      | None -> Printf.printf "%s: NULL  " field
      | Some value ->
          let str =
            match value with
            | Sqlx_driver.Value.Null -> "NULL"
            | Sqlx_driver.Value.Int n -> string_of_int n
            | Sqlx_driver.Value.Int64 n -> Int64.to_string n
            | Sqlx_driver.Value.Int16 n -> string_of_int n
            | Sqlx_driver.Value.Float f -> string_of_float f
            | Sqlx_driver.Value.String s -> format "\"%s\"" s
            | Sqlx_driver.Value.Bool b -> string_of_bool b
            | Sqlx_driver.Value.Bytes _ -> "<bytes>"
            | Sqlx_driver.Value.Timestamp _t -> "<timestamp>"
            | Sqlx_driver.Value.TimestampWithTimezone (_, tz, offset) ->
                format "<timestamp_with_timezone:%s%+d>"
                  (Datetime.Tz.to_string tz) offset
            | Sqlx_driver.Value.Date (y, m, d) -> format "%04d-%02d-%02d" y m d
            | Sqlx_driver.Value.Time (h, min, s, us) ->
                format "%02d:%02d:%02d.%06d" h min s us
            | Sqlx_driver.Value.Uuid u -> u
            | Sqlx_driver.Value.Json j -> j
            | Sqlx_driver.Value.Numeric n -> n
          in
          print "%s: %s  " field str)
    fields;
  print_endline ""

let execute_query pool sql =
  Log.info "Executing: %s" sql;
  match Sqlx.query pool sql [] with
  | Error e ->
      Log.error "Query failed: %s" (Sqlx.show_error e);
      Error ()
  | Ok cursor ->
      let rec print_rows count =
        match Sqlx.Cursor.fetch_one cursor with
        | None ->
            Log.info "Query returned %d rows" count;
            Ok ()
        | Some row ->
            print_row row;
            print_rows (count + 1)
      in
      print_rows 0

let run_sqltool matches =
  Log.set_level Log.Info;

  let sqlite_path = ArgParser.get_one matches "sqlite" in
  let postgres_conn = ArgParser.get_one matches "postgres" in
  let query = ArgParser.get_one matches "query" in

  let pool_result =
    match (sqlite_path, postgres_conn) with
    | Some path, None ->
        let sqlite_config =
          if path = ":memory:" then Sqlite.Config.in_memory ()
          else Sqlite.Config.default (Path.v path)
        in
        Log.info "Connecting to SQLite database: %s" path;
        Sqlx.connect ~driver:(module Sqlite.Driver) sqlite_config
    | None, Some conn -> (
        match Postgres.Config.from_string conn with
        | Error msg ->
            Log.error "Invalid PostgreSQL connection: %s" msg;
            Error (Sqlx.Connection_failed msg)
        | Ok pg_config ->
            Log.info "Connecting to PostgreSQL: %s" pg_config.host;
            let pool_config = { Sqlx.Config.default with pool_size = 1 } in
            Sqlx.connect ~config:pool_config
              ~driver:(module Postgres.Driver)
              pg_config)
    | None, None ->
        Log.error "Must specify either --sqlite or --postgres";
        Error (Sqlx.Connection_failed "No database specified")
    | Some _, Some _ ->
        Log.error "Cannot specify both --sqlite and --postgres";
        Error (Sqlx.Connection_failed "Conflicting database options")
  in

  match pool_result with
  | Error e ->
      Log.error "Connection failed: %s" (Sqlx.show_error e);
      Error ()
  | Ok pool ->
      Log.info "Connected successfully!";

      let result =
        match query with
        | Some sql -> execute_query pool sql
        | None ->
            Log.info "Interactive mode not yet implemented";
            Log.info "Use --query to execute SQL";
            Ok ()
      in

      Sqlx.shutdown pool;
      result

let main ~args:_ =
  let cmd =
    ArgParser.command "sqltool"
    |> ArgParser.about "Simple SQL command-line tool for testing SQLx"
    |> ArgParser.version "0.1.0"
    |> ArgParser.args
         [
           ArgParser.Arg.option "sqlite"
           |> ArgParser.Arg.long "sqlite"
           |> ArgParser.Arg.value_name "PATH"
           |> ArgParser.Arg.help
                "Use SQLite database at path (use ':memory:' for in-memory)";
           ArgParser.Arg.option "postgres"
           |> ArgParser.Arg.long "postgres"
           |> ArgParser.Arg.value_name "CONNECTION"
           |> ArgParser.Arg.help
                "PostgreSQL connection (postgresql://user:pass@host:port/db or \
                 host:port:db:user:pass)";
           ArgParser.Arg.option "query"
           |> ArgParser.Arg.short 'q' |> ArgParser.Arg.long "query"
           |> ArgParser.Arg.value_name "SQL"
           |> ArgParser.Arg.help "Execute SQL query and exit";
         ]
  in

  let matches =
    match ArgParser.get_matches cmd Env.args with
    | Ok m -> m
    | Error e ->
        ArgParser.print_error e;
        ArgParser.print_help cmd;
        failwith "Invalid arguments"
  in

  match run_sqltool matches with
  | Ok () -> Ok ()
  | Error () -> Error (Failure "sqltool failed")

let () = Miniriot.run ~main ~args:Env.args
