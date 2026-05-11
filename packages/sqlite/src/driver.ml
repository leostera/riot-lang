open Std
open Result.Syntax

module Config = Sqlite__Config
module Error = Sqlite__Error
module Native = Sqlite__Native
module Queue = Collections.Queue
module ValueCodec = Sqlite__Value_codec

type config = Config.t

type connection = {
  id: string;
  path: Path.t;
  handle: Native.db;
  mutable statements: statement list;
  mutable closed: bool;
  mutable in_transaction: bool;
}

and statement = {
  sql: string;
  conn: connection;
  native: Native.stmt;
  mutable finalized: bool;
}

type result_set = {
  rows: Sqlx_driver.Row.t Queue.t;
  rows_affected: int;
}

type error = Error.t

let name = "SQLite"

let error_to_string = Error.to_string

let error_serializer = Error.serializer

let row_code = Native.row_code ()

let done_code = Native.done_code ()

let open_flags = fun config ->
  let mode_flags =
    match Config.(config.mode) with
    | Config.ReadOnly -> Native.flag_readonly ()
    | Config.ReadWrite -> Native.flag_readwrite ()
    | Config.Create -> Native.flag_readwrite () lor Native.flag_create ()
  in
  mode_flags lor Native.flag_uri ()

let random_id = fun () ->
  Random.int 1_000_000
  |> Result.map ~fn:(fun id -> "sqlite_" ^ Int.to_string id)
  |> Result.map_err ~fn:Random.error_to_string

let reset_statement = fun stmt ->
  Native.reset stmt
  |> Result.map_err ~fn:(fun cause -> Error.ResetFailed cause)

let finalize_statement = fun stmt ->
  if stmt.finalized then
    Ok ()
  else (
    stmt.finalized <- true;
    Native.finalize stmt.native
    |> Result.map_err ~fn:(fun cause -> Error.FinalizeFailed cause)
  )

let run_prepared = fun conn sql stmt params ->
  let rows = Queue.create () in
  let readonly = Native.stmt_readonly stmt in
  let* () = reset_statement stmt in
  let* () =
    Native.clear_bindings stmt
    |> Result.map_err ~fn:(fun cause -> Error.ExecutionFailed { sql; cause })
  in
  let* () = ValueCodec.bind_params stmt params in
  let rec step_loop () =
    match Native.step stmt with
    | Error cause ->
        let _ = Native.reset stmt in
        Error (Error.ExecutionFailed { sql; cause })
    | Ok code when code = row_code ->
        Queue.push rows ~value:(ValueCodec.read_row stmt);
        step_loop ()
    | Ok code when code = done_code ->
        let rows_affected =
          if readonly then
            0
          else
            Native.changes conn.handle
        in
        let* () = reset_statement stmt in
        Ok { rows; rows_affected }
    | Ok code ->
        let _ = Native.reset stmt in
        Error (Error.ExecutionFailed {
          sql;
          cause = {
            Native.code = code;
            message = "unexpected sqlite3_step result code " ^ Int.to_string code;
          };
        })
  in
  step_loop ()

let run_sql_on_handle = fun handle sql ->
  let* stmt =
    Native.prepare handle sql
    |> Result.map_err ~fn:(fun cause -> Error.ConfigurationFailed { statement = sql; cause })
  in
  let rec drain () =
    match Native.step stmt with
    | Error cause -> Error (Error.ConfigurationFailed { statement = sql; cause })
    | Ok code when code = row_code -> drain ()
    | Ok code when code = done_code -> Ok ()
    | Ok code ->
        Error (Error.ConfigurationFailed {
          statement = sql;
          cause = {
            Native.code = code;
            message = "unexpected sqlite3_step result code " ^ Int.to_string code;
          };
        })
  in
  let result = drain () in
  match (result, Native.finalize stmt) with
  | (Ok (), Error cause) -> Error (Error.FinalizeFailed cause)
  | _ -> result

let synchronous_sql = fun mode ->
  match mode with
  | Config.Off -> "OFF"
  | Config.Normal -> "NORMAL"
  | Config.Full -> "FULL"
  | Config.Extra -> "EXTRA"

let apply_config = fun handle config ->
  let* () =
    match Config.(config.busy_timeout) with
    | None -> Ok ()
    | Some timeout ->
        Native.busy_timeout handle (Time.Duration.to_millis timeout)
        |> Result.map_err
          ~fn:(fun cause -> Error.ConfigurationFailed { statement = "sqlite3_busy_timeout"; cause })
  in
  let* () =
    match Config.(config.cache_size) with
    | None -> Ok ()
    | Some cache_size ->
        run_sql_on_handle handle ("PRAGMA cache_size = " ^ Int.to_string cache_size)
  in
  match Config.(config.synchronous) with
  | None -> Ok ()
  | Some mode -> run_sql_on_handle handle ("PRAGMA synchronous = " ^ synchronous_sql mode)

let connect = fun config ->
  let* id =
    random_id ()
    |> Result.map_err ~fn:(fun reason -> Error.RandomFailure reason)
  in
  let path = Path.to_string Config.(config.path) in
  let* handle =
    Native.open_db path (open_flags config)
    |> Result.map_err ~fn:(fun cause -> Error.OpenFailed cause)
  in
  match apply_config handle config with
  | Ok () ->
      Ok {
        id;
        path = Config.(config.path);
        handle;
        statements = [];
        closed = false;
        in_transaction = false;
      }
  | Error error ->
      let _ = Native.close handle in
      Error error

let close = fun conn ->
  if not conn.closed then (
    List.for_each conn.statements ~fn:(fun stmt -> ignore (finalize_statement stmt));
    conn.statements <- [];
    conn.closed <- true;
    let _ = Native.close conn.handle in
    ()
  )

let ping = fun conn -> not conn.closed

let prepare = fun conn sql ->
  if conn.closed then
    Error Error.ConnectionClosed
  else
    Native.prepare conn.handle sql
    |> Result.map
      ~fn:(fun native ->
        let stmt = {
          sql;
          conn;
          native;
          finalized = false;
        }
        in
        conn.statements <- stmt :: conn.statements;
        stmt)
    |> Result.map_err ~fn:(fun cause -> Error.PrepareFailed { sql; cause })

let execute = fun stmt params ->
  if stmt.conn.closed then
    Error Error.ConnectionClosed
  else if stmt.finalized then
    Error (Error.FinalizeFailed { Native.code = 21; message = "SQLite statement is finalized" })
  else
    run_prepared stmt.conn stmt.sql stmt.native params

let fetch_row = fun result_set -> Queue.pop result_set.rows

let rows_affected = fun result_set -> result_set.rows_affected

let execute_simple = fun conn sql ->
  let* native =
    Native.prepare conn.handle sql
    |> Result.map_err ~fn:(fun cause -> Error.PrepareFailed { sql; cause })
  in
  let result =
    let* _result = run_prepared conn sql native [] in
    Ok ()
  in
  let finalize_result =
    Native.finalize native
    |> Result.map_err ~fn:(fun cause -> Error.FinalizeFailed cause)
  in
  match (result, finalize_result) with
  | (Ok (), Error error) -> Error error
  | _ -> result

let begin_transaction = fun conn ->
  if conn.closed then
    Error Error.ConnectionClosed
  else if conn.in_transaction then
    Error Error.TransactionAlreadyInProgress
  else
    let* () = execute_simple conn "BEGIN" in
    conn.in_transaction <- true;
  Ok ()

let commit = fun conn ->
  if conn.closed then
    Error Error.ConnectionClosed
  else if not conn.in_transaction then
    Error Error.NoTransactionInProgress
  else
    let* () = execute_simple conn "COMMIT" in
    conn.in_transaction <- false;
  Ok ()

let rollback = fun conn ->
  if conn.closed then
    Error Error.ConnectionClosed
  else if not conn.in_transaction then
    Error Error.NoTransactionInProgress
  else
    let* () = execute_simple conn "ROLLBACK" in
    conn.in_transaction <- false;
  Ok ()

let set_isolation_level = fun conn level ->
  if conn.closed then
    Error Error.ConnectionClosed
  else
    match level with
    | Sqlx_driver.Driver.ReadUncommitted -> execute_simple conn "PRAGMA read_uncommitted = 1"
    | Sqlx_driver.Driver.Serializable -> execute_simple conn "PRAGMA read_uncommitted = 0"
    | Sqlx_driver.Driver.ReadCommitted
    | Sqlx_driver.Driver.RepeatableRead ->
        Error (Error.UnsupportedOperation "SQLite supports read-uncommitted and serializable modes")
