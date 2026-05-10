open Std

module Ser = Serde.Ser

module Config = struct
  type t = {
    path: Path.t;
    mode: [`ReadOnly | `ReadWrite | `Create];
    busy_timeout: Time.Duration.t option;
    cache_size: int option;
    synchronous: ([`Off | `Normal | `Full | `Extra]) option;
  }

  let default = fun path ->
    {
      path;
      mode = `Create;
      busy_timeout = Some (Time.Duration.from_secs 5);
      cache_size = None;
      synchronous = Some `Normal;
    }

  let in_memory = fun () ->
    {
      path = Path.v ":memory:";
      mode = `Create;
      busy_timeout = None;
      cache_size = None;
      synchronous = Some `Off;
    }
end

module Driver = struct
  type config = Config.t

  type connection = {
    id: string;
    path: Path.t;
    handle: unit;
    (* Will be replaced with actual SQLite handle via FFI *)
    mutable closed: bool;
  }

  type statement = {
    sql: string;
    conn: connection;
  }

  type result_set = {
    rows: Sqlx_driver.Row.t list;
    rows_affected: int;
  }

  type error =
    | ConnectionClosed
    | PrepareFailed of string
    | ExecutionFailed of string
    | UnsupportedOperation of string

  type error_document = { type_: string; message: string }

  let name = "SQLite"

  let error_to_string = fun __tmp1 ->
    match __tmp1 with
    | ConnectionClosed -> "Connection is closed"
    | PrepareFailed msg -> "Failed to prepare statement: " ^ msg
    | ExecutionFailed msg -> "Failed to execute statement: " ^ msg
    | UnsupportedOperation msg -> "Unsupported operation: " ^ msg

  let error_type = fun error ->
    match error with
    | ConnectionClosed -> "connection_closed"
    | PrepareFailed _ -> "prepare_failed"
    | ExecutionFailed _ -> "execution_failed"
    | UnsupportedOperation _ -> "unsupported_operation"

  let error_document = fun error -> {
    type_ = error_type error;
    message = error_to_string error;
  }

  let error_serializer =
    Ser.contramap
      error_document
      (
        Ser.record
          (
            Ser.fields
              [
                Ser.field "type" Ser.string (fun (error: error_document) -> error.type_);
                Ser.field "message" Ser.string (fun error -> error.message);
              ]
          )
      )

  let connect: config -> (connection, error) result = fun config ->
    let id =
      "sqlite_"
      ^ string_of_int
        (
          Random.int 1_000_000
          |> Result.expect ~msg:"failed to generate sqlite id"
        )
    in
    Ok {
      id;
      path = config.path;
      handle = ();
      closed = false;
    }

  let close = fun conn -> conn.closed <- true

  let ping = fun conn -> not conn.closed

  let prepare = fun conn sql ->
    if conn.closed then
      Error ConnectionClosed
    else
      Ok { sql; conn }

  let execute = fun _stmt _params ->
    (* TODO: Implement actual SQLite execution via FFI *)
    Ok { rows = []; rows_affected = 0 }

  let fetch_row = fun result_set ->
    match result_set.rows with
    | [] -> None
    | h :: _ -> Some h

  let rows_affected = fun result_set -> result_set.rows_affected

  let begin_transaction = fun _conn -> Ok ()

  let commit = fun _conn -> Ok ()

  let rollback = fun _conn -> Ok ()

  let set_isolation_level = fun _conn _level ->
    (* SQLite doesn't support standard isolation levels *)
    Error (UnsupportedOperation "SQLite does not support setting isolation levels")
end
