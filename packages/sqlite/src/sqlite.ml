open Std

module Config = struct
  type t = {
    path : Path.t;
    mode : [ `ReadOnly | `ReadWrite | `Create ];
    busy_timeout : Time.Duration.t option;
    cache_size : int option;
    synchronous : [ `Off | `Normal | `Full | `Extra ] option;
  }

  let default path =
    {
      path;
      mode = `Create;
      busy_timeout = Some (Time.Duration.from_secs 5);
      cache_size = None;
      synchronous = Some `Normal;
    }

  let in_memory () =
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
    id : string;
    path : Path.t;
    handle : unit; (* Will be replaced with actual SQLite handle via FFI *)
    mutable closed : bool;
  }

  type statement = { sql : string; conn : connection }
  type result_set = { rows : Sqlx_driver.Row.t list; rows_affected : int }

  let name = "SQLite"

  let connect (config : config) : (connection, string) result =
    let id = Printf.sprintf "sqlite_%d" (Random.int 1000000) in
    Ok { id; path = config.path; handle = (); closed = false }

  let close conn = conn.closed <- true
  let ping conn = not conn.closed

  let prepare conn sql =
    if conn.closed then Error "Connection is closed" else Ok { sql; conn }

  let execute _stmt _params =
    (* TODO: Implement actual SQLite execution via FFI *)
    Ok { rows = []; rows_affected = 0 }

  let fetch_row result_set =
    match result_set.rows with [] -> None | h :: _ -> Some h

  let rows_affected result_set = result_set.rows_affected
  let begin_transaction _conn = Ok ()
  let commit _conn = Ok ()
  let rollback _conn = Ok ()

  let set_isolation_level _conn _level =
    (* SQLite doesn't support standard isolation levels *)
    Error "SQLite does not support setting isolation levels"
end
