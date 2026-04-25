open Std

(** Error type that wraps driver errors with their conversion functions *)
type error =
  | DriverError : { error: 'err; to_string: 'err -> string; to_json: 'err -> Data.Json.t } -> error

(** Synchronous connection - executes SQL directly in caller's process *)
type t =
  | Connection : {
    id: string;
    driver_conn: 'connection;
    driver: (module Sqlx_driver.Driver.Intf with type connection = 'connection);
    created_at: Time.Instant.t;
    mutable last_used: Time.Instant.t;
  } -> t

type config =
  | Config : {
    driver: (module Sqlx_driver.Driver.Intf with type config = 'config);
    config: 'config;
  } -> config

let sample_random_int = fun () -> Random.int 1_000_000 |> Result.expect ~msg:"failed to generate random connection id"

let gen_id = fun () -> "conn_" ^ string_of_int (sample_random_int ()) ^ "_" ^ string_of_int (sample_random_int ())

(** Create a new connection - connects directly, no spawned process *)
let create = fun (Config { driver; config }) ->
  let module D = (val driver) in
  let id = gen_id () in
  match D.connect config with
  | Ok driver_conn ->
      Ok (
        Connection {
          id;
          driver_conn;
          driver = (module D);
          created_at = Time.Instant.now ();
          last_used = Time.Instant.now ()
        }
      )
  | Error e -> Error (DriverError { error = e; to_string = D.error_to_string; to_json = D.error_to_json })

(** Query executes DIRECTLY in caller's process *)
let query = fun (Connection t) sql params ->
  t.last_used <- Time.Instant.now ();
  let module D = (val t.driver) in
  match D.prepare t.driver_conn sql with
  | Error e -> Error (DriverError { error = e; to_string = D.error_to_string; to_json = D.error_to_json })
  | Ok stmt -> (
    match D.execute stmt params with
    | Error e -> Error (DriverError { error = e; to_string = D.error_to_string; to_json = D.error_to_json })
    | Ok result_set ->
        let cursor_id = "cursor_" ^ string_of_int (sample_random_int ()) in
        let cursor = Cursor.make cursor_id result_set (module D : Sqlx_driver.Driver.Intf with type result_set = D.result_set) in Ok cursor
  )

(** Execute runs DIRECTLY in caller's process *)
let execute = fun (Connection t) sql params ->
  t.last_used <- Time.Instant.now ();
  let module D = (val t.driver) in
  match D.prepare t.driver_conn sql with
  | Error e -> Error (DriverError { error = e; to_string = D.error_to_string; to_json = D.error_to_json })
  | Ok stmt -> (
    match D.execute stmt params with
    | Error e -> Error (DriverError { error = e; to_string = D.error_to_string; to_json = D.error_to_json })
    | Ok result_set -> Ok (D.rows_affected result_set)
  )

(** Ping executes DIRECTLY in caller's process *)
let ping = fun (Connection t) ->
  let module D = (val t.driver) in
  D.ping t.driver_conn

(** Close the underlying driver connection *)
let close = fun (Connection t) ->
  let module D = (val t.driver) in
  D.close t.driver_conn

let id = fun (Connection t) -> t.id

let created_at = fun (Connection t) -> t.created_at

let last_used = fun (Connection t) -> t.last_used
