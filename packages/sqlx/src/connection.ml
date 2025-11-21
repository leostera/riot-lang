open Std

(** Synchronous connection - executes SQL directly in caller's process *)
type t =
  | Connection : {
      id : string;
      driver_conn : 'connection;  (* Raw driver connection *)
      driver : (module Sqlx_driver.Driver.Intf with type connection = 'connection);
      created_at : Time.Instant.t;
      mutable last_used : Time.Instant.t;
    }
      -> t

type config =
  | Config : {
      driver : (module Sqlx_driver.Driver.Intf with type config = 'config);
      config : 'config;
    }
      -> config

let gen_id () =
  "conn_" ^ string_of_int (Random.int 1000000) ^ "_" ^ string_of_int (Random.int 1000000)

(** Create a new connection - connects directly, no spawned process *)
let create (Config { driver; config }) =
  let module D = (val driver) in
  let id = gen_id () in
  match D.connect config with
  | Ok driver_conn ->
      Ok (Connection {
        id;
        driver_conn;
        driver = (module D);
        created_at = Time.Instant.now ();
        last_used = Time.Instant.now ();
      })
  | Error e -> Error e

(** Query executes DIRECTLY in caller's process *)
let query (Connection t) sql params =
  t.last_used <- Time.Instant.now ();
  let module D = (val t.driver) in
  match D.prepare t.driver_conn sql with
  | Error e -> Error e
  | Ok stmt -> (
      match D.execute stmt params with
      | Error e -> Error e
      | Ok result_set ->
          let cursor_id =
            "cursor_" ^ string_of_int (Random.int 1000000)
          in
          let cursor =
            Cursor.make cursor_id result_set
              (module D : Sqlx_driver.Driver.Intf
                with type result_set = D.result_set)
          in
          Ok cursor)

(** Execute runs DIRECTLY in caller's process *)
let execute (Connection t) sql params =
  t.last_used <- Time.Instant.now ();
  let module D = (val t.driver) in
  match D.prepare t.driver_conn sql with
  | Error e -> Error e
  | Ok stmt -> (
      match D.execute stmt params with
      | Error e -> Error e
      | Ok result_set -> Ok (D.rows_affected result_set))

(** Ping executes DIRECTLY in caller's process *)
let ping (Connection t) =
  let module D = (val t.driver) in
  D.ping t.driver_conn

(** Close the underlying driver connection *)
let close (Connection t) =
  let module D = (val t.driver) in
  D.close t.driver_conn

let id (Connection t) = t.id
let created_at (Connection t) = t.created_at
let last_used (Connection t) = t.last_used
