open Std
open Miniriot

type t =
  | Connection : {
      id : string;
      pid : Pid.t;
      driver : (module Sqlx_driver.Driver.Intf with type config = 'cfg);
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

type conn_msg =
  | Query of string * Sqlx_driver.Value.t list * Pid.t
  | Execute of string * Sqlx_driver.Value.t list * Pid.t
  | Ping of Pid.t
  | Close

type Message.t += ConnMsg of conn_msg

type conn_response =
  | QueryResult of (Cursor.t, string) result
  | ExecResult of (int, string) result
  | PingResult of bool

type Message.t += ConnResponse of conn_response

let gen_id () =
  Printf.sprintf "conn_%d_%d" (Random.int 1000000) (Random.int 1000000)

let connection_process (type cfg) config_data =
  let (Config { driver; config } : config) = config_data in
  let module D = (val driver) in
  let rec loop conn =
    let selector msg =
      match msg with ConnMsg msg -> `select msg | _ -> `skip
    in
    match receive ~selector () with
    | Query (sql, params, reply_to) ->
        let result =
          match D.prepare conn sql with
          | Error e -> Error e
          | Ok stmt -> (
              match D.execute stmt params with
              | Error e -> Error e
              | Ok result_set ->
                  let cursor_id =
                    Printf.sprintf "cursor_%d" (Random.int 1000000)
                  in
                  let cursor =
                    Cursor.make cursor_id result_set
                      (module D : Sqlx_driver.Driver.Intf
                        with type result_set = D.result_set)
                  in
                  Ok cursor)
        in
        send reply_to (ConnResponse (QueryResult result));
        loop conn
    | Execute (sql, params, reply_to) ->
        let result =
          match D.prepare conn sql with
          | Error e -> Error e
          | Ok stmt -> (
              match D.execute stmt params with
              | Error e -> Error e
              | Ok result_set -> Ok (D.rows_affected result_set))
        in
        send reply_to (ConnResponse (ExecResult result));
        loop conn
    | Ping reply_to ->
        let alive = D.ping conn in
        send reply_to (ConnResponse (PingResult alive));
        loop conn
    | Close ->
        D.close conn;
        ()
  in

  match D.connect config with
  | Ok conn ->
      loop conn;
      Ok ()
  | Error e ->
      Log.error "Failed to connect to database: %s" e;
      Error (Failure e)

let create (Config { driver; config } as cfg) =
  let id = gen_id () in
  let pid = spawn (fun () -> connection_process cfg) in
  Ok
    (Connection
       {
         id;
         pid;
         driver;
         created_at = Time.Instant.now ();
         last_used = Time.Instant.now ();
       })

let query (Connection t) sql params =
  t.last_used <- Time.Instant.now ();
  send t.pid (ConnMsg (Query (sql, params, self ())));
  let selector msg =
    match msg with ConnResponse (QueryResult r) -> `select r | _ -> `skip
  in
  receive ~selector ()

let execute (Connection t) sql params =
  t.last_used <- Time.Instant.now ();
  send t.pid (ConnMsg (Execute (sql, params, self ())));
  let selector msg =
    match msg with ConnResponse (ExecResult r) -> `select r | _ -> `skip
  in
  receive ~selector ()

let ping (Connection t) =
  send t.pid (ConnMsg (Ping (self ())));
  let selector msg =
    match msg with ConnResponse (PingResult r) -> `select r | _ -> `skip
  in
  receive ~selector ()

let close (Connection t) = send t.pid (ConnMsg Close)
let id (Connection t) = t.id
let created_at (Connection t) = t.created_at
let last_used (Connection t) = t.last_used
