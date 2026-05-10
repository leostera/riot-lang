open Std
open Result.Syntax

module Ser = Serde.Ser

(** Error type that wraps driver errors with their conversion functions *)
type runtime_error =
  | RandomFailure of { label: string; reason: string }
  | RaisedException of string
  | InvalidConfiguration of string

type error =
  | DriverError: {
      error: 'err;
      to_string: 'err -> string;
      serializer: 'err Ser.t;
    } -> error
  | RuntimeError of runtime_error

(** Synchronous connection - executes SQL directly in caller's process *)
type t =
  | Connection: {
      id: string;
      driver_conn: 'connection;
      driver: (module Sqlx_driver.Driver.Intf with type connection = 'connection);
      created_at: Time.Instant.t;
      mutable last_used: Time.Instant.t;
      mutable pool_lease: int;
    } -> t

type config =
  | Config: {
      driver: (module Sqlx_driver.Driver.Intf with type config = 'config);
      config: 'config;
    } -> config

let random_error = fun label error ->
  RuntimeError (RandomFailure { label; reason = Random.error_to_string error })

let sample_random_int = fun label ->
  Random.int 1_000_000
  |> Result.map_err ~fn:(random_error label)

let gen_id = fun () ->
  let* left = sample_random_int "connection id" in
  let* right = sample_random_int "connection id" in
  Ok ("conn_" ^ Int.to_string left ^ "_" ^ Int.to_string right)

let exception_to_string = fun caught ->
  match caught with
  | Failure message -> "Failure: " ^ message
  | Invalid_argument message -> "Invalid_argument: " ^ message
  | Not_found -> "Not_found"
  | End_of_file -> "End_of_file"
  | Division_by_zero -> "Division_by_zero"
  | exn -> Exception.to_string exn

let runtime_error_to_string = fun error ->
  match error with
  | RandomFailure { label; reason } -> "failed to generate " ^ label ^ ": " ^ reason
  | RaisedException message -> message
  | InvalidConfiguration message -> message

let error_to_string = fun error ->
  match error with
  | DriverError { error; to_string; _ } -> to_string error
  | RuntimeError error -> "Runtime error: " ^ runtime_error_to_string error

type runtime_error_document = {
  type_: string;
  label: string option;
  reason: string option;
  message: string option;
}

let runtime_error_document = fun error ->
  match error with
  | RandomFailure { label; reason } ->
      {
        type_ = "random_failure";
        label = Some label;
        reason = Some reason;
        message = None;
      }
  | RaisedException message ->
      {
        type_ = "raised_exception";
        label = None;
        reason = None;
        message = Some message;
      }
  | InvalidConfiguration message ->
      {
        type_ = "invalid_configuration";
        label = None;
        reason = None;
        message = Some message;
      }

let runtime_error_serializer =
  Ser.contramap
    runtime_error_document
    (
      Ser.record
        (
          Ser.fields
            [
              Ser.field "type" Ser.string (fun (error: runtime_error_document) -> error.type_);
              Ser.field "label" (Ser.option Ser.string) (fun error -> error.label);
              Ser.field "reason" (Ser.option Ser.string) (fun error -> error.reason);
              Ser.field "message" (Ser.option Ser.string) (fun error -> error.message);
            ]
        )
    )

let error_serializer = {
  Ser.run =
    (fun backend state error ->
      match error with
      | DriverError { error; serializer; _ } -> serializer.run backend state error
      | RuntimeError error -> runtime_error_serializer.run backend state error);
}

(** Create a new connection - connects directly, no spawned process *)
let create = fun (Config { driver; config }) ->
  let module D = (val driver) in
  match gen_id () with
  | Error error -> Error error
  | Ok id -> (
      try
        match D.connect config with
        | Ok driver_conn ->
            Ok (
              Connection {
                id;
                driver_conn;
                driver = (module D);
                created_at = Time.Instant.now ();
                last_used = Time.Instant.now ();
                pool_lease = 0;
              }
            )
        | Error e ->
            Error (DriverError {
              error = e;
              to_string = D.error_to_string;
              serializer = D.error_serializer;
            })
      with
      | exn -> Error (RuntimeError (RaisedException (exception_to_string exn)))
    )

(** Close the underlying driver connection *)
let close = fun (Connection t) ->
  let module D = (val t.driver) in
  try D.close t.driver_conn with
  | _ -> ()

(** Query executes DIRECTLY in caller's process *)
let query = fun ((Connection t) as conn) sql params ->
  t.last_used <- Time.Instant.now ();
  let module D = (val t.driver) in
  try
    match D.prepare t.driver_conn sql with
    | Error e ->
        Error (DriverError {
          error = e;
          to_string = D.error_to_string;
          serializer = D.error_serializer;
        })
    | Ok stmt -> (
        match D.execute stmt params with
        | Error e ->
            Error (DriverError {
              error = e;
              to_string = D.error_to_string;
              serializer = D.error_serializer;
            })
        | Ok result_set -> (
            match sample_random_int "cursor id" with
            | Error error -> Error error
            | Ok random_id ->
                let cursor_id = "cursor_" ^ Int.to_string random_id in
                let cursor =
                  Cursor.make
                    cursor_id
                    result_set
                    (module D : Sqlx_driver.Driver.Intf with type result_set = D.result_set)
                in
                Ok cursor
          )
      )
  with
  | exn ->
      close conn;
      Error (RuntimeError (RaisedException (exception_to_string exn)))

(** Execute runs DIRECTLY in caller's process *)
let execute = fun ((Connection t) as conn) sql params ->
  t.last_used <- Time.Instant.now ();
  let module D = (val t.driver) in
  try
    match D.prepare t.driver_conn sql with
    | Error e ->
        Error (DriverError {
          error = e;
          to_string = D.error_to_string;
          serializer = D.error_serializer;
        })
    | Ok stmt -> (
        match D.execute stmt params with
        | Error e ->
            Error (DriverError {
              error = e;
              to_string = D.error_to_string;
              serializer = D.error_serializer;
            })
        | Ok result_set -> Ok (D.rows_affected result_set)
      )
  with
  | exn ->
      close conn;
      Error (RuntimeError (RaisedException (exception_to_string exn)))

(** Ping executes DIRECTLY in caller's process *)
let ping = fun (Connection t) ->
  let module D = (val t.driver) in
  try D.ping t.driver_conn with
  | _ -> false

let wrap_driver_result = fun
  (type err) (module D : Sqlx_driver.Driver.Intf with type error = err) result ->
  match result with
  | Ok value -> Ok value
  | Error error ->
      Error (DriverError { error; to_string = D.error_to_string; serializer = D.error_serializer })

let begin_transaction = fun (Connection t) ->
  let module D = (val t.driver) in
  try wrap_driver_result (module D) (D.begin_transaction t.driver_conn) with
  | exn -> Error (RuntimeError (RaisedException (exception_to_string exn)))

let commit = fun (Connection t) ->
  let module D = (val t.driver) in
  try wrap_driver_result (module D) (D.commit t.driver_conn) with
  | exn -> Error (RuntimeError (RaisedException (exception_to_string exn)))

let rollback = fun (Connection t) ->
  let module D = (val t.driver) in
  try wrap_driver_result (module D) (D.rollback t.driver_conn) with
  | exn -> Error (RuntimeError (RaisedException (exception_to_string exn)))

let set_isolation_level = fun (Connection t) level ->
  let module D = (val t.driver) in
  try wrap_driver_result (module D) (D.set_isolation_level t.driver_conn level) with
  | exn -> Error (RuntimeError (RaisedException (exception_to_string exn)))

let id = fun (Connection t) -> t.id

let created_at = fun (Connection t) -> t.created_at

let last_used = fun (Connection t) -> t.last_used

let pool_lease = fun (Connection t) -> t.pool_lease

let set_pool_lease = fun (Connection t) lease -> t.pool_lease <- lease
