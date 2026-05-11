open Std

module Native = Sqlite__Native
module Ser = Serde.Ser

type t =
  | ConnectionClosed
  | RandomFailure of string
  | OpenFailed of Native.error
  | ConfigurationFailed of {
      statement: string;
      cause: Native.error;
    }
  | PrepareFailed of {
      sql: string;
      cause: Native.error;
    }
  | BindFailed of {
      index: int;
      cause: Native.error;
    }
  | ParameterCountMismatch of { expected: int; actual: int }
  | ExecutionFailed of {
      sql: string;
      cause: Native.error;
    }
  | ResetFailed of Native.error
  | FinalizeFailed of Native.error
  | TransactionAlreadyInProgress
  | NoTransactionInProgress
  | UnsupportedOperation of string

type document = {
  type_: string;
  code: int option;
  message: string;
  sql: string option;
  index: int option;
  expected_params: int option;
  actual_params: int option;
}

let native_message = fun (error: Native.error) -> error.message

let to_string = fun error ->
  match error with
  | ConnectionClosed -> "Connection is closed"
  | RandomFailure reason -> "Failed to generate SQLite identifier: " ^ reason
  | OpenFailed cause -> "Failed to open SQLite database: " ^ native_message cause
  | ConfigurationFailed { statement; cause } ->
      "Failed to configure SQLite database with " ^ statement ^ ": " ^ native_message cause
  | PrepareFailed { sql; cause } ->
      "Failed to prepare SQLite statement " ^ sql ^ ": " ^ native_message cause
  | BindFailed { index; cause } ->
      "Failed to bind SQLite parameter " ^ Int.to_string index ^ ": " ^ native_message cause
  | ParameterCountMismatch { expected; actual } ->
      "SQLite statement expected "
      ^ Int.to_string expected
      ^ " parameters, got "
      ^ Int.to_string actual
  | ExecutionFailed { sql; cause } ->
      "Failed to execute SQLite statement " ^ sql ^ ": " ^ native_message cause
  | ResetFailed cause -> "Failed to reset SQLite statement: " ^ native_message cause
  | FinalizeFailed cause -> "Failed to finalize SQLite statement: " ^ native_message cause
  | TransactionAlreadyInProgress -> "SQLite transaction already in progress"
  | NoTransactionInProgress -> "SQLite transaction is not in progress"
  | UnsupportedOperation msg -> "Unsupported SQLite operation: " ^ msg

let document = fun ?sql ?index ?expected_params ?actual_params type_ code message ->
  {
    type_;
    code;
    message;
    sql;
    index;
    expected_params;
    actual_params;
  }

let document_native = fun ?sql ?index type_ (cause: Native.error) ->
  document
    ?sql
    ?index
    type_
    (Some cause.code)
    cause.message

let to_document = fun error ->
  match error with
  | ConnectionClosed -> document "connection_closed" None "Connection is closed"
  | RandomFailure reason -> document "random_failure" None reason
  | OpenFailed cause -> document_native "open_failed" cause
  | ConfigurationFailed { statement; cause } ->
      document_native ~sql:statement "configuration_failed" cause
  | PrepareFailed { sql; cause } -> document_native ~sql "prepare_failed" cause
  | BindFailed { index; cause } -> document_native ~index "bind_failed" cause
  | ParameterCountMismatch { expected; actual } ->
      document
        ~expected_params:expected
        ~actual_params:actual
        "parameter_count_mismatch"
        None
        (to_string error)
  | ExecutionFailed { sql; cause } -> document_native ~sql "execution_failed" cause
  | ResetFailed cause -> document_native "reset_failed" cause
  | FinalizeFailed cause -> document_native "finalize_failed" cause
  | TransactionAlreadyInProgress ->
      document "transaction_already_in_progress" None "SQLite transaction already in progress"
  | NoTransactionInProgress ->
      document "no_transaction_in_progress" None "SQLite transaction is not in progress"
  | UnsupportedOperation msg -> document "unsupported_operation" None msg

let serializer =
  Ser.contramap
    to_document
    (
      Ser.record
        (
          Ser.fields
            [
              Ser.field "type" Ser.string (fun (error: document) -> error.type_);
              Ser.field "code" (Ser.option Ser.int) (fun error -> error.code);
              Ser.field "message" Ser.string (fun error -> error.message);
              Ser.field "sql" (Ser.option Ser.string) (fun error -> error.sql);
              Ser.field "index" (Ser.option Ser.int) (fun error -> error.index);
              Ser.field "expected_params" (Ser.option Ser.int) (fun error -> error.expected_params);
              Ser.field "actual_params" (Ser.option Ser.int) (fun error -> error.actual_params);
            ]
        )
    )
