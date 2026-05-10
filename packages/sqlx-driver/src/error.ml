open Std

(* Database error type that can represent errors from any driver *)

type db_error = {
  code: string option;
  (* Driver-specific error code (e.g., SQLSTATE for PostgreSQL) *)
  message: string;
  (* Primary error message *)
  detail: string option;
  (* Additional detail *)
  hint: string option;
  (* Hint for fixing the error *)
  constraint_name: string option;
  (* Name of violated constraint *)
  table_name: string option;
  (* Table involved in error *)
  column_name: string option;
  (* Column involved in error *)
  position: int option;
  (* Character position in query *)
  context: string option;
  (* Additional context *)
}

type t =
  | Connection_error of {
      message: string;
      cause: db_error option;
    }
  | Query_error of {
      sql: string option;
      cause: db_error;
    }
  | Preparation_error of {
      sql: string;
      cause: db_error;
    }
  | Execution_error of {
      cause: db_error;
    }
  | Transaction_error of {
      message: string;
      cause: db_error option;
    }
  | Pool_error of string
  | Generic_error of string

(* Create a simple error from just a message *)

let from_string = fun msg -> Generic_error msg

(* Create connection error *)

let connection_error = fun ~message ?cause () -> Connection_error { message; cause }

(* Create query error *)

let query_error = fun ~sql cause -> Query_error { sql = Some sql; cause }

(* Create preparation error *)

let preparation_error = fun ~sql cause -> Preparation_error { sql; cause }

(* Create execution error *)

let execution_error = fun cause -> Execution_error { cause }

(* Create transaction error *)

let transaction_error = fun ~message ?cause () -> Transaction_error { message; cause }

(* Create pool error *)

let pool_error = fun msg -> Pool_error msg

(* Format db_error for display *)

let format_db_error = fun err ->
  let parts = [ "Database error: " ^ err.message ] in
  let parts =
    match err.code with
    | Some code -> parts @ [ "Error code: " ^ code ]
    | None -> parts
  in
  let parts =
    match err.detail with
    | Some d -> parts @ [ "Detail: " ^ d ]
    | None -> parts
  in
  let parts =
    match err.hint with
    | Some h -> parts @ [ "Hint: " ^ h ]
    | None -> parts
  in
  let parts =
    match err.constraint_name with
    | Some n -> parts @ [ "Constraint: " ^ n ]
    | None -> parts
  in
  let parts =
    match (err.table_name, err.column_name) with
    | (Some t, Some c) -> parts @ [ "Location: table \"" ^ t ^ "\", column \"" ^ c ^ "\"" ]
    | (Some t, None) -> parts @ [ "Table: \"" ^ t ^ "\"" ]
    | (None, Some c) -> parts @ [ "Column: \"" ^ c ^ "\"" ]
    | (None, None) -> parts
  in
  let parts =
    match err.position with
    | Some p -> parts @ [ "Position: " ^ string_of_int p ]
    | None -> parts
  in
  let parts =
    match err.context with
    | Some ctx -> parts @ [ "Context: " ^ ctx ]
    | None -> parts
  in
  String.concat "\n" parts

(* Format full error for display *)

let to_string = fun error ->
  match error with
  | Connection_error { message; cause = Some cause } ->
      "Connection failed: " ^ message ^ "\n" ^ format_db_error cause
  | Connection_error { message; cause = None } -> "Connection failed: " ^ message
  | Query_error { sql = Some sql; cause } -> "Query failed: " ^ sql ^ "\n" ^ format_db_error cause
  | Query_error { sql = None; cause } -> "Query failed\n" ^ format_db_error cause
  | Preparation_error { sql; cause } ->
      "Failed to prepare statement: " ^ sql ^ "\n" ^ format_db_error cause
  | Execution_error { cause } -> "Execution failed\n" ^ format_db_error cause
  | Transaction_error { message; cause = Some cause } ->
      "Transaction error: " ^ message ^ "\n" ^ format_db_error cause
  | Transaction_error { message; cause = None } -> "Transaction error: " ^ message
  | Pool_error msg -> "Pool error: " ^ msg
  | Generic_error msg -> msg

(* Extract the underlying db_error if available *)

let get_db_error = fun error ->
  match error with
  | Connection_error { cause = Some cause; _ } -> Some cause
  | Query_error { cause; _ } -> Some cause
  | Preparation_error { cause; _ } -> Some cause
  | Execution_error { cause } -> Some cause
  | Transaction_error { cause = Some cause; _ } -> Some cause
  | _ -> None

(* Check if error is a specific constraint violation by name *)

let is_constraint_violation = fun ~name ->
  fun error ->
    match error with
    | Query_error { cause; _ }
    | Execution_error { cause }
    | Preparation_error { cause; _ } -> (
        match cause.constraint_name with
        | Some n -> n = name
        | None -> false
      )
    | _ -> false

(* Check if error is a unique violation *)

let is_unique_violation = fun err ->
  match get_db_error err with
  | Some cause -> (
      match cause.code with
      | Some "23505" -> true
      | _ -> false
    )
  | None -> false

(* Check if error is a foreign key violation *)

let is_foreign_key_violation = fun err ->
  match get_db_error err with
  | Some cause -> (
      match cause.code with
      | Some "23503" -> true
      | _ -> false
    )
  | None -> false

(* Check if error is a not null violation *)

let is_not_null_violation = fun err ->
  match get_db_error err with
  | Some cause -> (
      match cause.code with
      | Some "23502" -> true
      | _ -> false
    )
  | None -> false
