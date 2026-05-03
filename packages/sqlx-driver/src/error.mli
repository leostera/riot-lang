open Std

(** Database-agnostic error values with structured driver details. *)

(** Structured database error with detailed information. *)
type db_error = {
  (** Driver-specific error code (e.g., SQLSTATE for PostgreSQL) *)
  code: string option;
  (** Primary error message *)
  message: string;
  (** Additional detail about the error *)
  detail: string option;
  (** Hint for fixing the error *)
  hint: string option;
  (** Name of violated constraint *)
  constraint_name: string option;
  (** Table involved in the error *)
  table_name: string option;
  (** Column involved in the error *)
  column_name: string option;
  (** Character position in query where error occurred *)
  position: int option;
  (** Additional context information *)
  context: string option;
}
(** Main error type representing different categories of database errors. *)
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

(** Create a generic error from a string message. *)
val of_string: string -> t

(** Create a connection error. *)
val connection_error: message:string -> ?cause:db_error -> unit -> t

(** Create a query error with the SQL that caused it. *)
val query_error: sql:string -> db_error -> t

(** Create a statement preparation error. *)
val preparation_error: sql:string -> db_error -> t

(** Create a statement execution error. *)
val execution_error: db_error -> t

(** Create a transaction error. *)
val transaction_error: message:string -> ?cause:db_error -> unit -> t

(** Create a connection pool error. *)
val pool_error: string -> t

(** Format a `db_error` record into a human-readable string. *)
val format_db_error: db_error -> string

(** Convert error to a full human-readable string. *)
val to_string: t -> string

(** Extract the underlying `db_error` if available. *)
val get_db_error: t -> db_error option

(** Check if error is a specific constraint violation by name. *)
val is_constraint_violation: name:string -> t -> bool

(** Check if error is a unique constraint violation. *)
val is_unique_violation: t -> bool

(** Check if error is a foreign key constraint violation. *)
val is_foreign_key_violation: t -> bool

(** Check if error is a not-null constraint violation. *)
val is_not_null_violation: t -> bool
