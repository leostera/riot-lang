open Std

(** Database-agnostic error type that preserves structured error information *)
(** Structured database error with detailed information *)
type db_error = {
  code: string option;  (** Driver-specific error code (e.g., SQLSTATE for PostgreSQL) *)
  message: string;  (** Primary error message *)
  detail: string option;  (** Additional detail about the error *)
  hint: string option;  (** Hint for fixing the error *)
  constraint_name: string option;  (** Name of violated constraint *)
  table_name: string option;  (** Table involved in the error *)
  column_name: string option;  (** Column involved in the error *)
  position: int option;  (** Character position in query where error occurred *)
  context: string option;  (** Additional context information *)
}
(** Main error type representing different categories of database errors *)
type t =
  | Connection_error of { message: string; cause: db_error option; }
  | Query_error of { sql: string option; cause: db_error; }
  | Preparation_error of { sql: string; cause: db_error; }
  | Execution_error of { cause: db_error; }
  | Transaction_error of { message: string; cause: db_error option; }
  | Pool_error of string
  | Generic_error of string
(** {1 Constructors} *)
val of_string: string -> t
(** Create a generic error from a string message *)
val connection_error: message:string -> ?cause:db_error -> unit -> t
(** Create a connection error *)
val query_error: sql:string -> db_error -> t
(** Create a query error with the SQL that caused it *)
val preparation_error: sql:string -> db_error -> t
(** Create a statement preparation error *)
val execution_error: db_error -> t
(** Create a statement execution error *)
val transaction_error: message:string -> ?cause:db_error -> unit -> t
(** Create a transaction error *)
val pool_error: string -> t
(** Create a connection pool error *)
(** {1 Formatting} *)

val format_db_error: db_error -> string
(** Format a db_error record into a human-readable string *)
val to_string: t -> string
(** Convert error to a full human-readable string *)
(** {1 Error Inspection} *)

val get_db_error: t -> db_error option
(** Extract the underlying db_error if available *)
val is_constraint_violation: name:string -> t -> bool
(** Check if error is a specific constraint violation by name *)
val is_unique_violation: t -> bool
(** Check if error is a unique constraint violation *)
val is_foreign_key_violation: t -> bool
(** Check if error is a foreign key constraint violation *)
val is_not_null_violation: t -> bool
(** Check if error is a not-null constraint violation *)
