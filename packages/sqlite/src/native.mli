open Std

type db
type stmt
type error = { code: int; message: string }

val flag_readonly: unit -> int

val flag_readwrite: unit -> int

val flag_create: unit -> int

val flag_uri: unit -> int

val row_code: unit -> int

val done_code: unit -> int

val open_db: string -> int -> (db, error) result

val close: db -> (unit, error) result

val busy_timeout: db -> int -> (unit, error) result

val prepare: db -> string -> (stmt, error) result

val finalize: stmt -> (unit, error) result

val reset: stmt -> (unit, error) result

val clear_bindings: stmt -> (unit, error) result

val bind_parameter_count: stmt -> int

val bind_null: stmt -> int -> (unit, error) result

val bind_int64: stmt -> int -> int64 -> (unit, error) result

val bind_double: stmt -> int -> float -> (unit, error) result

val bind_text: stmt -> int -> string -> (unit, error) result

val bind_blob: stmt -> int -> bytes -> (unit, error) result

val step: stmt -> (int, error) result

val column_count: stmt -> int

val column_name: stmt -> int -> string

val column_type: stmt -> int -> int

val column_int64: stmt -> int -> int64

val column_double: stmt -> int -> float

val column_text: stmt -> int -> string

val column_blob: stmt -> int -> bytes

val changes: db -> int

val stmt_readonly: stmt -> bool

val sqlite_integer: int

val sqlite_float: int

val sqlite_text: int

val sqlite_blob: int

val sqlite_null: int
