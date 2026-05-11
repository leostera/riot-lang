open Std

type db

type stmt

type error = { code: int; message: string }

external unsafe_flag_readonly: unit -> int = "riot_sqlite_flag_readonly"

external unsafe_flag_readwrite: unit -> int = "riot_sqlite_flag_readwrite"

external unsafe_flag_create: unit -> int = "riot_sqlite_flag_create"

external unsafe_flag_uri: unit -> int = "riot_sqlite_flag_uri"

external unsafe_row_code: unit -> int = "riot_sqlite_row_code"

external unsafe_done_code: unit -> int = "riot_sqlite_done_code"

external unsafe_open_db: string -> int -> (db, error) result = "riot_sqlite_open"

external unsafe_close: db -> (unit, error) result = "riot_sqlite_close"

external unsafe_busy_timeout: db -> int -> (unit, error) result = "riot_sqlite_busy_timeout"

external unsafe_prepare: db -> string -> (stmt, error) result = "riot_sqlite_prepare"

external unsafe_finalize: stmt -> (unit, error) result = "riot_sqlite_finalize"

external unsafe_reset: stmt -> (unit, error) result = "riot_sqlite_reset"

external unsafe_clear_bindings: stmt -> (unit, error) result = "riot_sqlite_clear_bindings"

external unsafe_bind_parameter_count: stmt -> int = "riot_sqlite_bind_parameter_count"

external unsafe_bind_null: stmt -> int -> (unit, error) result = "riot_sqlite_bind_null"

external unsafe_bind_int64: stmt -> int -> int64 -> (unit, error) result = "riot_sqlite_bind_int64"

external unsafe_bind_double: stmt -> int -> float -> (unit, error) result =
  "riot_sqlite_bind_double"

external unsafe_bind_text: stmt -> int -> string -> (unit, error) result = "riot_sqlite_bind_text"

external unsafe_bind_blob: stmt -> int -> bytes -> (unit, error) result = "riot_sqlite_bind_blob"

external unsafe_step: stmt -> (int, error) result = "riot_sqlite_step"

external unsafe_column_count: stmt -> int = "riot_sqlite_column_count"

external unsafe_column_name: stmt -> int -> string = "riot_sqlite_column_name"

external unsafe_column_type: stmt -> int -> int = "riot_sqlite_column_type"

external unsafe_column_int64: stmt -> int -> int64 = "riot_sqlite_column_int64"

external unsafe_column_double: stmt -> int -> float = "riot_sqlite_column_double"

external unsafe_column_text: stmt -> int -> string = "riot_sqlite_column_text"

external unsafe_column_blob: stmt -> int -> bytes = "riot_sqlite_column_blob"

external unsafe_changes: db -> int = "riot_sqlite_changes"

external unsafe_stmt_readonly: stmt -> bool = "riot_sqlite_stmt_readonly"

let flag_readonly = unsafe_flag_readonly

let flag_readwrite = unsafe_flag_readwrite

let flag_create = unsafe_flag_create

let flag_uri = unsafe_flag_uri

let row_code = unsafe_row_code

let done_code = unsafe_done_code

let open_db = unsafe_open_db

let close = unsafe_close

let busy_timeout = unsafe_busy_timeout

let prepare = unsafe_prepare

let finalize = unsafe_finalize

let reset = unsafe_reset

let clear_bindings = unsafe_clear_bindings

let bind_parameter_count = unsafe_bind_parameter_count

let bind_null = unsafe_bind_null

let bind_int64 = unsafe_bind_int64

let bind_double = unsafe_bind_double

let bind_text = unsafe_bind_text

let bind_blob = unsafe_bind_blob

let step = unsafe_step

let column_count = unsafe_column_count

let column_name = unsafe_column_name

let column_type = unsafe_column_type

let column_int64 = unsafe_column_int64

let column_double = unsafe_column_double

let column_text = unsafe_column_text

let column_blob = unsafe_column_blob

let changes = unsafe_changes

let stmt_readonly = unsafe_stmt_readonly

let sqlite_integer = 1

let sqlite_float = 2

let sqlite_text = 3

let sqlite_blob = 4

let sqlite_null = 5
