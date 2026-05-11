open Std

val bind_params: Sqlite__Native.stmt -> Sqlx_driver.Value.t list -> (unit, Sqlite__Error.t) result

val read_row: Sqlite__Native.stmt -> Sqlx_driver.Row.t
