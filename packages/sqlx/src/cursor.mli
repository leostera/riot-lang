open Std

type t

val make :
  string ->
  'rs ->
  (module Sqlx_driver.Driver.Intf with type result_set = 'rs) ->
  t

val fetch_one : t -> Sqlx_driver.Row.t option
val fetch_many : t -> int -> Sqlx_driver.Row.t list
val fetch_all : t -> Sqlx_driver.Row.t list
val id : t -> string
val row_count : t -> int
val is_exhausted : t -> bool
