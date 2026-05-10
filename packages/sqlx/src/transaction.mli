open Std

type t
type isolation_level = Sqlx_driver.Driver.isolation_level =
  | ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable

val begin_transaction: Connection.t -> (t, Connection.error) result

val commit: t -> (unit, Connection.error) result

val rollback: t -> (unit, Connection.error) result

val with_transaction:
  Connection.t ->
  (Connection.t -> ('a, Connection.error) result) ->
  ('a, Connection.error) result

val set_isolation_level: Connection.t -> isolation_level -> (unit, Connection.error) result
