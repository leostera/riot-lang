open Std

type t
type isolation_level =
[
  `Read_uncommitted
  | `Read_committed
  | `Repeatable_read
  | `Serializable
]
val begin_transaction : Connection.t -> (t, string) result

val commit : t -> (unit, string) result

val rollback : t -> (unit, string) result

val with_transaction : Connection.t -> (Connection.t -> ('a, 'e) result) -> ('a, 'e) result

val set_isolation_level : Connection.t -> isolation_level -> (unit, string) result
