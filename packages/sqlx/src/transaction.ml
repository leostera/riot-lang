open Std
open Miniriot

type t = { connection : Connection.t }

type isolation_level =
  [ `Read_uncommitted | `Read_committed | `Repeatable_read | `Serializable ]

(* TODO(@leostera): Implement actual transaction support by adding transaction messages to Connection *)

let begin_transaction conn = Ok { connection = conn }
let commit _t = Ok ()
let rollback _t = Ok ()

let with_transaction conn f =
  match begin_transaction conn with
  | Error e -> Error e
  | Ok txn -> (
      match f conn with
      | Ok result -> (
          match commit txn with
          | Ok () -> Ok result
          | Error e ->
              let _ = rollback txn in
              Error e)
      | Error e ->
          let _ = rollback txn in
          Error e)

let set_isolation_level _conn _level = Ok ()
