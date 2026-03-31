open Std

type t = {
  connection : Connection.t;
}

type isolation_level =
[
  `Read_uncommitted
  | `Read_committed
  | `Repeatable_read
  | `Serializable
]

(* TODO(@leostera): Implement actual transaction support by adding transaction messages to Connection *)

let begin_transaction = fun conn -> Ok {connection = conn}

let commit = fun _t -> Ok ()

let rollback = fun _t -> Ok ()

let with_transaction = fun conn f ->
  match begin_transaction conn with
  | Error e -> Error e
  | Ok txn -> (
      match f conn with
      | Ok result -> (
          match commit txn with
          | Ok () -> Ok result
          | Error e ->
              let _ = rollback txn in
              Error e
        )
      | Error e ->
          let _ = rollback txn in
          Error e
    )

let set_isolation_level = fun _conn _level -> Ok ()
