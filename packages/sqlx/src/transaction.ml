open Std

type t = {
  connection: Connection.t;
}

type isolation_level = Sqlx_driver.Driver.isolation_level =
  | ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable

let begin_transaction = fun conn ->
  match Connection.begin_transaction conn with
  | Error _ as error -> error
  | Ok () -> Ok { connection = conn }

let commit = fun t -> Connection.commit t.connection

let rollback = fun t -> Connection.rollback t.connection

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

let set_isolation_level = fun conn level -> Connection.set_isolation_level conn level
