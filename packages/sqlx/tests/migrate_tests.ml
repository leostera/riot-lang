open Std
open Sqlx

module Queue = Collections.Queue
module Vector = Collections.Vector
module M = Sqlx.Migrate

type fake_applied = {
  version: int64;
  checksum: string;
}

type fake_state = {
  applied: fake_applied Vector.t;
  executed: string Vector.t;
  transactions: string Vector.t;
}

let fake_state = fun () -> {
  applied = Vector.create ();
  executed = Vector.create ();
  transactions = Vector.create ();
}

let starts_with = fun prefix sql -> String.starts_with ~prefix sql

let remove_applied = fun state version ->
  let rec loop index =
    if index >= Vector.length state.applied then
      ()
    else
      let applied = Vector.get_unchecked state.applied ~at:index in
      if Int64.equal applied.version version then
        ignore (Vector.remove state.applied ~at:index)
      else
        loop (index + 1)
  in
  loop 0

module FakeDriver: Driver.Intf with type config = fake_state = struct
  type config = fake_state

  type connection = fake_state

  type statement = {
    conn: fake_state;
    sql: string;
  }

  type result_set = {
    rows: Row.t Queue.t;
    rows_affected: int;
  }

  type error = string

  let name = "Fake"

  let error_to_string = fun error -> error

  let error_to_json = fun error -> Data.Json.String error

  let connect = fun config -> Ok config

  let close = fun _ -> ()

  let ping = fun _ -> true

  let prepare = fun conn sql -> Ok { conn; sql }

  let rows = fun rows -> { rows = Queue.from_list rows; rows_affected = List.length rows }

  let execute = fun statement params ->
    Vector.push statement.conn.executed ~value:statement.sql;
    if starts_with "SELECT version FROM" statement.sql then
      Ok (rows [])
    else if starts_with "SELECT version, checksum FROM" statement.sql then
      let applied_rows =
        statement.conn.applied
        |> Vector.to_array
        |> Array.to_list
        |> List.map
          ~fn:(fun applied -> [
            ("version", Value.int64 applied.version);
            ("checksum", Value.string applied.checksum);
          ])
      in
      Ok (rows applied_rows)
    else if starts_with "INSERT INTO" statement.sql then (
      match params with
      | [ Value.Int64 version; _description; Value.String checksum ] ->
          Vector.push statement.conn.applied ~value:{ version; checksum };
          Ok { rows = Queue.create (); rows_affected = 1 }
      | _ -> Error "invalid insert params"
    ) else if starts_with "DELETE FROM" statement.sql then (
      match params with
      | [ Value.Int64 version ] ->
          remove_applied statement.conn version;
          Ok { rows = Queue.create (); rows_affected = 1 }
      | _ -> Error "invalid delete params"
    ) else
      Ok { rows = Queue.create (); rows_affected = 1 }

  let fetch_row = fun result -> Queue.pop result.rows

  let rows_affected = fun result -> result.rows_affected

  let begin_transaction = fun conn ->
    Vector.push conn.transactions ~value:"begin";
    Ok ()

  let commit = fun conn ->
    Vector.push conn.transactions ~value:"commit";
    Ok ()

  let rollback = fun conn ->
    Vector.push conn.transactions ~value:"rollback";
    Ok ()

  let set_isolation_level = fun _ _ -> Ok ()
end

let pool = fun state ->
  Sqlx.connect
    ~config:{ Sqlx.Config.default with pool_size = 1 }
    ~driver:(module FakeDriver)
    state

let require_pool = fun state ->
  match pool state with
  | Ok pool -> Ok pool
  | Error error -> Error (Sqlx.show_error error)

let require_migration = fun result ->
  match result with
  | Ok value -> Ok value
  | Error error -> Error (M.error_to_string error)

let write = fun path content ->
  match Fs.write content path with
  | Ok () -> Ok ()
  | Error error -> Error (IO.Error.message error)

let resolve_dir = fun dir ->
  M.Source.from_directory dir
  |> M.Source.resolve
  |> require_migration

let test_directory_source_resolves_sqlx_filenames = fun _ctx ->
  match Fs.with_tempdir
    (fun dir ->
      match write Path.(dir / Path.v "2_add_orders.up.sql") "CREATE TABLE orders (id BIGINT);" with
      | Error _ as error -> error
      | Ok () ->
          match write
            Path.(dir / Path.v "1_create_users.sql")
            "-- no-transaction\nCREATE TABLE users (id BIGINT);" with
          | Error _ as error -> error
          | Ok () -> resolve_dir dir) with
  | Error error -> Error (IO.Error.message error)
  | Ok (Error error) -> Error error
  | Ok (Ok migrations) ->
      Test.assert_equal ~expected:2 ~actual:(Vector.length migrations);
      let first = Vector.get_unchecked migrations ~at:0 in
      let second = Vector.get_unchecked migrations ~at:1 in
      Test.assert_equal ~expected:"1" ~actual:(M.Version.to_string M.Migration.(first.version));
      Test.assert_true M.Migration.(first.no_tx);
      Test.assert_equal ~expected:M.Simple ~actual:M.Migration.(first.migration_type);
      Test.assert_equal ~expected:"create users" ~actual:M.Migration.(first.description);
      Test.assert_equal ~expected:"2" ~actual:(M.Version.to_string M.Migration.(second.version));
      Test.assert_equal ~expected:M.ReversibleUp ~actual:M.Migration.(second.migration_type);
      Ok ()

let test_ignored_checksum_chars = fun _ctx ->
  let ignored = Vector.from_list [ ' '; '\n'; '\t'; '\r' ] in
  let config = M.Source.{ ignored_checksum_chars = ignored } in
  let version = M.Version.from_int64_unchecked 1L in
  match Fs.with_tempdir
    (fun dir ->
      match write Path.(dir / Path.v "1_one.sql") "CREATE\n TABLE things ( id BIGINT );" with
      | Error _ as error -> error
      | Ok () ->
          match M.Source.resolve ~config (M.Source.from_directory dir) with
          | Error error -> Error (M.error_to_string error)
          | Ok migrations -> Ok migrations) with
  | Error error -> Error (IO.Error.message error)
  | Ok (Error error) -> Error error
  | Ok (Ok migrations) ->
      let migration = Vector.get_unchecked migrations ~at:0 in
      let compact =
        M.Migration.make
          ~version
          ~description:"one"
          ~migration_type:M.Simple
          ~sql:"CREATETABLEthings(idBIGINT);"
          ()
      in
      Test.assert_equal
        ~expected:M.Migration.(compact.checksum)
        ~actual:M.Migration.(migration.checksum);
      Ok ()

let test_migrator_applies_pending_migrations = fun _ctx ->
  let state = fake_state () in
  match require_pool state with
  | Error _ as error -> error
  | Ok pool ->
      let version = M.Version.from_int64_unchecked 1L in
      let migration =
        M.Migration.make
          ~version
          ~description:"create users"
          ~migration_type:M.Simple
          ~sql:"CREATE TABLE users (id BIGINT);"
          ()
      in
      let source = M.Source.from_migrations (Vector.from_list [ migration ]) in
      match M.run pool source
      |> require_migration with
      | Error _ as error -> error
      | Ok report ->
          Test.assert_equal ~expected:1 ~actual:(Vector.length report.applied);
          Test.assert_equal ~expected:1 ~actual:(Vector.length state.applied);
          Test.assert_equal ~expected:2 ~actual:(Vector.length state.transactions);
          Sqlx.shutdown pool;
          Ok ()

let test_top_level_migrate_returns_unit = fun _ctx ->
  let state = fake_state () in
  match require_pool state with
  | Error _ as error -> error
  | Ok pool ->
      let version = M.Version.from_int64_unchecked 1L in
      let migration =
        M.Migration.make
          ~version
          ~description:"create users"
          ~migration_type:M.Simple
          ~sql:"CREATE TABLE users (id BIGINT);"
          ()
      in
      let source = M.Source.from_migrations (Vector.from_list [ migration ]) in
      (
        match Sqlx.migrate ~source pool () with
        | Error error ->
            Sqlx.shutdown pool;
            Error (M.error_to_string error)
        | Ok () ->
            Test.assert_equal ~expected:1 ~actual:(Vector.length state.applied);
            Sqlx.shutdown pool;
            Ok ()
      )

let test_migrator_rejects_modified_applied_migration = fun _ctx ->
  let state = fake_state () in
  Vector.push state.applied ~value:{ version = 1L; checksum = "old" };
  match require_pool state with
  | Error _ as error -> error
  | Ok pool ->
      let migration =
        M.Migration.make
          ~version:(M.Version.from_int64_unchecked 1L)
          ~description:"create users"
          ~migration_type:M.Simple
          ~sql:"CREATE TABLE users (id BIGINT);"
          ()
      in
      let source = M.Source.from_migrations (Vector.from_list [ migration ]) in
      let result = M.run pool source in
      Sqlx.shutdown pool;
      (
        match result with
        | Error (M.VersionMismatch version) ->
            Test.assert_equal ~expected:"1" ~actual:(M.Version.to_string version);
            Ok ()
        | Error error -> Error ("expected version mismatch, got: " ^ M.error_to_string error)
        | Ok _ -> Error "expected version mismatch"
      )

let tests =
  Test.[
    case "directory source resolves sqlx filenames" test_directory_source_resolves_sqlx_filenames;
    case "ignored checksum chars" test_ignored_checksum_chars;
    case "migrator applies pending migrations" test_migrator_applies_pending_migrations;
    case "top-level migrate returns unit" test_top_level_migrate_returns_unit;
    case
      "migrator rejects modified applied migration"
      test_migrator_rejects_modified_applied_migration;
  ]

let main ~args = Test.Cli.main ~name:"sqlx_migrate_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
