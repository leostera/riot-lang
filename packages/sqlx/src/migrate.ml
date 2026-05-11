open Std

module Vector = Collections.Vector
module Row = Sqlx_driver.Row
module Value = Sqlx_driver.Value

module Version = struct
  type t = int64

  type error =
    | NotPositive of int64
    | InvalidInteger of string
    | ExpectedIntegerValue

  let from_int64 = fun value ->
    if Int64.compare value 0L = Order.GT then
      Ok value
    else
      Error (NotPositive value)

  let from_int = fun value -> from_int64 (Int64.from_int value)

  let from_string = fun value ->
    match Int64.from_string_opt (String.trim value) with
    | Some version -> from_int64 version
    | None -> Error (InvalidInteger value)

  let from_int64_unchecked = fun value ->
    match from_int64 value with
    | Ok version -> version
    | Error error ->
        panic
          (
            match error with
            | NotPositive _ -> "migration version must be greater than zero"
            | InvalidInteger value -> "invalid migration version: " ^ value
            | ExpectedIntegerValue -> "expected integer migration version"
          )

  let to_int64 = fun value -> value

  let to_string = Int64.to_string

  let equal = Int64.equal

  let compare = Int64.compare

  let error_to_string = fun error ->
    match error with
    | NotPositive _ -> "migration version must be greater than zero"
    | InvalidInteger value -> "invalid migration version: " ^ value
    | ExpectedIntegerValue -> "expected integer migration version"
end

module TableName = struct
  type t = string

  type error =
    | Empty
    | InvalidIdentifier of string

  let char_between = fun char lower upper ->
    Char.compare char lower != Order.LT && Char.compare char upper != Order.GT

  let is_ident_start = fun char ->
    Char.equal char '_' || char_between char 'a' 'z' || char_between char 'A' 'Z'

  let is_ident_char = fun char -> is_ident_start char || char_between char '0' '9'

  let valid_part = fun part ->
    let length = String.length part in
    if length = 0 then
      false
    else if not (is_ident_start (String.get_unchecked part ~at:0)) then
      false
    else
      let rec loop index =
        if index >= length then
          true
        else if is_ident_char (String.get_unchecked part ~at:index) then
          loop (index + 1)
        else
          false
      in
      loop 1

  let from_string = fun value ->
    let value = String.trim value in
    if String.is_empty value then
      Error Empty
    else
      let parts = String.split ~by:"." value in
      if List.all parts ~fn:valid_part then
        Ok value
      else
        Error (InvalidIdentifier value)

  let from_string_unchecked = fun value ->
    match from_string value with
    | Ok table_name -> table_name
    | Error error ->
        panic
          (
            match error with
            | Empty -> "migration table name must not be empty"
            | InvalidIdentifier value -> "invalid migration table name: " ^ value
          )

  let default = from_string_unchecked "_sqlx_migrations"

  let to_string = fun value -> value

  let error_to_string = fun error ->
    match error with
    | Empty -> "migration table name must not be empty"
    | InvalidIdentifier value -> "invalid migration table name: " ^ value
end

type migration_type =
  | Simple
  | ReversibleUp
  | ReversibleDown

let migration_type_to_string = fun migration_type ->
  match migration_type with
  | Simple -> "simple"
  | ReversibleUp -> "up"
  | ReversibleDown -> "down"

let migration_type_suffix = fun migration_type ->
  match migration_type with
  | Simple -> ".sql"
  | ReversibleUp -> ".up.sql"
  | ReversibleDown -> ".down.sql"

let migration_type_from_filename = fun filename ->
  if String.ends_with ~suffix:(migration_type_suffix ReversibleUp) filename then
    ReversibleUp
  else if String.ends_with ~suffix:(migration_type_suffix ReversibleDown) filename then
    ReversibleDown
  else
    Simple

let is_up_migration = fun migration_type ->
  match migration_type with
  | Simple
  | ReversibleUp -> true
  | ReversibleDown -> false

let is_down_migration = fun migration_type ->
  match migration_type with
  | ReversibleDown -> true
  | Simple
  | ReversibleUp -> false

let checksum = fun sql ->
  Crypto.Sha256.hash_string sql
  |> Crypto.Digest.hex

let no_transaction_directive = "-- no-transaction"

let strip_no_transaction_directive = fun sql ->
  if String.starts_with ~prefix:no_transaction_directive sql then
    let offset = String.length no_transaction_directive in
    let len = String.length sql - offset in
    let rest = String.sub sql ~offset ~len in
    if String.starts_with ~prefix:"\r\n" rest then
      String.sub rest ~offset:2 ~len:(String.length rest - 2)
    else if String.starts_with ~prefix:"\n" rest then
      String.sub rest ~offset:1 ~len:(String.length rest - 1)
    else
      rest
  else
    sql

let checksum_ignoring = fun ignored sql ->
  let sql = strip_no_transaction_directive sql in
  if Vector.is_empty ignored then
    checksum sql
  else
    let is_ignored char =
      let found = ref false in
      Vector.for_each
        ignored
        ~fn:(fun ignored_char ->
          if Char.equal char ignored_char then
            found := true);
      !found
    in
    let buffer = IO.Buffer.create ~size:(String.length sql) in
    String.for_each
      sql
      ~fn:(fun char ->
        if not (is_ignored char) then
          IO.Buffer.add_char buffer char);
  checksum (IO.Buffer.contents buffer)

module Migration = struct
  type t = {
    version: Version.t;
    description: string;
    migration_type: migration_type;
    sql: string;
    checksum: string;
    no_tx: bool;
  }

  let make = fun
    ?(no_tx = false) ?checksum:provided_checksum ~version ~description ~migration_type ~sql () ->
    let checksum =
      match provided_checksum with
      | Some value -> value
      | None -> checksum sql
    in
    {
      version;
      description = String.trim description;
      migration_type;
      sql;
      checksum;
      no_tx;
    }
end

module AppliedMigration = struct
  type t = {
    version: Version.t;
    checksum: string;
  }
end

type locking =
  | NoLock
  | PostgresAdvisory of {
      key: int64;
    }
  | MysqlNamed of {
      name: string;
      timeout: Time.Duration.t;
    }

type dialect =
  | Postgres
  | Mysql

type transaction_mode =
  | Transactional
  | NonTransactional

module Config = struct
  type t = {
    table_name: TableName.t;
    ignore_missing: bool;
    dialect: dialect;
    locking: locking;
    transaction_mode: transaction_mode;
    create_schemas: string Vector.t;
  }

  let default = {
    table_name = TableName.default;
    ignore_missing = false;
    dialect = Postgres;
    locking = NoLock;
    transaction_mode = Transactional;
    create_schemas = Vector.create ();
  }

  let for_postgres = fun ?(lock_key = 4_392_003_337_890_001L) () -> {
    default with
    dialect = Postgres;
    locking = PostgresAdvisory { key = lock_key };
    transaction_mode = Transactional;
  }

  let for_mysql = fun
    ?(lock_name = "riot_sqlx_migrations") ?(lock_timeout = Time.Duration.from_secs 30) () -> {
    default with
    dialect = Mysql;
    locking = MysqlNamed { name = lock_name; timeout = lock_timeout };
    transaction_mode = NonTransactional;
  }
end

type applied = {
  migration: Migration.t;
  elapsed: Time.Duration.t;
}

type run_report = {
  applied: applied Vector.t;
  already_applied: AppliedMigration.t Vector.t;
}

type source_error =
  | ReadMigrationFileFailed of {
      path: Path.t;
      reason: string;
    }
  | ReadMigrationDirectoryFailed of {
      path: Path.t;
      reason: string;
    }
  | InspectMigrationPathFailed of {
      path: Path.t;
      reason: string;
    }
  | MissingQueryField of string
  | QueryFieldTypeMismatch of { field: string; expected: string }

type error =
  | SourceError of source_error
  | InvalidVersion of Version.error
  | InvalidTableName of TableName.error
  | InvalidSchemaName of TableName.error
  | PoolError of Pool.error
  | ConnectionError of Connection.error
  | MigrationExecutionError of {
      version: Version.t;
      error: Connection.error;
    }
  | Dirty of Version.t
  | LockUnavailable of string
  | VersionMissing of Version.t
  | VersionMismatch of Version.t
  | VersionNotPresent of Version.t

let connection_error_to_string = Connection.error_to_string

let pool_error_to_string = fun error ->
  match error with
  | Pool.Exhausted { waiting; max_connections; timeout } ->
      "pool exhausted: "
      ^ Int.to_string waiting
      ^ " waiting, max "
      ^ Int.to_string max_connections
      ^ " connections, timeout "
      ^ Time.Duration.to_secs_string timeout
  | Pool.ConnectionError error -> "connection error: " ^ connection_error_to_string error
  | Pool.Timeout duration -> "pool timeout after " ^ Time.Duration.to_secs_string duration

let error_to_string = fun error ->
  match error with
  | SourceError source_error -> (
      match source_error with
      | ReadMigrationFileFailed { path; reason } ->
          "migration source error: failed to read " ^ Path.to_string path ^ ": " ^ reason
      | ReadMigrationDirectoryFailed { path; reason } ->
          "migration source error: failed to read migration directory "
          ^ Path.to_string path
          ^ ": "
          ^ reason
      | InspectMigrationPathFailed { path; reason } ->
          "migration source error: failed to inspect migration path "
          ^ Path.to_string path
          ^ ": "
          ^ reason
      | MissingQueryField field ->
          "migration source error: migration query did not return field: " ^ field
      | QueryFieldTypeMismatch { field; expected } ->
          "migration source error: migration query field " ^ field ^ " is not " ^ expected
    )
  | InvalidVersion error -> "invalid migration version: " ^ Version.error_to_string error
  | InvalidTableName error -> "invalid migration table name: " ^ TableName.error_to_string error
  | InvalidSchemaName error -> "invalid migration schema name: " ^ TableName.error_to_string error
  | PoolError error -> pool_error_to_string error
  | ConnectionError error -> connection_error_to_string error
  | MigrationExecutionError { version; error } ->
      "while executing migration "
      ^ Version.to_string version
      ^ ": "
      ^ connection_error_to_string error
  | Dirty version ->
      "migration "
      ^ Version.to_string version
      ^ " is partially applied; fix it and remove the dirty row"
  | LockUnavailable name -> "migration lock is unavailable: " ^ name
  | VersionMissing version ->
      "migration "
      ^ Version.to_string version
      ^ " was previously applied but is missing in the resolved migrations"
  | VersionMismatch version ->
      "migration " ^ Version.to_string version ^ " was previously applied but has been modified"
  | VersionNotPresent version ->
      "migration " ^ Version.to_string version ^ " is not present in the migration source"

module Source = struct
  type resolve_config = {
    ignored_checksum_chars: char Vector.t;
  }

  let default_resolve_config = { ignored_checksum_chars = Vector.create () }

  type t =
    | Directory of Path.t
    | Static of Migration.t Vector.t

  let from_directory = fun path -> Directory path

  let from_migrations = fun migrations -> Static migrations

  let trim_suffix = fun value ~suffix ->
    let suffix_len = String.length suffix in
    String.sub value ~offset:0 ~len:(String.length value - suffix_len)

  let description_from_filename = fun filename migration_type ->
    filename
    |> trim_suffix ~suffix:(migration_type_suffix migration_type)
    |> String.map
      ~fn:(fun char ->
        if Char.equal char '_' then
          ' '
        else
          char)
    |> String.trim

  let parse_file = fun config path ->
    let filename = Path.basename path in
    match String.index_of filename ~char:'_' with
    | None -> Ok None
    | Some split_at ->
        if not (String.ends_with ~suffix:".sql" filename) then
          Ok None
        else
          let version_text = String.sub filename ~offset:0 ~len:split_at in
          match Version.from_string version_text with
          | Error error -> Error (InvalidVersion error)
          | Ok version -> (
              let name =
                String.sub
                  filename
                  ~offset:(split_at + 1)
                  ~len:(String.length filename - split_at - 1)
              in
              let migration_type = migration_type_from_filename name in
              let description = description_from_filename name migration_type in
              match Fs.read path with
              | Error fs_error ->
                  Error (SourceError (ReadMigrationFileFailed {
                    path;
                    reason = IO.Error.message fs_error;
                  }))
              | Ok sql ->
                  let no_tx = String.starts_with ~prefix:"-- no-transaction" sql in
                  let checksum = checksum_ignoring config.ignored_checksum_chars sql in
                  Ok (Some (Migration.make
                    ~version
                    ~description
                    ~migration_type
                    ~sql
                    ~checksum
                    ~no_tx
                    ()))
            )

  let resolve_directory = fun config dir ->
    match Fs.read_dir dir with
    | Error fs_error ->
        Error (SourceError (ReadMigrationDirectoryFailed {
          path = dir;
          reason = IO.Error.message fs_error;
        }))
    | Ok paths ->
        let migrations = Vector.create () in
        let rec loop paths =
          match Iter.MutIterator.next paths with
          | None ->
              Vector.sort_by
                migrations
                ~compare:(fun left right ->
                  Version.compare
                    Migration.(left.version)
                    Migration.(right.version));
              Ok migrations
          | Some path ->
              let path =
                if Path.is_absolute path then
                  path
                else
                  Path.(dir / path)
              in
              (
                match Fs.is_file path with
                | Error fs_error ->
                    Error (SourceError (InspectMigrationPathFailed {
                      path;
                      reason = IO.Error.message fs_error;
                    }))
                | Ok false -> loop paths
                | Ok true -> (
                    match parse_file config path with
                    | Error _ as error -> error
                    | Ok None -> loop paths
                    | Ok (Some migration) ->
                        Vector.push migrations ~value:migration;
                        loop paths
                  )
              )
        in
        loop paths

  let resolve = fun ?(config = default_resolve_config) source ->
    match source with
    | Static migrations ->
        let copy = Vector.concat migrations (Vector.create ()) in
        Vector.sort_by
          copy
          ~compare:(fun left right ->
            Version.compare
              Migration.(left.version)
              Migration.(right.version));
        Ok copy
    | Directory dir -> resolve_directory config dir
end

let execute = fun conn sql params ->
  match Connection.execute conn sql params with
  | Ok _ -> Ok ()
  | Error error -> Error (ConnectionError error)

let query = fun conn sql params ->
  match Connection.query conn sql params with
  | Ok cursor -> Ok cursor
  | Error error -> Error (ConnectionError error)

let version_from_value = fun value ->
  match Value.to_int64 value with
  | Some value -> Version.from_int64 value
  | None -> (
      match Value.to_int value with
      | Some value -> Version.from_int value
      | None -> Error Version.ExpectedIntegerValue
    )

let required_field = fun field row ->
  match Row.get field row with
  | Some value -> Ok value
  | None -> Error (SourceError (MissingQueryField field))

let version_field = fun field row ->
  match required_field field row with
  | Error _ as error -> error
  | Ok value -> (
      match version_from_value value with
      | Ok version -> Ok version
      | Error error -> Error (InvalidVersion error)
    )

let string_field = fun field row ->
  match required_field field row with
  | Error _ as error -> error
  | Ok value -> (
      match Value.to_string_value value with
      | Some value -> Ok value
      | None -> (
          match Value.to_bytes value with
          | Some value -> Ok (Std.IO.Bytes.to_string value)
          | None -> (
              match Value.to_json value with
              | Some value -> Ok value
              | None -> Error (SourceError (QueryFieldTypeMismatch { field; expected = "text" }))
            )
        )
    )

let dirty_version_direct = fun conn table_name ->
  let table_name = TableName.to_string table_name in
  match query
    conn
    ("SELECT version FROM " ^ table_name ^ " WHERE success = false ORDER BY version LIMIT 1")
    [] with
  | Error _ as error -> error
  | Ok cursor -> (
      match Cursor.fetch_one cursor with
      | None -> Ok None
      | Some row -> (
          match version_field "version" row with
          | Ok version -> Ok (Some version)
          | Error _ as error -> error
        )
    )

let list_applied_direct = fun conn table_name ->
  let table_name = TableName.to_string table_name in
  match query conn ("SELECT version, checksum FROM " ^ table_name ^ " ORDER BY version") [] with
  | Error _ as error -> error
  | Ok cursor ->
      let rows = Cursor.fetch_all cursor in
      let applied = Vector.with_capacity ~size:(List.length rows) in
      let rec loop rows =
        match rows with
        | [] -> Ok applied
        | row :: rest -> (
            match (version_field "version" row, string_field "checksum" row) with
            | (Ok version, Ok checksum) ->
                Vector.push applied ~value:AppliedMigration.{ version; checksum };
                loop rest
            | (Error error, _)
            | (_, Error error) -> Error error
          )
      in
      loop rows

let with_pool_connection = fun pool fn ->
  match Pool.acquire pool with
  | Error error -> Error (PoolError error)
  | Ok conn ->
      let result = fn conn in
      Pool.release pool conn;
      result

let list_applied = fun ?(config = Config.default) pool ->
  with_pool_connection
    pool
    (fun conn -> list_applied_direct conn config.table_name)

let placeholder = fun dialect index ->
  match dialect with
  | Postgres -> "$" ^ Int.to_string index
  | Mysql -> "?"

let migration_table_sql = fun dialect table_name ->
  let table_name = TableName.to_string table_name in
  match dialect with
  | Postgres ->
      "CREATE TABLE IF NOT EXISTS "
      ^ table_name
      ^ " ("
      ^ "version BIGINT PRIMARY KEY,"
      ^ "description TEXT NOT NULL,"
      ^ "installed_on TIMESTAMPTZ NOT NULL DEFAULT now(),"
      ^ "success BOOLEAN NOT NULL,"
      ^ "checksum TEXT NOT NULL,"
      ^ "execution_time BIGINT NOT NULL"
      ^ ")"
  | Mysql ->
      "CREATE TABLE IF NOT EXISTS "
      ^ table_name
      ^ " ("
      ^ "version BIGINT PRIMARY KEY,"
      ^ "description TEXT NOT NULL,"
      ^ "installed_on TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),"
      ^ "success BOOLEAN NOT NULL,"
      ^ "checksum TEXT NOT NULL,"
      ^ "execution_time BIGINT NOT NULL"
      ^ ") ENGINE=InnoDB"

let boolish_value = fun value ->
  match value with
  | Value.Bool value -> Some value
  | Value.Int value -> Some (value != 0)
  | Value.Int16 value -> Some (value != 0)
  | Value.Int64 value -> Some (not (Int64.equal value 0L))
  | Value.String value -> (
      match value with
      | "1" -> Some true
      | "0" -> Some false
      | _ -> None
    )
  | _ -> None

let ensure_schema = fun conn dialect schema_name ->
  match TableName.from_string schema_name with
  | Error error -> Error (InvalidSchemaName error)
  | Ok schema_name ->
      let sql =
        match dialect with
        | Postgres
        | Mysql -> "CREATE SCHEMA IF NOT EXISTS " ^ TableName.to_string schema_name
      in
      execute conn sql []

let ensure_migrations_table = fun conn dialect table_name ->
  execute
    conn
    (migration_table_sql dialect table_name)
    []

let lock = fun conn locking ->
  match locking with
  | NoLock -> Ok ()
  | PostgresAdvisory { key } ->
      let rec attempt () =
        match query conn "SELECT pg_try_advisory_lock($1) AS locked" [ Value.int64 key ] with
        | Error _ as error -> error
        | Ok cursor -> (
            match Cursor.fetch_one cursor with
            | Some row when Row.bool "locked" row = Some true -> Ok ()
            | Some _
            | None ->
                sleep (Time.Duration.from_millis 250);
                attempt ()
          )
      in
      attempt ()
  | MysqlNamed { name; timeout } -> (
      match query
        conn
        "SELECT GET_LOCK(?, ?) AS locked"
        [ Value.string name; Value.int (Time.Duration.to_secs timeout) ] with
      | Error _ as error -> error
      | Ok cursor -> (
          match Cursor.fetch_one cursor with
          | Some row -> (
              match Row.get "locked" row
              |> Option.and_then ~fn:boolish_value with
              | Some true -> Ok ()
              | Some false
              | None -> Error (LockUnavailable name)
            )
          | None -> Error (LockUnavailable name)
        )
    )

let unlock = fun conn locking ->
  match locking with
  | NoLock -> Ok ()
  | PostgresAdvisory { key } -> execute conn "SELECT pg_advisory_unlock($1)" [ Value.int64 key ]
  | MysqlNamed { name; _ } -> (
      match query conn "SELECT RELEASE_LOCK(?) AS released" [ Value.string name ] with
      | Error _ as error -> error
      | Ok cursor -> (
          match Cursor.fetch_one cursor with
          | Some row -> (
              match Row.get "released" row
              |> Option.and_then ~fn:boolish_value with
              | Some true -> Ok ()
              | Some false
              | None -> Error (LockUnavailable name)
            )
          | None -> Error (LockUnavailable name)
        )
    )

let with_lock = fun conn locking fn ->
  match lock conn locking with
  | Error _ as error -> error
  | Ok () ->
      let result = fn () in
      match unlock conn locking with
      | Ok () -> result
      | Error _ as error -> error

let find_applied = fun applied version ->
  let rec loop index =
    if index >= Vector.length applied then
      None
    else
      let item = Vector.get_unchecked applied ~at:index in
      if Version.equal AppliedMigration.(item.version) version then
        Some item
      else
        loop (index + 1)
  in
  loop 0

let find_up_migration = fun migrations version ->
  let rec loop index =
    if index >= Vector.length migrations then
      None
    else
      let migration = Vector.get_unchecked migrations ~at:index in
      if
        Version.equal Migration.(migration.version) version
        && is_up_migration Migration.(migration.migration_type)
      then
        Some migration
      else
        loop (index + 1)
  in
  loop 0

let validate_applied = fun ~ignore_missing applied migrations ->
  let rec loop index =
    if index >= Vector.length applied then
      Ok ()
    else
      let applied_migration = Vector.get_unchecked applied ~at:index in
      match find_up_migration migrations AppliedMigration.(applied_migration.version) with
      | None ->
          if ignore_missing then
            loop (index + 1)
          else
            Error (VersionMissing AppliedMigration.(applied_migration.version))
      | Some migration ->
          if
            String.equal
              Migration.(migration.checksum)
              AppliedMigration.(applied_migration.checksum)
          then
            loop (index + 1)
          else
            Error (VersionMismatch Migration.(migration.version))
  in
  loop 0

let record_migration_start = fun conn dialect table_name migration ->
  let table_name = TableName.to_string table_name in
  match Connection.execute
    conn
    ("INSERT INTO "
    ^ table_name
    ^ " (version, description, success, checksum, execution_time) VALUES ("
    ^ placeholder dialect 1
    ^ ", "
    ^ placeholder dialect 2
    ^ ", FALSE, "
    ^ placeholder dialect 3
    ^ ", -1)")
    [
      Value.int64 (Version.to_int64 Migration.(migration.version));
      Value.string Migration.(migration.description);
      Value.string Migration.(migration.checksum);
    ] with
  | Error error -> Error (ConnectionError error)
  | Ok _ -> Ok ()

let execute_migration_body = fun conn migration ->
  let rec loop statements =
    match statements with
    | [] -> Ok ()
    | statement :: rest -> (
        match Connection.execute conn statement [] with
        | Error error ->
            Error (MigrationExecutionError { version = Migration.(migration.version); error })
        | Ok _ -> loop rest
      )
  in
  match Connection.prepare_migration conn Migration.(migration.sql) with
  | Error error ->
      Error (MigrationExecutionError { version = Migration.(migration.version); error })
  | Ok statements -> loop statements

let finalize_migration = fun conn dialect table_name migration elapsed ->
  let table_name = TableName.to_string table_name in
  match Connection.execute
    conn
    ("UPDATE "
    ^ table_name
    ^ " SET success = TRUE, execution_time = "
    ^ placeholder dialect 1
    ^ " WHERE version = "
    ^ placeholder dialect 2)
    [
      Value.int64 (Time.Duration.to_nanos elapsed);
      Value.int64 (Version.to_int64 Migration.(migration.version));
    ] with
  | Ok _ -> Ok ()
  | Error error -> Error (ConnectionError error)

let with_transaction = fun conn fn ->
  match Connection.begin_transaction conn with
  | Error error -> Error (ConnectionError error)
  | Ok () -> (
      match fn conn with
      | Ok value -> (
          match Connection.commit conn with
          | Ok () -> Ok value
          | Error error ->
              ignore (Connection.rollback conn);
              Error (ConnectionError error)
        )
      | Error error ->
          ignore (Connection.rollback conn);
          Error error
    )

let use_transaction = fun config migration ->
  match (Config.(config.transaction_mode), Migration.(migration.no_tx)) with
  | (_, true) -> false
  | (Transactional, false) -> true
  | (NonTransactional, false) -> false

let apply_one = fun conn config migration ->
  let started_at = Time.Instant.now () in
  let apply_body = fun body_conn ->
    match record_migration_start
      body_conn
      Config.(config.dialect)
      Config.(config.table_name)
      migration with
    | Error _ as error -> error
    | Ok () -> execute_migration_body body_conn migration
  in
  let result =
    if use_transaction config migration then
      with_transaction conn apply_body
    else
      apply_body conn
  in
  match result with
  | Error _ as error -> error
  | Ok () ->
      let elapsed = Time.Instant.duration_since ~earlier:started_at (Time.Instant.now ()) in
      match finalize_migration
        conn
        Config.(config.dialect)
        Config.(config.table_name)
        migration
        elapsed with
      | Error _ as error -> error
      | Ok () -> Ok { migration; elapsed }

let revert_migration_body = fun conn dialect table_name migration ->
  let table_name = TableName.to_string table_name in
  match execute_migration_body conn migration with
  | Error _ as error -> error
  | Ok () -> (
      match Connection.execute
        conn
        ("DELETE FROM " ^ table_name ^ " WHERE version = " ^ placeholder dialect 1)
        [ Value.int64 (Version.to_int64 Migration.(migration.version)) ] with
      | Error error -> Error (ConnectionError error)
      | Ok _ -> Ok ()
    )

let revert_one = fun conn config migration ->
  let started_at = Time.Instant.now () in
  let result =
    if use_transaction config migration then
      with_transaction
        conn
        (fun tx_conn ->
          revert_migration_body
            tx_conn
            Config.(config.dialect)
            Config.(config.table_name)
            migration)
    else
      revert_migration_body conn Config.(config.dialect) Config.(config.table_name) migration
  in
  match result with
  | Error _ as error -> error
  | Ok () ->
      let elapsed = Time.Instant.duration_since ~earlier:started_at (Time.Instant.now ()) in
      Ok { migration; elapsed }

let create_schemas = fun conn dialect schemas ->
  let rec loop index =
    if index >= Vector.length schemas then
      Ok ()
    else
      match ensure_schema
        conn
        dialect
        (Vector.get_unchecked schemas ~at:index) with
      | Error _ as error -> error
      | Ok () -> loop (index + 1)
  in
  loop 0

let setup = fun conn config ->
  match create_schemas conn Config.(config.dialect) Config.(config.create_schemas) with
  | Error _ as error -> error
  | Ok () -> (
      match ensure_migrations_table conn Config.(config.dialect) Config.(config.table_name) with
      | Error _ as error -> error
      | Ok () -> (
          match dirty_version_direct conn Config.(config.table_name) with
          | Error _ as error -> error
          | Ok (Some version) -> Error (Dirty version)
          | Ok None -> list_applied_direct conn Config.(config.table_name)
        )
    )

let run_direct = fun ?target config conn migrations ->
  with_lock
    conn
    Config.(config.locking)
    (fun () ->
      match setup conn config with
      | Error _ as error -> error
      | Ok applied_migrations -> (
          match validate_applied
            ~ignore_missing:Config.(config.ignore_missing)
            applied_migrations
            migrations with
          | Error _ as error -> error
          | Ok () ->
              let applied = Vector.create () in
              let rec loop index =
                if index >= Vector.length migrations then
                  Ok { applied; already_applied = applied_migrations }
                else
                  let migration = Vector.get_unchecked migrations ~at:index in
                  match target with
                  | Some target when Version.compare Migration.(migration.version) target = Order.GT ->
                      Ok { applied; already_applied = applied_migrations }
                  | _ ->
                      if is_down_migration Migration.(migration.migration_type) then
                        loop (index + 1)
                      else
                        match find_applied applied_migrations Migration.(migration.version) with
                        | Some _ -> loop (index + 1)
                        | None -> (
                            match apply_one conn config migration with
                            | Error _ as error -> error
                            | Ok applied_migration ->
                                Vector.push applied ~value:applied_migration;
                                loop (index + 1)
                          )
              in
              loop 0
        ))

let run_resolved = fun ?target config pool migrations ->
  with_pool_connection
    pool
    (fun conn ->
      run_direct ?target config conn migrations)

let run = fun ?(config = Config.default) pool source ->
  match Source.resolve source with
  | Error _ as error -> error
  | Ok migrations -> run_resolved config pool migrations

let run_to = fun ?(config = Config.default) pool source ~target ->
  match Source.resolve source with
  | Error _ as error -> error
  | Ok migrations -> run_resolved ~target config pool migrations

let undo_direct = fun config conn migrations target ->
  with_lock
    conn
    Config.(config.locking)
    (fun () ->
      match setup conn config with
      | Error _ as error -> error
      | Ok applied_migrations -> (
          match validate_applied
            ~ignore_missing:Config.(config.ignore_missing)
            applied_migrations
            migrations with
          | Error _ as error -> error
          | Ok () ->
              let reverted = Vector.create () in
              let rec loop index =
                if index < 0 then
                  Ok { applied = reverted; already_applied = applied_migrations }
                else
                  let migration = Vector.get_unchecked migrations ~at:index in
                  if
                    is_down_migration Migration.(migration.migration_type)
                    && Version.compare Migration.(migration.version) target = Order.GT
                    && Option.is_some
                      (find_applied applied_migrations Migration.(migration.version))
                  then
                    match revert_one conn config migration with
                    | Error _ as error -> error
                    | Ok applied_migration ->
                        Vector.push reverted ~value:applied_migration;
                        loop (index - 1)
                  else
                    loop (index - 1)
              in
              loop (Vector.length migrations - 1)
        ))

let undo = fun ?(config = Config.default) pool source ~target ->
  match Source.resolve source with
  | Error _ as error -> error
  | Ok migrations ->
      with_pool_connection pool (fun conn -> undo_direct config conn migrations target)
