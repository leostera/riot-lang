# SQLx Design Document

## Overview

SQLx is a minimal but powerful SQL database library for OCaml that provides:
- Type-safe database access without an ORM
- Connection pooling with automatic management
- Support for multiple database drivers
- Actor-based architecture using Miniriot
- Clean, simple API focused on raw SQL

## Architecture

### Core Principles

1. **Simplicity First**: Simple interface that hides complexity
2. **No ORM**: Focus on SQL with lightweight result mapping
3. **Actor-Based**: Leverage Riot's process model for concurrency
4. **Type Safety**: Leverage OCaml's type system for safety
5. **Driver Agnostic**: Clean abstraction over different databases

### Module Structure

```
sqlx/
├── src/
│   ├── sqlx.ml           # Main module with public API
│   ├── sqlx.mli          # Public interface
│   ├── driver.ml         # Driver interface definition
│   ├── driver.mli        # Driver interface
│   ├── connection.ml     # Connection management
│   ├── connection.mli    # Connection interface
│   ├── pool.ml          # Connection pooling logic
│   ├── pool.mli         # Pool interface
│   ├── cursor.ml        # Cursor implementation
│   ├── cursor.mli       # Cursor interface
│   ├── row.ml           # Row access implementation
│   ├── row.mli          # Row interface
│   ├── value.ml         # Value type and conversions
│   ├── value.mli        # Value interface
│   ├── transaction.ml   # Transaction management
│   ├── transaction.mli  # Transaction interface
│   └── drivers/
│       ├── postgres.ml  # PostgreSQL driver
│       ├── postgres.mli # PostgreSQL interface
│       ├── sqlite.ml    # SQLite driver (future)
│       └── sqlite.mli   # SQLite interface (future)
└── test/
    ├── test_sqlx.ml     # Main tests
    ├── test_pool.ml     # Pool tests
    └── test_postgres.ml # PostgreSQL specific tests
```

## Implementation Plan

### Phase 1: Core Types and Interfaces

#### 1.1 Value Type (`value.ml`)

```ocaml
type t = 
  | Null
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Bytes of bytes
  | Timestamp of Time.Instant.t

(* Constructors *)
val null : t
val int : int -> t
val string : string -> t
val bool : bool -> t
val float : float -> t
val bytes : bytes -> t
val timestamp : Time.Instant.t -> t

(* Conversions *)
val to_int : t -> int option
val to_string : t -> string option
val to_bool : t -> bool option
val to_float : t -> float option
val is_null : t -> bool
```

#### 1.2 Driver Interface (`driver.ml`)

```ocaml
module type Intf = sig
  type config
  type connection
  type statement
  type result_set
  
  val name : string
  val connect : config -> (connection, string) result
  val close : connection -> unit
  val ping : connection -> bool
  val prepare : connection -> string -> (statement, string) result
  val execute : statement -> Value.t list -> (result_set, string) result
  val fetch_row : result_set -> Row.t option
  val rows_affected : result_set -> int
  
  (* Transaction support *)
  val begin_transaction : connection -> (unit, string) result
  val commit : connection -> (unit, string) result
  val rollback : connection -> (unit, string) result
  val set_isolation_level : connection -> [`Read_uncommitted | `Read_committed | `Repeatable_read | `Serializable] -> (unit, string) result
end
```

### Phase 2: Connection Management

#### 2.1 Connection Module (`connection.ml`)

```ocaml


type driver_connection = 
  | Postgres of Postgres_driver.connection
  | Sqlite of Sqlite_driver.connection

type t = {
  id : string;
  driver : (module Driver.Intf);
  conn : driver_connection;
  pool : Pool.t option;  (* Back reference if from pool *)
  in_transaction : bool ref;
  created_at : Time.Instant.t;
  last_used : Time.Instant.t ref;
}

(* Connection process messages *)
type Message.t +=
  | Query of string * Value.t list * Pid.t
  | Execute of string * Value.t list * Pid.t
  | Ping of Pid.t
  | Close

(* Connection actor *)
let connection_process config driver =
  let rec loop conn =
    match receive_any () with
    | Query (sql, params, reply_to) ->
        let result = execute_query conn sql params in
        send reply_to (QueryResult result);
        loop conn
    | Execute (sql, params, reply_to) ->
        let result = execute_stmt conn sql params in
        send reply_to (ExecResult result);
        loop conn
    | Ping reply_to ->
        let alive = check_connection conn in
        send reply_to (PingResult alive);
        loop conn
    | Close ->
        cleanup_connection conn
  in
  (* Connect and start loop *)
  match connect_driver config driver with
  | Ok conn -> loop conn
  | Error e -> failwith e
```

#### 2.2 Connection Pool (`pool.ml`)

```ocaml


type config = {
  driver : (module Driver.Intf);
  driver_config : 'a;
  min_connections : int;
  max_connections : int;
  acquire_timeout : Duration.t;
  idle_timeout : Duration.t;
  max_lifetime : Duration.t option;
}

type connection_state = 
  | Available of Connection.t
  | InUse of Connection.t * Pid.t * Time.Instant.t

type t = {
  config : config;
  connections : connection_state MutList.t;
  waiting : Pid.t Queue.t;
  supervisor : Pid.t;
}

(* Pool manager process messages *)
type Message.t +=
  | Acquire of Pid.t
  | Release of Connection.t * Pid.t
  | HealthCheck
  | Shutdown

(* Pool supervisor process *)
let pool_supervisor config =
  let connections = MutList.create () in
  let waiting = Queue.create () in
  
  (* Initialize minimum connections *)
  for i = 1 to config.min_connections do
    let conn = spawn_connection config.driver config.driver_config in
    MutList.push connections (Available conn)
  done;
  
  let rec loop () =
    match receive_any () with
    | Acquire requester ->
        handle_acquire connections waiting requester config;
        loop ()
    | Release (conn, _releaser) ->
        handle_release connections waiting conn;
        loop ()
    | HealthCheck ->
        check_all_connections connections config;
        loop ()
    | Shutdown ->
        shutdown_all connections
  in
  loop ()

let handle_acquire connections waiting requester config =
  (* Try to find available connection *)
  match find_available connections with
  | Some conn ->
      mark_in_use connections conn requester;
      send requester (ConnectionAcquired conn)
  | None ->
      if MutList.length connections < config.max_connections then
        (* Spawn new connection *)
        let conn = spawn_connection config.driver config.driver_config in
        MutList.push connections (InUse (conn, requester, Time.Instant.now ()));
        send requester (ConnectionAcquired conn)
      else
        (* Queue the request *)
        Queue.push requester waiting
```

### Phase 3: Query Execution

#### 3.1 Cursor Module (`cursor.ml`)

```ocaml
type t = {
  id : string;
  connection : Connection.t;
  result_set : driver_result_set;
  mutable exhausted : bool;
  mutable row_count : int;
}

let fetch_one cursor =
  if cursor.exhausted then None
  else
    match fetch_row_from_driver cursor.result_set with
    | Some row ->
        cursor.row_count <- cursor.row_count + 1;
        Some row
    | None ->
        cursor.exhausted <- true;
        None

let fetch_many cursor count =
  MutIterator.create (fun () ->
    if cursor.exhausted || cursor.row_count >= count then
      None
    else
      fetch_one cursor
  )

let fetch_all cursor =
  MutIterator.create (fun () ->
    if cursor.exhausted then None
    else fetch_one cursor
  )
```

#### 3.2 Row Module (`row.ml`)

```ocaml
type t = (string * Value.t) list

let get field row =
  List.assoc_opt field row

let fields row =
  List.map fst row

(* Typed accessors *)
let int field row =
  match get field row with
  | Some (Value.Int n) -> Some n
  | _ -> None

let string field row =
  match get field row with
  | Some (Value.String s) -> Some s
  | _ -> None

let bool field row =
  match get field row with
  | Some (Value.Bool b) -> Some b
  | _ -> None

(* Exception variants *)
let int_exn field row =
  match int field row with
  | Some n -> n
  | None -> failwith (Printf.sprintf "Field %s not found or not an int" field)

let string_exn field row =
  match string field row with
  | Some s -> s
  | None -> failwith (Printf.sprintf "Field %s not found or not a string" field)
```

### Phase 4: Main API

#### 4.1 Main Module (`sqlx.ml`)

```ocaml
open Std

type error = 
  | Connection_failed of string
  | Query_failed of string
  | Pool_exhausted
  | Invalid_value of string
  | Driver_error of string

module Config = struct
  type t = {
    pool_size : int;
    max_idle_time : Duration.t;
    acquire_timeout : Duration.t;
    idle_check_interval : Duration.t;
    max_lifetime : Duration.t option;
    auto_commit : bool;
    isolation_level : [`Read_uncommitted | `Read_committed | `Repeatable_read | `Serializable] option;
    query_timeout : Duration.t option;
    log_queries : bool;
    log_slow_queries : Duration.t option;
  }
  
  let default = {
    pool_size = 10;
    max_idle_time = Duration.of_min 10;
    acquire_timeout = Duration.of_sec 30;
    idle_check_interval = Duration.of_min 1;
    max_lifetime = Some (Duration.of_hour 1);
    auto_commit = true;
    isolation_level = None;
    query_timeout = None;
    log_queries = false;
    log_slow_queries = None;
  }
end

(* Re-export submodules *)
module Connection = Connection
module Cursor = Cursor
module Row = Row
module Value = Value
module Transaction = Transaction
module Statement = Statement

(* Main API *)
let connect ?config ~driver driver_config =
  let config = Option.value config ~default:Config.default in
  Pool.create config driver driver_config

let query conn sql params =
  Pool.with_connection conn (fun db_conn ->
    Connection.query db_conn sql params
  )

let exec conn sql params =
  Pool.with_connection conn (fun db_conn ->
    Connection.execute db_conn sql params
  )

let with_transaction conn f =
  Pool.with_connection conn (fun db_conn ->
    Transaction.with_transaction db_conn f
  )
```

### Phase 5: PostgreSQL Driver

#### 5.1 PostgreSQL Driver (`drivers/postgres.ml`)

```ocaml
open Std

type config = {
  host : string;
  port : int;
  database : string;
  user : string;
  password : string;
  ssl_mode : [`Disable | `Require | `Prefer];
  application_name : string option;
  search_path : string list option;
  options : (string * string) list;
}

type connection = {
  socket : Unix.file_descr;
  pid : int;  (* Backend process ID *)
  secret : int;  (* Backend secret key *)
  parameters : (string * string) list;
  mutable transaction_status : char;
  prepared_statements : (string, statement) Hashtbl.t;
}

type statement = {
  name : string;
  sql : string;
  param_types : oid list;
}

type result_set = {
  fields : field_desc list;
  rows : Value.t list list Queue.t;
  mutable current_row : Value.t list option;
}

let connect config =
  try
    (* Create socket *)
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    
    (* Connect to PostgreSQL *)
    let addr = Unix.inet_addr_of_string config.host in
    let sockaddr = Unix.ADDR_INET (addr, config.port) in
    Unix.connect socket sockaddr;
    
    (* Send startup message *)
    send_startup_message socket config;
    
    (* Authentication *)
    match read_auth_request socket with
    | AuthOk -> ()
    | AuthMD5 salt ->
        let password_hash = compute_md5_password config.user config.password salt in
        send_password socket password_hash;
        wait_for_auth_ok socket
    | _ -> failwith "Unsupported authentication method";
    
    (* Read backend parameters *)
    let params = read_parameters socket in
    let pid = read_backend_key socket in
    
    (* Wait for ReadyForQuery *)
    wait_for_ready socket;
    
    Ok {
      socket;
      pid;
      secret = 0;
      parameters = params;
      transaction_status = 'I';
      prepared_statements = Hashtbl.create 16;
    }
  with
  | exn -> Error (Printexc.to_string exn)

let execute stmt params =
  (* Parameter conversion *)
  let pg_params = List.map value_to_postgres params in
  
  (* Send Bind + Execute *)
  send_bind stmt.socket stmt.name pg_params;
  send_execute stmt.socket;
  
  (* Read results *)
  let rec read_results acc =
    match read_message stmt.socket with
    | DataRow values ->
        let row = List.map postgres_to_value values in
        read_results (row :: acc)
    | CommandComplete tag ->
        let rows_affected = parse_command_tag tag in
        Ok { fields = []; rows = Queue.of_list (List.rev acc); current_row = None }
    | EmptyQueryResponse ->
        Ok { fields = []; rows = Queue.create (); current_row = None }
    | ErrorResponse err ->
        Error (format_postgres_error err)
    | _ -> read_results acc
  in
  read_results []

(* Protocol implementation details *)
let send_startup_message socket config =
  let msg = Buffer.create 256 in
  Buffer.add_int32_be msg 196608l;  (* Protocol version 3.0 *)
  Buffer.add_string msg "user\x00";
  Buffer.add_string msg config.user;
  Buffer.add_string msg "\x00";
  Buffer.add_string msg "database\x00";
  Buffer.add_string msg config.database;
  Buffer.add_string msg "\x00";
  Option.iter (fun app ->
    Buffer.add_string msg "application_name\x00";
    Buffer.add_string msg app;
    Buffer.add_string msg "\x00";
  ) config.application_name;
  Buffer.add_string msg "\x00";
  
  let contents = Buffer.contents msg in
  let len = String.length contents + 4 in
  let final_msg = Buffer.create (len + 4) in
  Buffer.add_int32_be final_msg (Int32.of_int len);
  Buffer.add_string final_msg contents;
  
  Unix.write socket (Buffer.to_bytes final_msg) 0 (Buffer.length final_msg)
```

## Testing Strategy

### Unit Tests
- Value conversions
- Row accessors
- Pool management logic
- Connection lifecycle

### Integration Tests
- PostgreSQL connection and queries
- Transaction handling
- Concurrent access via pool
- Error handling

### Example Test:

```ocaml
open Std
open Test

let test_basic_query () =
  let config = Postgres.{
    host = "localhost";
    port = 5432;
    database = "test_db";
    user = "test_user";
    password = "test_pass";
    ssl_mode = `Disable;
    application_name = Some "sqlx_test";
    search_path = None;
    options = [];
  } in
  
  match Sqlx.connect ~driver:(module Postgres) config with
  | Error e -> fail (Printf.sprintf "Connection failed: %s" (Sqlx.show_error e))
  | Ok conn ->
      (* Create test table *)
      let _ = Sqlx.exec conn 
        "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT, age INT)" 
        [] in
      
      (* Insert test data *)
      let _ = Sqlx.exec conn
        "INSERT INTO users (name, age) VALUES ($1, $2)"
        [Sqlx.Value.string "Alice"; Sqlx.Value.int 30] in
      
      (* Query data *)
      match Sqlx.query conn "SELECT name, age FROM users WHERE age > $1" [Sqlx.Value.int 25] with
      | Error e -> fail (Printf.sprintf "Query failed: %s" (Sqlx.show_error e))
      | Ok cursor ->
          match Sqlx.Cursor.fetch_one cursor with
          | None -> fail "Expected at least one row"
          | Some row ->
              assert_equal (Sqlx.Row.string "name" row) (Some "Alice");
              assert_equal (Sqlx.Row.int "age" row) (Some 30);
              
      (* Cleanup *)
      let _ = Sqlx.exec conn "DROP TABLE users" [] in
      Sqlx.Connection.close conn
```

## Migration Path

### From Current State
1. Implement core types (Value, Row)
2. Define Driver interface
3. Create connection pool using Miniriot actors
4. Implement PostgreSQL driver using existing OCaml postgres libraries
5. Add transaction support
6. Create comprehensive test suite

### Future Enhancements
1. **Prepared Statements Cache**: Automatic caching of prepared statements
2. **Query Builder**: Optional module for building dynamic queries safely
3. **Migration System**: Database schema migration management
4. **Compile-time Checking**: PPX for compile-time SQL validation (like Rust's SQLx)
5. **More Drivers**: MySQL, SQLite, MariaDB support
6. **Streaming Results**: For large result sets
7. **Connection Retry Logic**: Automatic reconnection with exponential backoff

## Dependencies

- `std` - Standard library for Path, Result, Collections
- `kernel` - Low-level networking
- `miniriot` - Actor system for connection pooling
- `suri` - Socket pool implementation reference
- `postgresql-ocaml` or `pgx` - PostgreSQL protocol implementation

## Performance Considerations

1. **Connection Pooling**: Reuse connections to avoid handshake overhead
2. **Prepared Statements**: Cache and reuse prepared statements
3. **Lazy Result Fetching**: Use cursors and iterators to avoid loading entire result sets
4. **Actor-based Concurrency**: Leverage Miniriot for parallel query execution
5. **Efficient Value Conversion**: Minimize allocations in hot paths

## Security Considerations

1. **SQL Injection Prevention**: Always use parameterized queries
2. **Connection String Security**: Never log passwords
3. **SSL/TLS Support**: Enforce encrypted connections in production
4. **Timeout Protection**: Prevent resource exhaustion
5. **Connection Limits**: Enforce maximum connection pool size

## API Stability

The public API should be stable from v1.0:
- Core types (Value, Row, Connection, Cursor)
- Main functions (connect, query, exec)
- Driver interface for extensibility

Internal implementation details can change:
- Pool management strategy
- Protocol implementations
- Performance optimizations
