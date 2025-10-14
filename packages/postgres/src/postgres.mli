open Std

(* PostgreSQL Database Driver
   
   This module provides a PostgreSQL driver implementation for SQLx.
   PostgreSQL is a powerful, open-source object-relational database system
   with strong support for complex queries, transactions, and concurrency.
   
   ## Features
   
   - Full ACID compliance
   - Rich SQL support with extensions
   - Advanced data types (JSON, arrays, custom types)
   - Full-text search
   - Concurrent access with MVCC
   - Streaming replication
   - Prepared statements for performance
   
   ## Wire Protocol
   
   This driver implements PostgreSQL's wire protocol v3.0, supporting:
   - Simple and extended query protocols
   - Prepared statements with parameter binding
   - Binary and text format data transfer
   - COPY protocol for bulk operations
   - Asynchronous notifications (LISTEN/NOTIFY)
   
   ## Example Usage
   
   ```ocaml
   open Sqlx
   
   (* Connect to PostgreSQL *)
   let config = Postgres.Config.{
     host = "localhost";
     port = 5432;
     database = "myapp";
     user = "postgres";
     password = "secret";
     ssl_mode = `Prefer;
     application_name = Some "my_app";
     connect_timeout = Time.Duration.of_sec 10;
     keepalives_idle = None;
   } in
   
   let pool = Sqlx.connect ~driver:(module Postgres.Driver) config in
   
   (* Use PostgreSQL-specific features *)
   Sqlx.exec pool 
     "INSERT INTO users (data) VALUES ($1::jsonb)" 
     [Sqlx.Value.string {|{"name": "Alice", "age": 30}|}]
   ```
   
   ## Authentication Methods
   
   Currently supported:
   - Password (cleartext) - not recommended
   - MD5 password
   - SCRAM-SHA-256 (planned)
   
   ## Connection Pooling
   
   PostgreSQL benefits greatly from connection pooling due to its
   process-per-connection model. The SQLx pool handles this automatically.
*)

(* Configuration for PostgreSQL connections *)
module Config : sig
  (* PostgreSQL connection configuration *)
  type t = {
    host : string;
        (* Database server hostname or IP address.
       Use "localhost" for local connections, or "/var/run/postgresql" for Unix sockets. *)
    port : int; (* Database server port (default PostgreSQL port is 5432) *)
    database : string; (* Name of the database to connect to *)
    user : string; (* Username for authentication *)
    password : string;
        (* Password for authentication.
       Consider using environment variables or secure vaults for production. *)
    ssl_mode : [ `Disable | `Require | `Prefer ];
        (* SSL/TLS connection mode:
       - `Disable`: Never use SSL (not recommended for production)
       - `Require`: Always use SSL, fail if server doesn't support it
       - `Prefer`: Try SSL first, fall back to non-SSL if unavailable
    *)
    application_name : string option;
        (* Application name to report to PostgreSQL.
       Visible in pg_stat_activity and useful for monitoring. *)
    connect_timeout : Time.Duration.t;
        (* Maximum time to wait when establishing a connection.
       This includes DNS resolution, TCP connection, and authentication. *)
    keepalives_idle : Time.Duration.t option;
        (* Time before sending TCP keepalive probes on idle connections.
       Helps detect broken connections behind firewalls/NAT.
       `None` uses system defaults. *)
  }

  (* `default ()` creates a configuration with common default values.
     
     Default settings:
     - host = "localhost"
     - port = 5432
     - database = "postgres"
     - user = "postgres"
     - password = "" (empty)
     - ssl_mode = `Prefer`
     - application_name = None
     - connect_timeout = 10 seconds
     - keepalives_idle = None (system default)
     
     You should override at least the database, user, and password fields.
  *)
  val default : unit -> t

  (* `from_string str` parses a connection string in either format:
     
     1. PostgreSQL URI format:
        postgresql://user:password@host:port/database
        postgres://user:password@host:port/database
        
     2. Simple colon-separated format:
        host:port:database:user:password
     
     Examples:
       - "postgresql://myuser:secret@localhost:5432/mydb"
       - "localhost:5432:mydb:myuser:secret"
     
     Returns an error if the format is invalid or required components are missing.
  *)
  val from_string : string -> (t, string) Result.t
end

(* PostgreSQL driver implementation for SQLx *)
module Driver : Sqlx_driver.Driver.Intf with type config = Config.t
