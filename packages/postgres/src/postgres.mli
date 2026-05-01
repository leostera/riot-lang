open Std

(**
   PostgreSQL driver support for [Sqlx].

   Use this package when a Riot application needs to talk to PostgreSQL
   through the shared SQL interface. The public surface here focuses on the
   parts callers actually configure directly: connection settings, driver
   wiring, and error rendering.
*)

(**
   PostgreSQL driver errors.

   Use this module when a caller needs to render driver failures into logs,
   JSON responses, or diagnostics.
*)
module Error: sig
  type t

  (** Render a driver error as a human-readable string. *)
  val to_string: t -> string

  (** Render a driver error as JSON. *)
  val to_json: t -> Data.Json.t
end

(**
   Connection settings for PostgreSQL.

   This module is the main entry point for configuring how the driver connects
   to a PostgreSQL server.
*)
module Config: sig
  (** SSL/TLS policy for the connection. *)
  type ssl_mode =
    | Disable
    | Require
    | Prefer
  type t = {
    (** Database server hostname, IP address, or Unix-socket directory. *)
    host: string;
    (** Database server port. PostgreSQL usually listens on [5432]. *)
    port: int;
    (** Name of the database to connect to. *)
    database: string;
    (** Username used during authentication. *)
    user: string;
    (** Password used during authentication. *)
    password: string;
    (**
       TLS policy used for the connection.

       Current implementation note: TLS negotiation is not implemented yet.
       [Require] fails with a structured driver error. [Prefer] currently uses
       the plaintext path.
    *)
    ssl_mode: ssl_mode;
    (** Optional application name reported to PostgreSQL. *)
    application_name: string option;
    (**
       Maximum time allowed for establishing the connection.

       Reserved for the connection path; not enforced by the current TCP
       implementation yet.
    *)
    connect_timeout: Time.Duration.t;
    (**
       Optional idle timeout before TCP keepalive probes are sent.

       Reserved for the connection path; not enforced by the current TCP
       implementation yet.
    *)
    keepalives_idle: Time.Duration.t option;
  }

  (**
     Build a configuration with development-friendly defaults.

     Example return values:
     - [host = "localhost"]
     - [port = 5432]
     - [database = "postgres"]
     - [user = "postgres"]
     - [ssl_mode = Prefer]

     Override at least the database name and credentials before using the
     result against a real server.
  *)
  val default: unit -> t

  (**
     Parse a PostgreSQL connection string.

     Supported formats:
     - URI form, such as [postgresql://user:password@localhost:5432/app]
     - Simple colon-separated form, such as [localhost:5432:app:user:password]

     Use [from_string] when configuration comes from environment variables,
     command-line flags, or secrets storage.
  *)
  val from_string: string -> (t, string) Result.t
end

(** Internal protocol modules exposed for package-level regression tests. *)
module Internal: sig
  module Protocol: module type of Protocol
end

(** [Sqlx_driver] implementation backed by PostgreSQL. *)
module Driver: Sqlx_driver.Driver.Intf with type config = Config.t
