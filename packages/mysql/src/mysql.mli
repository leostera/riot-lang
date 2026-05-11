open Std

(**
   MySQL/InnoDB driver support for [Sqlx].

   The driver speaks the MySQL 4.1+ protocol and targets InnoDB-backed
   application schemas. Use [?] placeholders in parameterized SQL. Migration
   bodies are prepared as semicolon-terminated statements for SQLx's migration
   runner.
*)
module Error: sig
  type t

  val to_string: t -> string

  val serialize: t Serde.Ser.t
end

module Config: sig
  type ssl_mode =
    | Disable
    | Prefer
    | Require
  type t = {
    host: string;
    port: int;
    database: string option;
    user: string;
    password: string;
    ssl_mode: ssl_mode;
    collation_id: int;
    connect_timeout: Time.Duration.t;
    keepalives_idle: Time.Duration.t option;
  }
  type parse_error =
    | InvalidUserinfoFormat
    | InvalidAuthorityFormat
    | MissingUserCredentials
    | InvalidPortNumber of string
    | InvalidSslMode of string
    | InvalidConnectionStringFormat
    | InvalidUri

  val default: unit -> t

  val parse_error_to_string: parse_error -> string

  (**
     Parse [mysql://user:password@host:port/database] or legacy
     [host:port:database:user:password] connection strings.

     URI query parameters may include [ssl-mode], [ssl_mode], or [sslMode]
     with one of [disable], [prefer], or [require].
  *)
  val from_string: string -> (t, parse_error) result
end

module Internal: sig
  module Protocol: module type of Protocol
end

module Driver: Sqlx_driver.Driver.Intf with type config = Config.t
