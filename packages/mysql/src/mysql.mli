open Std

(**
   MySQL/InnoDB driver support for [Sqlx].

   The driver speaks the MySQL 4.1+ protocol and targets InnoDB-backed
   application schemas. Use [?] placeholders in parameterized SQL.
*)
module Error: sig
  type t

  val to_string: t -> string

  val serialize: t Serde.Ser.t

  val to_json_string: t -> (string, Serde.error) result

  val to_json: t -> Data.Json.t
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
    | InvalidConnectionStringFormat
    | InvalidUri

  val default: unit -> t

  val parse_error_to_string: parse_error -> string

  val from_string: string -> (t, parse_error) result
end

module Internal: sig
  module Protocol: module type of Protocol
end

module Driver: Sqlx_driver.Driver.Intf with type config = Config.t
