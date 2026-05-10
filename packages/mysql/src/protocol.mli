open Std

type parse_error =
  | Truncated of string
  | InvalidPacketLength of int
  | InvalidPacketSequence of { expected: int; actual: int }
  | InvalidLengthEncodedInteger of string
  | InvalidCString of string
  | UnknownPacket of int
  | UnsupportedPacket of string
  | InvalidPayload of string

val parse_error_to_string: parse_error -> string

module Capability: sig
  type t = int

  val long_password: int

  val found_rows: int

  val long_flag: int

  val connect_with_db: int

  val protocol_41: int

  val ssl: int

  val transactions: int

  val secure_connection: int

  val multi_results: int

  val ps_multi_results: int

  val plugin_auth: int

  val connect_attrs: int

  val plugin_auth_lenenc_client_data: int

  val session_track: int

  val deprecate_eof: int

  val has: int -> int -> bool

  val default_client: ?database:bool -> ?ssl:bool -> unit -> int
end

module Packet: sig
  type t = { sequence: int; payload: string }

  val max_payload_length: int

  val make: sequence:int -> payload:string -> t

  val encode: t -> string list

  val decode_one: string -> (t, parse_error) result
end

module ColumnType: sig
  type t =
    | Tiny
    | Short
    | Long
    | Float
    | Double
    | Null
    | Timestamp
    | LongLong
    | Int24
    | Date
    | Time
    | DateTime
    | Year
    | VarChar
    | Json
    | NewDecimal
    | Enum
    | Set
    | TinyBlob
    | MediumBlob
    | LongBlob
    | Blob
    | VarString
    | String
    | Geometry
    | Unknown of int

  val from_int: int -> t

  val to_int: t -> int

  val to_string: t -> string
end

module ServerStatus: sig
  type t = { flags: int; in_transaction: bool; autocommit: bool; more_results: bool }

  val from_int: int -> t
end

module Error: sig
  type t = {
    code: int;
    sql_state: string option;
    message: string;
  }

  val to_string: t -> string

  val serialize: t Serde.Ser.t

  val to_json_string: t -> (string, Serde.error) result

  val to_json: t -> Data.Json.t
end

type ok_packet = {
  affected_rows: int64;
  last_insert_id: int64;
  status: ServerStatus.t;
  warnings: int;
  info: string;
}
type handshake = {
  protocol_version: int;
  server_version: string;
  connection_id: int;
  auth_plugin_data: string;
  capability_flags: int;
  character_set: int;
  status_flags: int;
  auth_plugin_name: string option;
}
type column_definition = {
  schema: string;
  table: string;
  org_table: string;
  name: string;
  org_name: string;
  character_set: int;
  column_length: int;
  column_type: ColumnType.t;
  flags: int;
  decimals: int;
}
type prepare_ok = { statement_id: int; num_columns: int; num_params: int; warnings: int }
type row_value =
  | Null
  | Value of string

module Auth: sig
  val mysql_native_password: password:string -> seed:string -> string

  val caching_sha2_password: password:string -> seed:string -> string
end

module Reader: sig
  val parse_handshake: string -> (handshake, parse_error) result

  val parse_ok_packet: string -> (ok_packet, parse_error) result

  val parse_error_packet: string -> (Error.t, parse_error) result

  val parse_column_definition: string -> (column_definition, parse_error) result

  val parse_prepare_ok: string -> (prepare_ok, parse_error) result

  val parse_text_row:
    column_definition list ->
    string ->
    ((string * Sqlx_driver.Value.t) list, parse_error) result

  val parse_binary_row:
    column_definition list ->
    string ->
    ((string * Sqlx_driver.Value.t) list, parse_error) result
end

module Writer: sig
  val packet: sequence:int -> payload:string -> string list

  val ssl_request: capability_flags:int -> max_packet_size:int -> character_set:int -> string

  val handshake_response:
    capability_flags:int ->
    max_packet_size:int ->
    character_set:int ->
    user:string ->
    database:string option ->
    auth_response:string ->
    auth_plugin:string ->
    string

  val com_query: string -> string

  val com_stmt_prepare: string -> string

  val com_stmt_execute: statement_id:int -> params:Sqlx_driver.Value.t list -> string

  val com_stmt_close: int -> string

  val com_quit: unit -> string
end
