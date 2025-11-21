open Std

type message_type =
  | Startup
  | Query
  | Terminate
  | PasswordMessage
  | Parse
  | Bind
  | Execute
  | Describe
  | Close
  | Sync

type backend_message =
  | AuthenticationOk
  | AuthenticationCleartextPassword
  | AuthenticationMD5Password of bytes
  | BackendKeyData of { process_id : int; secret_key : int }
  | ParameterStatus of { name : string; value : string }
  | ReadyForQuery of char
  | RowDescription of field list
  | DataRow of string option list  (* None = NULL, Some s = value *)
  | CommandComplete of string
  | ErrorResponse of (char * string) list
  | NoticeResponse of (char * string) list
  | ParseComplete
  | BindComplete
  | CloseComplete
  | NoData
  | ParameterDescription of int list
  | EmptyQueryResponse

and field = {
  name : string;
  table_oid : int;
  column_attr : int;
  type_oid : int;
  type_size : int;
  type_modifier : int;
  format_code : int;
}

module TypeOid : sig
  val bool : int
  val bytea : int
  val char : int
  val int8 : int
  val int2 : int
  val int4 : int
  val text : int
  val oid : int
  val json : int
  val float4 : int
  val float8 : int
  val varchar : int
  val date : int
  val time : int
  val timestamp : int
  val timestamptz : int
  val interval : int
  val numeric : int
  val uuid : int
  val jsonb : int
end

module Writer : sig
  val startup_message :
    user:string -> database:string -> application_name:string option -> string

  val query_message : string -> string

  val parse_message :
    statement_name:string -> query:string -> param_types:int list -> string

  val bind_message :
    portal_name:string -> statement_name:string -> params:string list -> string

  val execute_message : portal_name:string -> max_rows:int -> string
  val describe_message : what:char -> name:string -> string
  val sync_message : unit -> string
  val close_message : what:char -> name:string -> string
  val terminate_message : unit -> string
end

module Reader : sig
  val parse_backend_message : int -> int -> bytes -> backend_message
end
