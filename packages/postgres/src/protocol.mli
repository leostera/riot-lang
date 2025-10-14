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
  | DataRow of string list
  | CommandComplete of string
  | ErrorResponse of (char * string) list
  | NoticeResponse of (char * string) list

and field = {
  name : string;
  table_oid : int;
  column_attr : int;
  type_oid : int;
  type_size : int;
  type_modifier : int;
  format_code : int;
}

module Writer : sig
  val startup_message :
    user:string -> database:string -> application_name:string option -> string

  val query_message : string -> string
  val terminate_message : unit -> string
end

module Reader : sig
  val parse_backend_message : int -> int -> bytes -> backend_message
end
