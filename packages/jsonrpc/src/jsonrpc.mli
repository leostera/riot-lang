(** JSON-RPC 2.0 Protocol Implementation 
    
    This module provides a complete implementation of the JSON-RPC 2.0 specification
    as defined at https://www.jsonrpc.org/specification
*)

(** JSON-RPC version constant *)
val version : string

(** Request ID type - can be string, number, or null *)
type id = 
  | String of string
  | Number of int
  | Null

(** Parameters can be positional (array) or named (object) *)
type params =
  | Positional of Json.t list
  | Named of (string * Json.t) list
  | NoParams

(** JSON-RPC 2.0 Request *)
type request = {
  jsonrpc : string;  (* Must be "2.0" *)
  method_ : string;  (* Method name to invoke *)
  params : params;   (* Optional parameters *)
  id : id option;    (* Optional ID (None for notifications) *)
}

(** Standard error codes *)
type error_code =
  | ParseError           (* -32700 *)
  | InvalidRequest       (* -32600 *)
  | MethodNotFound       (* -32601 *)
  | InvalidParams        (* -32602 *)
  | InternalError        (* -32603 *)
  | ServerError of int   (* -32000 to -32099 *)
  | ApplicationError of int  (* Application defined *)

(** JSON-RPC 2.0 Error *)
type error = {
  code : error_code;
  message : string;
  data : Json.t option;
}

(** JSON-RPC 2.0 Response *)
type response = {
  jsonrpc : string;    (* Must be "2.0" *)
  result : Json.t option;  (* Success result *)
  error : error option;     (* Error result *)
  id : id;                  (* Must match request ID *)
}

(** Batch request/response types *)
type batch_request = request list
type batch_response = response list

(** Convert types to/from JSON *)
val request_to_json : request -> Json.t
val request_of_json : Json.t -> (request, string) result

val response_to_json : response -> Json.t
val response_of_json : Json.t -> (response, string) result

val error_to_json : error -> Json.t
val error_of_json : Json.t -> (error, string) result

val id_to_json : id -> Json.t
val id_of_json : Json.t -> (id, string) result

(** Helper functions *)
val make_request : method_:string -> ?params:params -> ?id:id -> unit -> request
val make_response : ?result:Json.t -> ?error:error -> id:id -> unit -> response
val make_error : code:error_code -> message:string -> ?data:Json.t -> unit -> error
val make_notification : method_:string -> ?params:params -> unit -> request

(** Error code conversions *)
val error_code_to_int : error_code -> int
val int_to_error_code : int -> error_code

(** Check if a request is a notification (no id) *)
val is_notification : request -> bool