(** JSON-RPC 2.0 Protocol Implementation

    This module provides a complete implementation of the JSON-RPC 2.0
    specification as defined at https://www.jsonrpc.org/specification *)

val version : string
(** JSON-RPC version constant *)

(** Request ID type - can be string, number, or null *)
type id = String of string | Number of int | Null

(** Parameters can be positional (array) or named (object) *)
type params =
  | Positional of Json.t list
  | Named of (string * Json.t) list
  | NoParams

type request = {
  jsonrpc : string; (* Must be "2.0" *)
  method_ : string; (* Method name to invoke *)
  params : params; (* Optional parameters *)
  id : id option; (* Optional ID (None for notifications) *)
}
(** JSON-RPC 2.0 Request *)

(** Standard error codes *)
type error_code =
  | ParseError (* -32700 *)
  | InvalidRequest (* -32600 *)
  | MethodNotFound (* -32601 *)
  | InvalidParams (* -32602 *)
  | InternalError (* -32603 *)
  | ServerError of int (* -32000 to -32099 *)
  | ApplicationError of int (* Application defined *)

type error = { code : error_code; message : string; data : Json.t option }
(** JSON-RPC 2.0 Error *)

type response = {
  jsonrpc : string; (* Must be "2.0" *)
  result : Json.t option; (* Success result *)
  error : error option; (* Error result *)
  id : id; (* Must match request ID *)
}
(** JSON-RPC 2.0 Response *)

type batch_request = request list
(** Batch request/response types *)

type batch_response = response list

val request_to_json : request -> Json.t
(** Convert types to/from JSON *)

val request_of_json : Json.t -> (request, string) result
val response_to_json : response -> Json.t
val response_of_json : Json.t -> (response, string) result
val error_to_json : error -> Json.t
val error_of_json : Json.t -> (error, string) result
val id_to_json : id -> Json.t
val id_of_json : Json.t -> (id, string) result

val make_request : method_:string -> ?params:params -> ?id:id -> unit -> request
(** Helper functions *)

val make_response : ?result:Json.t -> ?error:error -> id:id -> unit -> response

val make_error :
  code:error_code -> message:string -> ?data:Json.t -> unit -> error

val make_notification : method_:string -> ?params:params -> unit -> request

val error_code_to_int : error_code -> int
(** Error code conversions *)

val int_to_error_code : int -> error_code

val is_notification : request -> bool
(** Check if a request is a notification (no id) *)

module Client : sig
  (** JSON-RPC 2.0 Client Implementation *)

  (** Transport interface - handles sending/receiving strings *)
  module type Transport = sig
    type t

    val send : t -> string -> (unit, string) result
    val receive : t -> (string, string) result
    val close : t -> unit
  end

  type t
  (** Client type *)

  val create : (module Transport with type t = 'a) -> 'a -> t
  (** Create a new client with a transport *)

  val generate_id : unit -> id
  (** Generate a unique ID for requests *)

  val call :
    t -> method_:string -> ?params:params -> unit -> (Json.t, error) result
  (** Send a request and wait for response *)

  val notify :
    t -> method_:string -> ?params:params -> unit -> (unit, string) result
  (** Send a notification (no response expected) *)

  val call_batch : t -> request list -> (response list, string) result
  (** Send a batch of requests *)

  val send_request : t -> request -> (unit, string) result
  (** Low-level: send a raw request *)

  val receive_response : t -> (response, string) result
  (** Low-level: receive a raw response *)

  val close : t -> unit
  (** Close the client connection *)
end

(** Helper functions for creating responses *)

val result : result:Json.t -> id:id -> response
(** Create a successful response with result *)

val error_response : error:error -> id:id -> response
(** Create an error response *)

module Server : sig
  (** JSON-RPC 2.0 Server Implementation *)

  type handler = (response -> unit) -> params -> unit
  (** Method handler type - takes reply function and params *)

  type config
  (** Server configuration *)

  val create_config : unit -> config
  (** Create a new server configuration *)

  val create : methods:(string * handler) list -> config
  (** Create a server configuration with methods *)

  val register_method : config -> string -> handler -> unit
  (** Register a method handler *)

  val set_notification_handler : config -> (string -> params -> unit) -> config
  (** Register a notification handler *)

  val handle_message : config -> (response -> unit) -> string -> unit
  (** Handle a single JSON-RPC message string with a reply function *)
end
