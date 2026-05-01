(**
   Typed JSON-RPC 2.0 helpers.

   Use this package when you want transport-neutral request, response, client,
   and server helpers without hand-assembling JSON objects.
*)
open Std
open Std.Data

(**
   JSON-RPC protocol version string.

   Example:
   ```ocaml
   Jsonrpc.version = "2.0"
   ```
*)
val version: string

(** Request and response identifier. *)
type id =
  | String of string
  | Number of int
  | Null
(** Method parameters. *)
type params =
  | Positional of Json.t list
  (** Positional parameters encoded as a JSON array. *)
  | Named of (string * Json.t) list
  (** Named parameters encoded as a JSON object. *)
  | NoParams
(** Typed request lowered into a JSON-RPC method name and params. *)
type prerequest = {
  (** Method name to invoke. *)
  method_: string;
  (** Method parameters. *)
  params: params;
}
(** JSON-RPC request or notification. *)
type request = {
  (** Protocol version. *)
  jsonrpc: string;
  (** Method name to invoke. *)
  method_: string;
  (** Method parameters. *)
  params: params;
  (** Request identifier, or [None] for notifications. *)
  id: id option;
}
(** Client-side JSON-RPC error. *)
type error =
  | ParseError of { raw_input: string; parse_error: string }
  (** Client failed to parse JSON or JSON-RPC structure. *)
  | InvalidRequest of {
      request_json: Json.t;
      reason: string;
    }
  (** Malformed JSON-RPC request or response. *)
  | MethodNotFound of { method_name: string }
  (** Requested method does not exist. *)
  | InvalidParams of {
      method_name: string;
      params: params;
      reason: string;
    }
  (** Method parameters could not be decoded. *)
  | InternalError of { context: string; details: string }
  (** Internal client error such as transport or serialization failure. *)
  | UnknownServerError of {
      code: int;
      message: string;
      data: Json.t option;
    }

(** Server returned an untyped JSON-RPC error object. *)

(** JSON-RPC response wrapper. *)
type 'res response = {
  (** Protocol version. *)
  jsonrpc: string;
  (** Typed response value. *)
  result: 'res;
  (** Identifier matching the originating request. *)
  id: id;
}
(** Batch request containing multiple JSON-RPC requests. *)
type batch_request = request list
(** Batch response containing multiple JSON-RPC responses. *)
type 'res batch_response = 'res response list

(** Encode a request as JSON. *)
val request_to_json: request -> Json.t

(** Decode a request from JSON. *)
val request_of_json: Json.t -> (request, string) result

(** Encode an identifier as JSON. *)
val id_to_json: id -> Json.t

(** Decode an identifier from JSON. *)
val id_of_json: Json.t -> (id, string) result

(**
   Create a JSON-RPC request.

   Use this when you need a plain request value without going through a typed
   [ApplicationProtocol].
*)
val request: method_:string -> ?params:params -> ?id:id -> unit -> request

(**
   Create a JSON-RPC notification.

   Notifications do not carry an identifier and therefore do not expect a
   response.
*)
val notification: method_:string -> ?params:params -> unit -> request

(**
   Return `true` if the request is a notification.

   Example:
   ```ocaml
   Jsonrpc.is_notification (Jsonrpc.notification ~method_:"ping" ()) = true
   ```
*)
val is_notification: request -> bool

(** Create a successful response wrapper. *)
val ok: ?id:id -> 'res -> 'res response

(**
   Bridge between typed application values and JSON-RPC wire values.

   Implement this once for your protocol, then reuse it on both the client and
   server side.
*)
module type ApplicationProtocol = sig
  (** Application-specific request type. *)
  type request
  (** Application-specific response type. *)
  type response

  (** Encode a typed response as JSON. *)
  val response_to_json: response -> Json.t

  (** Decode a typed response from JSON. *)
  val response_of_json: Json.t -> (response, Json.t) result

  (** Lower a typed request into a method name and params. *)
  val request_to_params: request -> prerequest

  (** Decode a typed request from a method name and params. *)
  val request_of_params: string -> params -> (request, Json.t) result
end

(** Typed JSON-RPC client helpers. *)
module Client: sig
  (**
     Raw transport used by the client.

     Implement this for the transport you already have, such as a socket,
     HTTP stream, or WebSocket connection.
  *)
  module type Transport = sig
    (** Transport connection handle. *)
    type t

    (** Send one raw JSON-RPC message. *)
    val send: t -> string -> (unit, string) result

    (** Receive one raw JSON-RPC message. *)
    val receive: t -> (string, string) result

    (** Close the transport. *)
    val close: t -> unit
  end

  (** Client handle parameterized by typed request and response values. *)
  type ('request, 'response) t

  (** Create a typed client from a transport and protocol adapter. *)
  val create:
    transport:(module Transport with type t = 'transport) ->
    protocol:(module ApplicationProtocol with type request = 'req and type response = 'res) ->
    'transport ->
    ('req, 'res) t

  (** Send a raw method/params call and wait for the typed response. *)
  val call: ('req, 'res) t -> method_:string -> ?params:params -> unit -> ('res, error) result

  (** Send a raw notification. *)
  val notify: ('req, 'res) t -> method_:string -> ?params:params -> unit -> (unit, error) result

  (** Send a batch of typed requests and wait for all responses. *)
  val call_batch: ('req, 'res) t -> 'req list -> ('res response list, error) result

  (** Send a typed request without waiting for the response yet. *)
  val send_request: ('req, 'res) t -> 'req -> (unit, error) result

  (**
     Receive the next typed response.

     Use this after [send_request] when you want to manage request and
     response timing separately.
  *)
  val receive_response: ('req, 'res) t -> ('res response, error) result

  (** Close the client and underlying transport. *)
  val close: ('req, 'res) t -> unit
end

(** Typed JSON-RPC server helpers. *)
module Server: sig
  (**
     Handler for one JSON-RPC method.

     The handler receives a reply callback and the decoded request payload.
  *)
  type ('req, 'res) handler = {
    method_: string;
    fn: ('res -> unit) -> 'req -> unit;
  }
  (** Typed server configuration. *)
  type ('request, 'response) t

  (** Create a server from a protocol adapter and method handlers. *)
  val create:
    protocol:(module ApplicationProtocol with type request = 'req and type response = 'res) ->
    methods:('req, 'res) handler list ->
    ('req, 'res) t

  (**
     Process one raw JSON-RPC message.

     Use the reply callback to emit zero or more raw response strings.
  *)
  val handle_message: ('req, 'res) t -> (string -> unit) -> string -> unit
end
