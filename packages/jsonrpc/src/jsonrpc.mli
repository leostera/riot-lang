(** JSON-RPC 2.0 Protocol Implementation

    This module provides a complete, type-safe implementation of the JSON-RPC
    2.0 specification as defined at https://www.jsonrpc.org/specification.

    The implementation is parameterized by application-specific protocol types,
    allowing for fully typed request/response handling while maintaining
    JSON-RPC 2.0 compliance. *)
open Std
open Std.Data

val version: string

(** JSON-RPC version constant ("2.0") *)
type id =
  | String of string
  | Number of int
  | Null
(** Request/response ID type - can be string, number, or null. Used to
          correlate requests with responses. *)
(** Method parameters - can be positional (array), named (object), or absent *)
type params =
  | Positional of Json.t list
  (** Positional parameters as JSON array *)
  | Named of (string * Json.t) list
  (** Named parameters as JSON object *)
  | NoParams
(** No parameters *)
type prerequest = {
  method_: string;  (** Method name to invoke *)
  params: params;  (** Method parameters *)
}
(** Pre-request type used by ApplicationProtocol for typed → JSON-RPC conversion
*)
type request = {
  jsonrpc: string;  (** Must be "2.0" *)
  method_: string;  (** Method name to invoke *)
  params: params;  (** Method parameters *)
  id: id option;  (** Request ID (None for notifications) *)
}
(** JSON-RPC 2.0 Request - represents both regular requests and notifications *)
(** Standard JSON-RPC 2.0 error codes *)
type error =
  | ParseError of { raw_input: string; parse_error: string }
  (** Client failed to parse JSON or JSON-RPC structure *)
  | InvalidRequest of { request_json: Json.t; reason: string }
  (** Received malformed JSON-RPC request/response *)
  | MethodNotFound of { method_name: string }
  (** Method does not exist *)
  | InvalidParams of { method_name: string; params: params; reason: string }
  (** Invalid method parameters *)
  | InternalError of { context: string; details: string }
  (** Internal client error (transport, serialization, etc.) *)
  | UnknownServerError of { code: int; message: string; data: Json.t option }
(** Server returned a JSON-RPC error object that couldn't be parsed into a
          typed response variant *)
(** Client-side errors with rich context information. Includes
    UnknownServerError for when the server sends a JSON-RPC error that we don't
    have a typed response variant for. *)
type 'res response = {
  jsonrpc: string;  (** Must be "2.0" *)
  result: 'res;  (** Response value from server *)
  id: id;  (** ID matching the request *)
}
(** JSON-RPC 2.0 Response - contains the server's response. Server errors are
    part of the response type (as variants), not Error results. *)
type batch_request = request list
(** Batch request - multiple requests sent as array *)
type 'res batch_response = 'res response list
(** Batch response - array of responses matching batch request *)
val request_to_json: request -> Json.t
(** Convert a request to JSON representation *)
val request_of_json: Json.t -> (request, string) result
(** Parse a request from JSON, returns error message on failure *)
val id_to_json: id -> Json.t
(** Convert an ID to JSON representation *)
val id_of_json: Json.t -> (id, string) result
(** Parse an ID from JSON, returns error message on failure *)
val request: method_:string -> ?params:params -> ?id:id -> unit -> request
(** Create a request with the given method, optional parameters, and optional ID
*)
val notification: method_:string -> ?params:params -> unit -> request
(** Create a notification (request with no ID) *)
val is_notification: request -> bool
(** Check if a request is a notification (has no ID) *)

(** ApplicationProtocol interface - bridges typed application values and
    JSON-RPC.

    This module type defines how to convert between your application's typed
    request/response types and the JSON representation used by JSON-RPC. Both
    client and server use the same protocol to ensure type safety across the RPC
    boundary.

    Example:
    {[
      module MyProtocol : ApplicationProtocol = struct
        type request = GetUser of int | CreateUser of string * string
        type response = User of user | Success | Error of string

        let request_to_params = function
          | GetUser id ->
              { method_ = "getUser"; params = Positional [ Json.int id ] }
          | CreateUser (name, email) ->
              {
                method_ = "createUser";
                params =
                  Named
                    [ ("name", Json.string name); ("email", Json.string email) ];
              }

        let response_to_json = function
          | User u ->
              Json.obj
                [ ("type", Json.string "user"); ("data", user_to_json u) ]
          | Success -> Json.obj [ ("type", Json.string "success") ]
          | Error msg ->
              Json.obj
                [ ("type", Json.string "error"); ("message", Json.string msg) ]

        (* ... implement response_of_json and request_of_params ... *)
      end
    ]} *)
val ok: ?id:id -> 'res -> 'res response
(** Create a response with an optional ID (defaults to Null) *)
module type ApplicationProtocol = sig
  type request
  (** Application-specific request type *)
  type response
  (** Application-specific response type. This should include all possible
      server states including errors (e.g., NotFound, BuildFailed, etc.) as
      variants. These are not "errors" in the RPC sense - they're valid
      responses from the server. *)
  val response_to_json: response -> Json.t
  (** Convert typed response to JSON for transmission *)
  val response_of_json: Json.t -> (response, Json.t) result
  (** Parse JSON into typed response, returns error as Json.t on failure *)
  val request_to_params: request -> prerequest
  (** Convert typed request to method name and parameters *)
  val request_of_params: string -> params -> (request, Json.t) result
  (** Parse parameters into typed request for the given method name, returns
      error as Json.t on failure *)
end

module Client: sig
  (** Type-safe JSON-RPC 2.0 Client Implementation

      The client is parameterized by request and response types through an
      ApplicationProtocol, ensuring type safety for all RPC calls. *)
  module type Transport = sig
    (** Transport interface for sending/receiving JSON-RPC messages.
        Implementations might use TCP, HTTP, WebSockets, etc. *)
    type t
    (** Transport connection type *)
    val send: t -> string -> (unit, string) result
    (** Send a string message over the transport *)
    val receive: t -> (string, string) result
    (** Receive a string message from the transport *)
    val close: t -> unit
    (** Close the transport connection *)
  end

  type ('request, 'response) t
  (** Client type parameterized by application request/response types *)
  val create:
    transport:(module Transport with type t = 'transport) ->
    protocol:(module ApplicationProtocol with type request = 'req and type response = 'res) ->
    'transport ->
    ('req, 'res) t
  (** Create a new client with the given transport and protocol. The protocol
      defines how to convert between typed values and JSON. *)
  val call: ('req, 'res) t -> method_:string -> ?params:params -> unit -> ('res, error) result
  (** Send a raw JSON-RPC request and wait for response.
      - Ok(response): Server successfully processed the request and returned a
        response
      - Error(error): Client-side failure (network, parsing, etc.)

      Note: Server errors/failures are part of the response type, not Error
      results. *)
  val notify: ('req, 'res) t -> method_:string -> ?params:params -> unit -> (unit, error) result
  (** Send a notification (no response expected). Notifications are
      fire-and-forget - the server will not send a response. *)
  val call_batch: ('req, 'res) t -> 'req list -> ('res response list, error) result
  (** Send a batch of typed requests and receive batch response. All requests
      are sent together and responses are returned together. Useful for reducing
      round-trip latency. *)
  val send_request: ('req, 'res) t -> 'req -> (unit, error) result
  (** Send a typed request without waiting for response. Use with
      receive_response for streaming or async patterns. *)
  val receive_response: ('req, 'res) t -> ('res response, error) result
  (** Receive and parse a typed response. Use after send_request to complete the
      request/response cycle. *)
  val close: ('req, 'res) t -> unit
  (** Close the client and underlying transport connection *)
end
(** Helper functions for creating responses - REMOVED: use ok and error above
    instead *)
module Server: sig
  (** JSON-RPC 2.0 Server Implementation *)
  type ('req, 'res) handler = {
    method_: string;
    fn: ('res -> unit) -> 'req -> unit;
  }
  (** Method handler type - takes reply function and typed request *)
  type ('request, 'response) t
  (** Server configuration *)
  val create:
    protocol:(module ApplicationProtocol with type request = 'req and type response = 'res) ->
    methods:('req, 'res) handler list ->
    ('req, 'res) t
  (** Create a new server with the given protocol and method handlers. Each
      handler will be called when its method name is invoked. The protocol
      defines how to convert between typed values and JSON. *)
  val handle_message: ('req, 'res) t -> (string -> unit) -> string -> unit
  (** Process a JSON-RPC message string and call the appropriate handler. The
      reply function will be called with a JSON-RPC response string ready to
      send. For notifications, handlers are called but no reply is sent.

      The server automatically: 1. Converts the typed response to JSON using the
      protocol 2. Wraps it in a JSON-RPC response envelope 3. Passes the
      complete JSON string to the reply function *)
end

(** {2 Example Usage}

    {[
      (* Define your protocol *)
      module MyProtocol = struct
        type request = Ping | GetStatus | Shutdown
        type response = Pong | Status of string | Ok

        let request_to_params = function
          | Ping -> { method_ = "ping"; params = NoParams }
          | GetStatus -> { method_ = "getStatus"; params = NoParams }
          | Shutdown -> { method_ = "shutdown"; params = NoParams }

        let response_to_json = function
          | Pong -> Json.string "pong"
          | Status s -> Json.obj ["status", Json.string s]
          | Ok -> Json.string "ok"

        (* ... implement response_of_json and request_of_params ... *)
      end

      (* Create client *)
      let client = Client.create
        ~transport:(module TcpTransport)
        ~protocol:(module MyProtocol)
        tcp_connection

      (* Send typed request *)
      let () = Client.send_request client Ping in
      let response = Client.receive_response client in
      match response with
      | Ok { result = Ok Pong; _ } -> print_endline "Got pong!"
      | Ok { result = Error err; _ } -> println "Error: %s" err.message
      | Error e -> println "Protocol error: %s" e

      (* Create server *)
      let server = Server.create
        ~protocol:(module MyProtocol)
        ~methods:[
          { method_ = "ping";
            fn = fun reply _params -> reply Pong };
          { method_ = "getStatus";
            fn = fun reply _params -> reply (Status "running") };
        ]
    ]} *)
