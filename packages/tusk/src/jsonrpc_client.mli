(** Generic JSON-RPC client with first-class modules *)

(** Transport signature - handles sending/receiving strings *)
module type Transport = sig
  type t
  val send : t -> string -> (unit, string) result
  val receive : t -> (string, string) result
  val close : t -> unit
end

(** Protocol signature - handles serialization of types to/from JSON strings *)
module type Protocol = sig
  type request
  type response
  
  val serialize_request : request -> string
  val serialize_response : response -> string
  val deserialize_response : string -> (response, string) result
  val deserialize_request : string -> (request, string) result
  
  val is_streaming_response : response -> bool
  val is_final_response : response -> bool
end

(** Client type parametrized by request and response types *)
type ('req, 'res) t

val create : 
  'transport 'req 'res.
  transport:(module Transport with type t = 'transport) -> 
  protocol:(module Protocol with type request = 'req and type response = 'res) ->
  'transport ->
  ('req, 'res) t
(** Create a new JSON-RPC client with the given transport and protocol.
    Pass an instance of the transport (e.g., a connected TCP client or stdio handle).
    The client type captures the request and response types from the protocol.
    
    Example:
    {[
      let tcp_conn = TcpTransport.connect ~host:"localhost" ~port:8080 in
      let client = Jsonrpc_client.create 
        ~transport:(module TcpTransport) 
        ~protocol:(module MyProtocol)
        tcp_conn
    ]} *)

val call : ('req, 'res) t -> 'req -> ('res, string) result
(** Send a request and get a response. Type-safe based on the protocol. *)

val call_streaming : ('req, 'res) t -> 'req -> ('res * 'res list, string) result
(** Send a request and collect streaming responses.
    Returns (final_response, all_streaming_responses) *)

val close : ('req, 'res) t -> unit
(** Close the client connection *)

(** TCP transport implementation *)
module TcpTransport : sig
  include Transport with type t = Miniriot.Net.TcpClient.t
  
  val connect : host:string -> port:int -> (t, string) result
  (** Connect to a TCP server *)
end

(** Stdio transport implementation *)
module StdioTransport : sig
  include Transport with type t = unit
  
  val create : unit -> t
  (** Create a stdio transport instance *)
end
