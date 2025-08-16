(** JSON-RPC 2.0 Client Implementation *)

(** Transport interface - handles sending/receiving strings *)
module type Transport = sig
  type t
  val send : t -> string -> (unit, string) result
  val receive : t -> (string, string) result
  val close : t -> unit
end

(** Client type *)
type t

(** Create a new client with a transport *)
val create : (module Transport with type t = 'a) -> 'a -> t

(** Generate a unique ID for requests *)
val generate_id : unit -> Jsonrpc.id

(** Send a request and wait for response *)
val call : t -> method_:string -> ?params:Jsonrpc.params -> unit -> (Json.t, Jsonrpc.error) result

(** Send a notification (no response expected) *)
val notify : t -> method_:string -> ?params:Jsonrpc.params -> unit -> (unit, string) result

(** Send a batch of requests *)
val call_batch : t -> Jsonrpc.request list -> (Jsonrpc.response list, string) result

(** Low-level: send a raw request *)
val send_request : t -> Jsonrpc.request -> (unit, string) result

(** Low-level: receive a raw response *)
val receive_response : t -> (Jsonrpc.response, string) result

(** Close the client connection *)
val close : t -> unit