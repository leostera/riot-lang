(** JSON-RPC 2.0 Server Implementation *)

(** Method handler type - takes params and returns result or error *)
type handler = Jsonrpc.params -> (Json.t, Jsonrpc.error) result

(** Server configuration *)
type config = {
  methods : (string, handler) Hashtbl.t;
  on_notification : (string -> Jsonrpc.params -> unit) option;
  on_error : (string -> unit) option;
}

(** Create a new server configuration *)
val create : unit -> config

(** Register a method handler *)
val register_method : config -> string -> handler -> unit

(** Register a notification handler *)
val set_notification_handler : config -> (string -> Jsonrpc.params -> unit) -> config

(** Process a JSON-RPC request and return a response *)
val handle_request : config -> Jsonrpc.request -> Jsonrpc.response option

(** Process a batch request and return batch response *)
val handle_batch : config -> Jsonrpc.batch_request -> Jsonrpc.batch_response

(** Process raw JSON input and return JSON response *)
val handle_json : config -> Json.t -> Json.t option

(** Process raw string input and return string response *)
val handle_string : config -> string -> string option