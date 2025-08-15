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
  
  val serialize_request : request -> Json.t
  val serialize_response : response -> Json.t
  val deserialize_response : Json.t -> (response, string) result
  val deserialize_request : Json.t -> (request, string) result
end

(** Client type parametrized by request and response types *)
type ('req, 'res) t = 
  | Client : {
      transport_mod : (module Transport with type t = 'a);
      transport : 'a;
      protocol : (module Protocol with type request = 'req and type response = 'res);
    } -> ('req, 'res) t

(** Create a new JSON-RPC client *)
let create (type transport req res) 
    ~(transport : (module Transport with type t = transport))
    ~(protocol : (module Protocol with type request = req and type response = res))
    (conn : transport) : (req, res) t =
  Client {
    transport_mod = transport;
    transport = conn;
    protocol = protocol;
  }

(** Send a request *)
let send (type req res) (client : (req, res) t) (request : req) : (unit, string) result =
  let Client { transport_mod; transport; protocol } = client in
  let module P = (val protocol : Protocol with type request = req and type response = res) in
  let module T = (val transport_mod : Transport with type t = _) in
  
  (* Serialize and send request *)
  let request_json = P.serialize_request request in
  let request_str = Json.to_string request_json ^ "\n" in
  match T.send transport request_str with
  | Error e -> Error (Printf.sprintf "Failed to send: %s" e)
  | Ok () -> Ok ()

(** Receive a response *)
let receive (type req res) (client : (req, res) t) : (res, string) result =
  let Client { transport_mod; transport; protocol } = client in
  let module P = (val protocol : Protocol with type request = req and type response = res) in
  let module T = (val transport_mod : Transport with type t = _) in
  
  (* Receive and deserialize response *)
  match T.receive transport with
  | Error e -> Error (Printf.sprintf "Failed to receive: %s" e)
  | Ok response_str -> 
      match Json.of_string response_str with
      | Error e -> Error (Printf.sprintf "JSON parse error: %s" e)
      | Ok json -> P.deserialize_response json

(** Send a request and get a single response *)
let call (type req res) (client : (req, res) t) (request : req) : (res, string) result =
  match send client request with
  | Error e -> Error e
  | Ok () -> receive client

(** Close the client connection *)
let close (type req res) (client : (req, res) t) =
  let Client { transport_mod; transport; _ } = client in
  let module T = (val transport_mod : Transport with type t = _) in
  T.close transport

(** TCP transport implementation *)
module TcpTransport = struct
  type t = Miniriot.Net.TcpClient.t
  
  let connect ~host ~port = 
    match Miniriot.Net.TcpClient.connect ~host ~port with
    | Ok client -> Ok client
    | Error e -> Error (match e with
      | `Connection_refused -> "connection refused"
      | `Closed -> "connection closed"  
      | `System_error s -> s)
  
  let send = Miniriot.Net.TcpClient.send
  let receive = Miniriot.Net.TcpClient.receive
  let close = Miniriot.Net.TcpClient.close
end

(** Stdio transport implementation *)
module StdioTransport = struct
  type t = unit
  
  let create () = ()
  
  let send () data =
    print_string data;
    flush stdout;
    Ok ()
  
  let receive () =
    try Ok (input_line stdin)
    with
    | End_of_file -> Error "EOF"
    | e -> Error (Printexc.to_string e)
  
  let close () = ()
end