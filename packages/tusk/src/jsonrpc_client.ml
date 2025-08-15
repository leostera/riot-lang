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
  
  (* Helpers for streaming protocols *)
  val is_streaming_response : response -> bool
  val is_final_response : response -> bool
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

(** Send a request and get a response *)
let call (type req res) (client : (req, res) t) (request : req) : (res, string) result =
  let Client { transport_mod; transport; protocol } = client in
  let module P = (val protocol : Protocol with type request = req and type response = res) in
  let module T = (val transport_mod : Transport with type t = _) in
  
  (* Serialize and send request *)
  let request_str = P.serialize_request request ^ "\n" in
  match T.send transport request_str with
  | Error e -> Error (Printf.sprintf "Failed to send: %s" e)
  | Ok () ->
      (* Receive and deserialize response *)
      match T.receive transport with
      | Error e -> Error (Printf.sprintf "Failed to receive: %s" e)
      | Ok response_str -> P.deserialize_response response_str

(** Call with streaming support *)
let call_streaming (type req res) (client : (req, res) t) (request : req) : (res * res list, string) result =
  let Client { transport_mod; transport; protocol } = client in
  let module P = (val protocol : Protocol with type request = req and type response = res) in
  let module T = (val transport_mod : Transport with type t = _) in
  
  (* Send initial request and get first response *)
  match call client request with
  | Error e -> Error e
  | Ok first_response ->
      if not (P.is_streaming_response first_response) then
        (* Not streaming, return single response *)
        Ok (first_response, [])
      else
        (* Collect streaming responses *)
        let rec collect acc =
          match T.receive transport with
          | Error e -> Error (Printf.sprintf "Stream error: %s" e)
          | Ok response_str ->
              match P.deserialize_response response_str with
              | Error e -> Error e
              | Ok response ->
                  if P.is_final_response response then
                    Ok (response, List.rev (first_response :: acc))
                  else
                    collect (response :: acc)
        in
        collect []

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