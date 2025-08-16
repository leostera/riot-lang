(** JSON-RPC 2.0 Server Implementation *)

type handler = Jsonrpc.params -> (Json.t, Jsonrpc.error) result

type config = {
  methods : (string, handler) Hashtbl.t;
  on_notification : (string -> Jsonrpc.params -> unit) option;
  on_error : (string -> unit) option;
}

let create () = {
  methods = Hashtbl.create 32;
  on_notification = None;
  on_error = None;
}

let register_method config method_name handler =
  Hashtbl.replace config.methods method_name handler

let set_notification_handler config handler =
  { config with on_notification = Some handler }

let handle_request config (req : Jsonrpc.request) =
  (* Check JSON-RPC version *)
  if req.jsonrpc <> "2.0" then
    match req.id with
    | Some id -> 
        Some (Jsonrpc.make_response 
          ~error:(Jsonrpc.make_error ~code:Jsonrpc.InvalidRequest 
            ~message:"Invalid JSON-RPC version" ()) 
          ~id ())
    | None -> None  (* Notification with wrong version - ignore *)
  else
    (* Check if it's a notification *)
    match req.id with
    | None ->
        (* Handle notification - no response *)
        (match config.on_notification with
        | Some handler -> handler req.method_ req.params
        | None -> ());
        None
    | Some id ->
        (* Handle regular request *)
        match Hashtbl.find_opt config.methods req.method_ with
        | None ->
            (* Method not found *)
            Some (Jsonrpc.make_response 
              ~error:(Jsonrpc.make_error ~code:Jsonrpc.MethodNotFound 
                ~message:(Printf.sprintf "Method not found: %s" req.method_) ()) 
              ~id ())
        | Some handler ->
            (* Execute handler *)
            match handler req.params with
            | Ok result ->
                Some (Jsonrpc.make_response ~result ~id ())
            | Error err ->
                Some (Jsonrpc.make_response ~error:err ~id ())

let handle_batch config requests =
  List.filter_map (handle_request config) requests

let handle_json config json =
  (* Try to parse as single request first *)
  match Jsonrpc.request_of_json json with
  | Ok req -> 
      Option.map Jsonrpc.response_to_json (handle_request config req)
  | Error _ ->
      (* Try as batch request *)
      match json with
      | Json.Array items ->
          let responses = List.filter_map (fun item ->
            match Jsonrpc.request_of_json item with
            | Ok req -> handle_request config req
            | Error _ -> 
                (* Invalid request in batch *)
                Some (Jsonrpc.make_response 
                  ~error:(Jsonrpc.make_error ~code:Jsonrpc.InvalidRequest 
                    ~message:"Invalid request in batch" ()) 
                  ~id:Jsonrpc.Null ())
          ) items in
          if responses = [] then None
          else Some (Json.Array (List.map Jsonrpc.response_to_json responses))
      | _ ->
          (* Invalid JSON-RPC *)
          Some (Jsonrpc.response_to_json (Jsonrpc.make_response 
            ~error:(Jsonrpc.make_error ~code:Jsonrpc.ParseError 
              ~message:"Parse error" ()) 
            ~id:Jsonrpc.Null ()))

let handle_string config str =
  match Json.of_string str with
  | Ok json -> 
      Option.map Json.to_string (handle_json config json)
  | Error e ->
      (* JSON parse error *)
      Some (Json.to_string (Jsonrpc.response_to_json (Jsonrpc.make_response 
        ~error:(Jsonrpc.make_error ~code:Jsonrpc.ParseError 
          ~message:(Printf.sprintf "JSON parse error: %s" e) ()) 
        ~id:Jsonrpc.Null ())))