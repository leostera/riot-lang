(** JSON-RPC 2.0 Server Implementation *)

type handler = (Common.response -> unit) -> Common.params -> unit

type config = {
  methods : (string, handler) Hashtbl.t;
  on_notification : (string -> Common.params -> unit) option;
  on_error : (string -> unit) option;
}

let create_config () =
  { methods = Hashtbl.create 32; on_notification = None; on_error = None }

let create ~methods =
  let config = create_config () in
  List.iter
    (fun (name, handler) -> Hashtbl.replace config.methods name handler)
    methods;
  config

let register_method config method_name handler =
  Hashtbl.replace config.methods method_name handler

let set_notification_handler config handler =
  { config with on_notification = Some handler }

let handle_request config reply (req : Common.request) =
  (* Check JSON-RPC version *)
  if req.jsonrpc <> "2.0" then
    match req.id with
    | Some id ->
        reply
          (Common.make_response
             ~error:
               (Common.make_error ~code:Common.InvalidRequest
                  ~message:"Invalid JSON-RPC version" ())
             ~id ())
    | None -> () (* Notification with wrong version - ignore *)
  else
    (* Check if it's a notification *)
    match req.id with
    | None -> (
        (* Handle notification - no response *)
        match config.on_notification with
        | Some handler -> handler req.method_ req.params
        | None -> ())
    | Some id -> (
        (* Handle regular request *)
        match Hashtbl.find_opt config.methods req.method_ with
        | None ->
            (* Method not found *)
            reply
              (Common.make_response
                 ~error:
                   (Common.make_error ~code:Common.MethodNotFound
                      ~message:
                        (Printf.sprintf "Method not found: %s" req.method_)
                      ())
                 ~id ())
        | Some handler ->
            (* Execute handler with reply function that wraps responses with request id *)
            let wrapped_reply response =
              let updated_response = { response with Common.id } in
              reply updated_response
            in
            handler wrapped_reply req.params)

let handle_batch config reply requests =
  List.iter (handle_request config reply) requests

let handle_json config reply json =
  (* Try to parse as single request first *)
  match Common.request_of_json json with
  | Ok req -> handle_request config reply req
  | Error _ -> (
      (* Try as batch request *)
      match json with
      | Json.Array items ->
          List.iter
            (fun item ->
              match Common.request_of_json item with
              | Ok req -> handle_request config reply req
              | Error _ ->
                  (* Invalid request in batch *)
                  reply
                    (Common.make_response
                       ~error:
                         (Common.make_error ~code:Common.InvalidRequest
                            ~message:"Invalid request in batch" ())
                       ~id:Common.Null ()))
            items
      | _ ->
          (* Invalid JSON-RPC *)
          reply
            (Common.make_response
               ~error:
                 (Common.make_error ~code:Common.InvalidRequest
                    ~message:"Invalid JSON-RPC request" ())
               ~id:Common.Null ()))

let handle_message config reply str =
  match Json.of_string str with
  | Ok json -> handle_json config reply json
  | Error e ->
      (* JSON parse error *)
      reply
        (Common.make_response
           ~error:
             (Common.make_error ~code:Common.ParseError
                ~message:(Printf.sprintf "JSON parse error: %s" e)
                ())
           ~id:Common.Null ())
