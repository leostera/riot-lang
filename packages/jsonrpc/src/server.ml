(** JSON-RPC 2.0 Server Implementation *)

type 'res handler = 
  | Handler : {
      method_: string;
      parse_params: Common.params -> ('req, Json.t) result;
      handle_request: req:'req -> reply:('res -> unit) -> unit;
    } -> 'res handler

type ('request, 'response) t = {
  protocol_mod : (module Common.ApplicationProtocol with type request = 'request and type response = 'response);
  handlers : 'response handler list;
}

let create ~protocol:protocol_mod ~methods:handlers =
  { protocol_mod; handlers }

let handle_message (type req res) (server : (req, res) t) (reply : string -> unit) (message : string) =
  let module P = (val server.protocol_mod : Common.ApplicationProtocol with type request = req and type response = res) in
  
  (* Parse the incoming message *)
  match Json.of_string message with
  | Error e ->
      (* Parse error - can't send typed response, just log/ignore *)
      ()
  | Ok json -> (
      match Common.request_of_json json with
      | Error e ->
          (* Invalid request - can't send typed response, just log/ignore *)
          ()
      | Ok request ->
          (* Find handler for method *)
          let rec find_and_handle = function
            | [] ->
                (* Method not found - can't send typed response, just log/ignore *)
                Printf.eprintf "[JSONRPC SERVER] No handler found for %s\n" request.method_;
                flush stderr;
                ()
            | (Handler h) :: rest ->
                if h.method_ = request.method_ then (
                  Printf.eprintf "[JSONRPC SERVER] Found handler for %s\n" request.method_;
                  flush stderr;
                  (* Parse params with this handler's parser *)
                  match h.parse_params request.params with
                  | Error _err ->
                      Printf.eprintf "[JSONRPC SERVER] Failed to parse params\n";
                      flush stderr;
                      ()
                  | Ok typed_request ->
                      Printf.eprintf "[JSONRPC SERVER] Calling handler\n";
                      flush stderr;
                      if Common.is_notification request then
                        (* Notification - no response expected *)
                        h.handle_request ~req:typed_request ~reply:(fun _ -> ())
                      else
                        (* Regular request - response expected *)
                        let typed_reply res =
                          (* Convert typed response to JSON *)
                          let response_json = P.response_to_json res in
                          (* Wrap in JSON-RPC response *)
                          let id = Option.value request.id ~default:Common.Null in
                          let json_response = 
                            Json.obj [
                              ("jsonrpc", Json.string Common.version);
                              ("result", response_json);
                              ("id", Common.id_to_json id)
                            ] in
                          (* Convert to string and send *)
                          reply (Json.to_string json_response)
                        in
                        h.handle_request ~req:typed_request ~reply:typed_reply
                ) else find_and_handle rest
          in
          find_and_handle server.handlers
  )