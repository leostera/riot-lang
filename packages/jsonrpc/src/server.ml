(** JSON-RPC 2.0 Server Implementation *)

open Std.Data

(* TODO: In the future, we could use a GADT handler type to allow each
   handler to have its own request type and param parser. For now,
   we use a unified request type for simplicity. *)

type ('req, 'res) handler = {
  method_ : string;
  fn : ('res -> unit) -> 'req -> unit;
}

type ('request, 'response) t = {
  protocol_mod :
    (module Common.ApplicationProtocol
       with type request = 'request
        and type response = 'response);
  handlers : ('request, 'response) handler list;
}

let create ~protocol:protocol_mod ~methods:handlers = { protocol_mod; handlers }

let handle_message (type req res) (server : (req, res) t)
    (reply : string -> unit) (message : string) =
  let module P =
    (val server.protocol_mod
        : Common.ApplicationProtocol with type request = req
         and type response = res)
  in
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
      | Ok request -> (
          (* Find handler for method *)
          match
            List.find_opt (fun h -> h.method_ = request.method_) server.handlers
          with
          | None ->
              (* Method not found - can't send typed response, just log/ignore *)
              ()
          | Some handler -> (
              (* Convert params to typed request using method name *)
              Printf.eprintf "[JSONRPC SERVER] Found handler for %s\n"
                request.method_;
              match P.request_of_params request.method_ request.params with
              | Error _err ->
                  (* Invalid params - can't send typed response, just log/ignore *)
                  Printf.eprintf "[JSONRPC SERVER] Failed to convert params\n";
                  ()
              | Ok typed_request ->
                  (* Execute handler with typed request *)
                  Printf.eprintf "[JSONRPC SERVER] Calling handler\n";
                  if Common.is_notification request then
                    (* Notification - no response expected *)
                    handler.fn (fun _ -> ()) typed_request
                  else
                    (* Regular request - response expected *)
                    let typed_reply res =
                      (* Convert typed response to JSON *)
                      let response_json = P.response_to_json res in
                      (* Wrap in JSON-RPC response *)
                      let id = Option.value request.id ~default:Common.Null in
                      let json_response =
                        Json.obj
                          [
                            ("jsonrpc", Json.string Common.version);
                            ("result", response_json);
                            ("id", Common.id_to_json id);
                          ]
                      in
                      (* Convert to string and send *)
                      reply (Json.to_string json_response)
                    in
                    handler.fn typed_reply typed_request)))
