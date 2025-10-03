(** JSON-RPC 2.0 Server Implementation *)

open Std
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
  (* Log current directory at start of request *)
  let cwd = Env.current_dir () |> Result.unwrap |> Path.to_string in
  Log.info "[JSONRPC SERVER] Processing request in cwd: %s" cwd;
  (* Parse the incoming message *)
  Log.trace "[JSONRPC SERVER] Parsing message: %s" message;
  match Json.of_string message with
  | Error e ->
      (* Parse error - can't send typed response, just log/ignore *)
      Log.error "[JSONRPC SERVER] JSON parse error: %s" (Json.error_to_string e);
      ()
  | Ok json -> (
      Log.trace "[JSONRPC SERVER] JSON parsed successfully";
      match Common.request_of_json json with
      | Error e ->
          (* Invalid request - can't send typed response, just log/ignore *)
          Log.error "[JSONRPC SERVER] Request parse error";
          ()
      | Ok request -> (
          Log.trace "[JSONRPC SERVER] Looking for handler for method: %s"
            request.method_;
          Log.trace "[JSONRPC SERVER] Available handlers: %d"
            (List.length server.handlers);
          List.iter
            (fun h -> Log.trace "[JSONRPC SERVER]   - %s" h.method_)
            server.handlers;
          (* Find handler for method *)
          match
            List.find_opt (fun h -> h.method_ = request.method_) server.handlers
          with
          | None ->
              (* Method not found - can't send typed response, just log/ignore *)
              Log.error "[JSONRPC SERVER] No handler found for method: %s"
                request.method_;
              ()
          | Some handler -> (
              (* Convert params to typed request using method name *)
              Log.trace "[JSONRPC SERVER] Found handler for %s" request.method_;
              match P.request_of_params request.method_ request.params with
              | Error _err ->
                  (* Invalid params - can't send typed response, just log/ignore *)
                  Log.error "[JSONRPC SERVER] Failed to convert params";
                  ()
              | Ok typed_request ->
                  (* Execute handler with typed request *)
                  Log.trace "[JSONRPC SERVER] Calling handler";
                  if Common.is_notification request then (
                    (* Notification - no response expected *)
                    handler.fn (fun _ -> ()) typed_request;
                    let cwd_after =
                      Env.current_dir () |> Result.unwrap |> Path.to_string
                    in
                    if cwd <> cwd_after then
                      Log.warn
                        "[JSONRPC SERVER] CWD changed during handler! %s -> %s"
                        cwd cwd_after)
                  else
                    (* Regular request - response expected *)
                    let typed_reply res =
                      (* Convert typed response to JSON *)
                      let response_json = P.response_to_json res in
                      (* Wrap in JSON-RPC response *)
                      let id =
                        Option.unwrap_or request.id ~default:Common.Null
                      in
                      let json_response =
                        Json.obj
                          [
                            ("jsonrpc", Json.string Common.version);
                            ("result", response_json);
                            ("id", Common.id_to_json id);
                          ]
                      in
                      (* Convert to string and send *)
                      reply (Json.to_string json_response);
                      let cwd_after =
                        Env.current_dir () |> Result.unwrap |> Path.to_string
                      in
                      if cwd <> cwd_after then
                        Log.warn
                          "[JSONRPC SERVER] CWD changed during handler! %s -> \
                           %s"
                          cwd cwd_after
                    in
                    handler.fn typed_reply typed_request)))
