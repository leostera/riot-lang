(** JSON-RPC 2.0 Server Implementation *)
open Std
open Std.Data
open Std.Collections

(* TODO: In the future, we could use a GADT handler type to allow each
   handler to have its own request type and param parser. For now,
   we use a unified request type for simplicity.
*)

type ('req, 'res) handler = {
  method_: string;
  fn: ('res -> unit) -> 'req -> unit;
}

type ('request, 'response) t = {
  protocol_mod:
    (module Common.ApplicationProtocol with type request = 'request and type response = 'response);
  handlers: ('request, 'response) handler list;
}

let create = fun ~protocol:protocol_mod ~methods:handlers -> { protocol_mod; handlers }

let handle_message = fun
  (type req res) (server: (req, res) t) (reply: string -> unit) (message: string) ->
  let module P = (val server.protocol_mod : Common.ApplicationProtocol with type request = req and type response = res) in
  let cwd =
    match Env.current_dir () with
    | Ok path -> Path.to_string path
    | Error _ -> "<unknown>"
  in
  Log.trace ("[JSONRPC SERVER] Processing request in cwd: " ^ cwd);
  (* Parse the incoming message *)
  Log.trace ("[JSONRPC SERVER] Parsing message: " ^ message);
  match Json.from_string message with
  | Error e ->
      (* Parse error - can't send typed response, just log/ignore *)
      Log.trace ("[JSONRPC SERVER] JSON parse error: " ^ Json.error_to_string e);
      ()
  | Ok json ->
      Log.trace "[JSONRPC SERVER] JSON parsed successfully";
      match Common.request_of_json json with
      | Error e ->
          (* Invalid request - can't send typed response, just log/ignore *)
          Log.trace "[JSONRPC SERVER] Request parse error";
          ()
      | Ok request ->
          Log.trace ("[JSONRPC SERVER] Looking for handler for method: " ^ request.method_);
          Log.trace
            ("[JSONRPC SERVER] Available handlers: " ^ Int.to_string (List.length server.handlers));
          List.for_each
            server.handlers
            ~fn:(fun h -> Log.trace ("[JSONRPC SERVER]   - " ^ h.method_));
          (* Find handler for method *)
          match List.find server.handlers ~fn:(fun h -> h.method_ = request.method_) with
          | None ->
              (* Method not found - can't send typed response, just log/ignore *)
              Log.trace ("[JSONRPC SERVER] No handler found for method: " ^ request.method_);
              ()
          | Some handler ->
              (* Convert params to typed request using method name *)
              Log.trace ("[JSONRPC SERVER] Found handler for " ^ request.method_);
              match P.request_of_params request.method_ request.params with
              | Error _err ->
                  (* Invalid params - can't send typed response, just log/ignore *)
                  Log.trace "[JSONRPC SERVER] Failed to convert params";
                  ()
              | Ok typed_request ->
                  (* Execute handler with typed request *)
                  Log.trace "[JSONRPC SERVER] Calling handler";
                  if Common.is_notification request then (
                    (* Notification - no response expected *)
                    handler.fn (fun _ -> ()) typed_request;
                    let cwd_after =
                      match Env.current_dir () with
                      | Ok path -> Path.to_string path
                      | Error _ -> "<unknown>"
                    in
                    if cwd != cwd_after then
                      Log.trace
                        ("[JSONRPC SERVER] CWD changed during handler! " ^ cwd ^ " -> " ^ cwd_after)
                  ) else
                    (* Regular request - response expected *)
                    let typed_reply res =
                      let response_json = P.response_to_json res in
                      (* Wrap in JSON-RPC response *)
                      let id = Option.unwrap_or request.id ~default:Common.Null in
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
                        match Env.current_dir () with
                        | Ok path -> Path.to_string path
                        | Error _ -> "<unknown>"
                      in
                      if cwd != cwd_after then
                        Log.trace
                          ("[JSONRPC SERVER] CWD changed during handler! "
                          ^ cwd
                          ^ " -> "
                          ^ cwd_after)
                    in
                    handler.fn typed_reply typed_request
