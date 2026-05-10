open Std
open Std.Data
open Std.Result.Syntax

module Framing = Framing
module Session = Session

module File_log = struct
  type t = {
    path: Path.t;
    sink: Fs.File.t;
  }

  let default_path = fun () ->
    match Env.var Env.String ~name:"RIOT_LSP_LOG_PATH" with
    | Some path when not (String.equal path "") -> Path.v path
    | _ ->
        match Env.home_dir () with
        | Some home -> Path.(home / Path.v ".riot" / Path.v "logs" / Path.v "riot-lsp.log")
        | None -> Path.v "/tmp/riot-lsp.log"

  let open_sink = fun ?path () ->
    let path =
      match path with
      | Some path -> path
      | None -> default_path ()
    in
    let* () =
      match Path.parent path with
      | None -> Ok ()
      | Some parent ->
          Fs.create_dir_all parent
          |> Result.map_err ~fn:IO.error_message
    in
    let* sink =
      Fs.File.open_append path
      |> Result.map_err ~fn:Fs.File.error_to_string
    in
    Ok { path; sink }

  let write = fun t ~level message ->
    let line = DateTime.to_iso8601 (DateTime.now_utc ()) ^ " | " ^ level ^ " | " ^ message ^ "\n" in
    ignore ((Fs.File.write_all t.sink line): (unit, _) result)

  let close = fun t -> ignore ((Fs.File.close t.sink): (unit, _) result)
end

let log = fun logger ~level message ->
  match logger with
  | None -> ()
  | Some logger -> File_log.write logger ~level message

let payload_summary = fun payload ->
  match Json.from_string payload with
  | Error error -> "invalid-json payload: " ^ Json.error_to_string error
  | Ok json ->
      match Jsonrpc.request_of_json json with
      | Error reason -> "invalid-request payload: " ^ reason
      | Ok request ->
          let kind =
            match request.Jsonrpc.id with
            | Some _ -> "request"
            | None -> "notification"
          in
          let id_suffix =
            match request.Jsonrpc.id with
            | Some id -> " id=" ^ Json.to_string (Jsonrpc.id_to_json id)
            | None -> ""
          in
          kind ^ " " ^ request.Jsonrpc.method_ ^ id_suffix

let write_outbound = fun output messages ->
  List.fold_left
    messages
    ~init:(Ok ())
    ~fn:(fun acc json ->
      let* () = acc in
      Framing.write output (Std.Data.Json.to_string json)
      |> Result.map_err ~fn:(fun message -> Failure message))

let rec loop = fun logger ->
  fun input ->
    fun output ->
      fun state ->
        let* payload_opt =
          Framing.read input
          |> Result.map_err ~fn:(fun message -> Failure message)
        in
        match payload_opt with
        | None -> Ok ()
        | Some payload ->
            log logger ~level:"DEBUG" ("received " ^ payload_summary payload);
            let outcome = Session.handle_payload state payload in
            List.for_each outcome.debug_events ~fn:(fun event -> log logger ~level:"DEBUG" event);
            if not (List.is_empty outcome.outbound) then
              log
                logger
                ~level:"DEBUG"
                ("sending " ^ Int.to_string (List.length outcome.outbound) ^ " outbound message(s)");
            (
              match outcome.exit_code with
              | None -> ()
              | Some code ->
                  log logger ~level:"INFO" ("lsp session exiting with code " ^ Int.to_string code)
            );
            let* () = write_outbound output outcome.outbound in
            match outcome.exit_code with
            | Some 0 -> Ok ()
            | Some code -> Error (Failure ("riot-lsp exited with status " ^ Int.to_string code))
            | None -> loop logger input output outcome.state

let run = fun ?log_path () ->
  let input = IO.stdin () in
  let output = IO.stdout () in
  match File_log.open_sink ?path:log_path () with
  | Error _ -> loop None input output Session.empty
  | Ok logger ->
      let close () = File_log.close logger in
      log (Some logger) ~level:"INFO" ("riot-lsp started; logging to " ^ Path.to_string logger.path);
      let result =
        match loop (Some logger) input output Session.empty with
        | Ok () as ok ->
            log (Some logger) ~level:"INFO" "riot-lsp stopped cleanly";
            ok
        | Error error as err ->
            log (Some logger) ~level:"ERROR" ("riot-lsp failed: " ^ Exception.to_string error);
            err
      in
      close ();
      result
