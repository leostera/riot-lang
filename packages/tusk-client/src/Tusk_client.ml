open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol

type t = {
  client : (WireProtocol.request, WireProtocol.response) Jsonrpc.Client.t;
  transport : Std.Net.TcpClient.t;
}

(** Build request type *)
type build_request = BuildPackage of string | BuildAll

(** Streaming build event *)
type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Event.t
  | BuildFinished of (unit, string) result

(** Create a new Tusk RPC client *)
let create ~host ~port =
  (* Create TCP transport using Std.Net.TcpClient *)
  match Std.Net.TcpClient.connect ~host ~port with
  | Ok transport ->
      let client =
        Jsonrpc.Client.create
          ~transport:(module Std.Net.TcpClient)
          ~protocol:(module WireProtocol)
          transport
      in
      Ok { client; transport }
  | Error e ->
      let error_msg =
        match e with
        | `Connection_refused -> "Connection refused"
        | `Closed -> "Connection closed"
        | `System_error msg -> format "System error: %s" msg
      in
      Error (format "Failed to connect to server: %s" error_msg)

(** Format all OCaml files in the workspace *)
let format_all t ~mode =
  match
    Jsonrpc.Client.call t.client ~method_:method_format_all
      ~params:
        (Jsonrpc.Named
           [
             ( "mode",
               Json.String
                 (match mode with `check -> "check" | `write -> "write") );
           ])
      ()
  with
  | Ok (WireProtocol.FormatAllResult { files_formatted; files_failed; errors })
    ->
      Ok (files_formatted, files_failed, errors)
  | Ok (WireProtocol.FormatError { error }) -> Error error
  | Ok _ -> Error "Invalid format all response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Create a new package *)
let new_package t ~path ~name ~is_library =
  match
    Jsonrpc.Client.call t.client ~method_:method_new_package
      ~params:
        (Jsonrpc.Named
           [
             ("path", Json.String path);
             ("name", Json.String name);
             ("is_library", Json.Bool is_library);
           ])
      ()
  with
  | Ok (WireProtocol.PackageCreated { path; name }) -> Ok (path, name)
  | Ok (WireProtocol.PackageCreationError { error }) -> Error error
  | Ok _ -> Error "Invalid package creation response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Create a new package in ./packages/ with dependencies *)
let create_package t ~name ~deps ~is_library =
  (* Create package in ./packages/<name> *)
  let path = format "packages/%s" name in
  match new_package t ~path ~name ~is_library with
  | Ok (created_path, created_name) ->
      (* TODO: Add dependencies to tusk.toml *)
      let files =
        [ format "%s/tusk.toml" created_path; format "%s/src" created_path ]
      in
      Ok (created_path, files)
  | Error e -> Error e

(** Create a new module file in a package *)
let create_module t ~package ~module_name ~contents =
  (* For now, return an error since we need filesystem access from the server *)
  Error
    (format
       "Module creation not yet implemented. Please create %s.ml in package \
        '%s' manually"
       module_name package)

(** Close the client *)
let close t =
  (* Jsonrpc.Client.close already closes the transport *)
  Jsonrpc.Client.close t.client

(** Ping the server *)
let ping t =
  match
    Jsonrpc.Client.call t.client ~method_:method_ping ~params:Jsonrpc.NoParams
      ()
  with
  | Ok _ -> Ok ()
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Get workspace configuration *)
let get_workspace_config t =
  match
    Jsonrpc.Client.call t.client ~method_:method_get_workspace_config
      ~params:Jsonrpc.NoParams ()
  with
  | Ok (WireProtocol.WorkspaceConfig config) -> Ok config
  | Ok _ -> Error "Invalid workspace config response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Get package information *)
let get_package_info t package_name =
  match
    Jsonrpc.Client.call t.client ~method_:method_get_package_info
      ~params:(Jsonrpc.Named [ ("package", Json.String package_name) ])
      ()
  with
  | Ok (WireProtocol.PackageInfo detail) -> Ok detail
  | Ok _ -> Error "Invalid package info response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Get build graph *)
let get_build_graph t =
  match
    Jsonrpc.Client.call t.client ~method_:method_get_build_graph
      ~params:Jsonrpc.NoParams ()
  with
  | Ok (WireProtocol.BuildGraph graph) -> Ok graph
  | Ok _ -> Error "Invalid build graph response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Build with streaming support *)
let build_streaming t request callback =
  let typed_request =
    match request with
    | BuildPackage pkg -> WireProtocol.BuildPackage pkg
    | BuildAll -> WireProtocol.BuildAll
  in

  (* Send the typed build request - this starts a streaming response *)
  match Jsonrpc.Client.send_request t.client typed_request with
  | Error e -> Error (format "Failed to send request: %s" e)
  | Ok () -> (
      (* Receive the first response *)
      match Jsonrpc.Client.receive_response t.client with
      | Error e -> Error (format "Failed to receive response: %s" e)
      | Ok response -> (
          match response.Jsonrpc.result with
          | Ok (WireProtocol.BuildStarted { session_id; started_at = _ }) ->
              (* Got BuildStarted *)
              callback (BuildStarted session_id);

              (* Now receive streaming events until build completes *)
              let rec receive_events () =
                match Jsonrpc.Client.receive_response t.client with
                | Ok
                    {
                      result =
                        Ok (WireProtocol.BuildEvent { session_id = _; event });
                      _;
                    } ->
                    callback (BuildEvent event);
                    receive_events ()
                | Ok
                    {
                      result =
                        Ok
                          (WireProtocol.CycleDetected
                             { session_id; cycle_nodes });
                      _;
                    } ->
                    (* Report cycle detected as a log event *)
                    callback
                      (BuildEvent
                         (Event.create ~session_id ~level:Error
                            (Event.CycleDetected { packages = cycle_nodes })));
                    receive_events ()
                | Ok
                    {
                      result =
                        Ok
                          (WireProtocol.PackageNotFound
                             { session_id; package_name; available_packages });
                      _;
                    } ->
                    (* Report package not found as an error and finish build *)
                    let error_msg =
                      format "Package '%s' not found. Available: %s"
                        package_name
                        (String.concat ", " available_packages)
                    in
                    callback (BuildFinished (Error error_msg));
                    Ok (BuildFinished (Error error_msg))
                | Ok
                    { result = Ok (WireProtocol.BuildComplete { stats; _ }); _ }
                  ->
                    if stats.packages_failed > 0 then
                      Ok
                        (BuildFinished
                           (Error
                              (format "%d packages failed to build"
                                 stats.packages_failed)))
                    else Ok (BuildFinished (Ok ()))
                | Ok
                    {
                      result =
                        Ok (WireProtocol.BuildFailed { session_id; error; _ });
                      _;
                    } ->
                    Ok (BuildFinished (Error error))
                | Ok { result = Ok (WireProtocol.Error msg); _ } ->
                    (* Got a general error response *)
                    Error (format "Server error: %s" msg)
                | Ok { result = Error err; _ } ->
                    Ok (BuildFinished (Error err.message))
                | Error e -> Error (format "Failed to receive event: %s" e)
                | Ok resp ->
                    (* Debug: print what response type we got *)
                    let resp_type =
                      match resp.result with
                      | Ok WireProtocol.Pong -> "Pong"
                      | Ok (WireProtocol.BuildGraph _) -> "BuildGraph"
                      | Ok (WireProtocol.WorkspaceConfig _) -> "WorkspaceConfig"
                      | Ok (WireProtocol.PackageInfo _) -> "PackageInfo"
                      | Ok (WireProtocol.BuildStarted _) -> "BuildStarted"
                      | Ok (WireProtocol.BuildEvent _) -> "BuildEvent"
                      | Ok (WireProtocol.CycleDetected _) -> "CycleDetected"
                      | Ok (WireProtocol.BuildComplete _) -> "BuildComplete"
                      | Ok (WireProtocol.BuildFailed _) -> "BuildFailed"
                      | Ok WireProtocol.ShutdownAck -> "ShutdownAck"
                      | Ok WireProtocol.RestartAck -> "RestartAck"
                      | Ok (WireProtocol.FormatResult _) -> "FormatResult"
                      | Ok (WireProtocol.FormatError _) -> "FormatError"
                      | Ok (WireProtocol.FormatAllResult _) -> "FormatAllResult"
                      | Ok (WireProtocol.PackageCreated _) -> "PackageCreated"
                      | Ok (WireProtocol.PackageCreationError _) ->
                          "PackageCreationError"
                      | Ok (WireProtocol.PackageNotFound _) -> "PackageNotFound"
                      | Ok WireProtocol.ExecutableNotFound ->
                          "ExecutableNotFound"
                      | Ok (WireProtocol.ExecutableFound _) -> "ExecutableFound"
                      | Ok (WireProtocol.ArtifactFound _) -> "ArtifactFound"
                      | Ok (WireProtocol.ArtifactNotFound _) ->
                          "ArtifactNotFound"
                      | Ok (WireProtocol.Error _) -> "Error"
                      | Error e -> format "JsonRpcError(%s)" e.message
                    in
                    Log.debug
                      "[CLIENT] Unexpected response in receive_events: %s"
                      resp_type;
                    Error "Unexpected response type"
              in
              receive_events ()
          | Ok
              (WireProtocol.BuildComplete
                 { session_id; completed_at = _; stats }) ->
              (* Check if build actually succeeded *)
              if stats.packages_failed > 0 then
                Ok
                  (BuildFinished
                     (Error
                        (format "%d packages failed to build"
                           stats.packages_failed)))
              else Ok (BuildFinished (Ok ()))
          | Ok (WireProtocol.BuildFailed { session_id; error; _ }) ->
              (* Direct error *)
              Ok (BuildFinished (Error error))
          | Ok (WireProtocol.Error msg) ->
              (* Other error *)
              Ok (BuildFinished (Error msg))
          | Error err ->
              Error (format "Build request failed: %s" err.Jsonrpc.message)
          | Ok resp ->
              (* Log unexpected response for debugging *)
              Error "Unexpected response type"))

(** Shutdown the server *)
let shutdown t =
  match
    Jsonrpc.Client.call t.client ~method_:method_shutdown
      ~params:Jsonrpc.NoParams ()
  with
  | Ok _ -> Ok ()
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Build a specific package *)
let build_package t package =
  match
    Jsonrpc.Client.call t.client ~method_:method_build_package
      ~params:(build_package_params package)
      ()
  with
  | Ok response -> Ok response
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Build all packages *)
let build_all t =
  match
    Jsonrpc.Client.call t.client ~method_:method_build_all
      ~params:Jsonrpc.NoParams ()
  with
  | Ok response -> Ok response
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Find an executable by binary name *)
let find_executable t name =
  match
    Jsonrpc.Client.call t.client ~method_:method_find_executable
      ~params:(Jsonrpc.Named [ ("name", Json.String name) ])
      ()
  with
  | Ok (WireProtocol.ExecutableFound { package; binary }) ->
      Ok (Some (package, binary))
  | Ok WireProtocol.ExecutableNotFound -> Ok None
  | Ok _ -> Error "Invalid findExecutable response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Find an artifact path *)
let find_artifact t ~package ~kind ~name =
  match
    Jsonrpc.Client.call t.client ~method_:method_find_artifact
      ~params:
        (Jsonrpc.Named
           [
             ("package", Json.String package);
             ("kind", Json.String kind);
             ("name", Json.String name);
           ])
      ()
  with
  | Ok (WireProtocol.ArtifactFound { path }) -> Ok path
  | Ok (WireProtocol.ArtifactNotFound { error }) -> Error error
  | Ok _ -> Error "Invalid findArtifact response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Restart the server *)
let restart t =
  match
    Jsonrpc.Client.call t.client ~method_:method_restart
      ~params:Jsonrpc.NoParams ()
  with
  | Ok _ -> Ok ()
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Format a file with ocamlformat *)
let format_file t ~file_path ~check_only =
  match
    Jsonrpc.Client.call t.client ~method_:method_format_file
      ~params:
        (Jsonrpc.Named
           [
             ("file_path", Json.String file_path);
             ("check_only", Json.Bool check_only);
           ])
      ()
  with
  | Ok (WireProtocol.FormatResult { formatted_code; changed }) ->
      Ok (formatted_code, changed)
  | Ok (WireProtocol.FormatError { error }) -> Error error
  | Ok _ -> Error "Invalid format response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Format code string with ocamlformat *)
let format_code t ~code ~file_path =
  let params =
    match file_path with
    | Some path ->
        Jsonrpc.Named
          [ ("code", Json.String code); ("file_path", Json.String path) ]
    | None -> Jsonrpc.Named [ ("code", Json.String code) ]
  in
  match Jsonrpc.Client.call t.client ~method_:method_format_code ~params () with
  | Ok (WireProtocol.FormatResult { formatted_code; changed }) ->
      Ok (formatted_code, changed)
  | Ok (WireProtocol.FormatError { error }) -> Error error
  | Ok _ -> Error "Invalid format response"
  | Error e ->
      Error (format "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)
