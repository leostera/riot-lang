open Std
open Std.Data
open Std.Result.Syntax

let server_mode_arg = "__riot_lsp_server"

let lift_process = fun result ->
  match result with
  | Ok value -> Ok value
  | Error error -> Error (Process.error_to_string error)

let lift_file = fun result ->
  match result with
  | Ok value -> Ok value
  | Error error -> Error (Fs.File.error_to_string error)

let workspace_path (ctx: Test.ctx) =
  let root =
    match ctx.Test.workspace_root with
    | Some root -> root
    | None ->
        Env.current_dir ()
        |> Result.unwrap_or ~default:(Path.v ".")
  in
  Path.(root
  / Path.v "packages"
  / Path.v "riot-lsp"
  / Path.v "tests"
  / Path.v "workspace_fixtures"
  / Path.v "snow")

let app_path workspace = Path.(workspace / Path.v "app" / Path.v "src" / Path.v "app.ml")

let lsp_request ?params ~id ~method_ () =
  let fields = [
    ("jsonrpc", Json.string "2.0");
    ("id", Json.int id);
    ("method", Json.string method_);
  ]
  in
  let fields =
    match params with
    | None -> fields
    | Some params -> fields @ [ ("params", params); ]
  in
  Json.obj fields

let lsp_notification ?params ~method_ () =
  let fields = [ ("jsonrpc", Json.string "2.0"); ("method", Json.string method_); ] in
  let fields =
    match params with
    | None -> fields
    | Some params -> fields @ [ ("params", params); ]
  in
  Json.obj fields

let text_document_identifier uri = Json.obj [ ("uri", Lsp.Uri.to_json uri); ]

let initialize_request workspace =
  lsp_request
    ~id:1
    ~method_:"initialize"
    ~params:(Json.obj
      [
        ("capabilities", Json.obj []);
        ("processId", Json.int 123);
        ("rootUri", Lsp.Uri.to_json (Lsp.Uri.from_path workspace));
      ])
    ()

let did_open_notification uri source =
  lsp_notification
    ~method_:"textDocument/didOpen"
    ~params:(Json.obj
      [
        (
          "textDocument",
          Json.obj
            [
              ("uri", Lsp.Uri.to_json uri);
              ("languageId", Json.string "ocaml");
              ("version", Json.int 1);
              ("text", Json.string source);
            ]
        );
      ])
    ()

let hover_request uri =
  lsp_request
    ~id:2
    ~method_:"textDocument/hover"
    ~params:(Json.obj
      [
        ("textDocument", text_document_identifier uri);
        ("position", Json.obj [ ("line", Json.int 0); ("character", Json.int 6); ]);
      ])
    ()

let shutdown_request = lsp_request ~id:3 ~method_:"shutdown" ~params:(Json.obj []) ()

let exit_notification = lsp_notification ~method_:"exit" ()

let write_message writer json =
  let* () =
    Riot_lsp.Framing.write writer (Json.to_string json)
    |> Result.map_err ~fn:(fun message -> "failed to write LSP message: " ^ message)
  in
  IO.flush writer
  |> Result.map_err ~fn:(fun error -> "failed to flush LSP message: " ^ IO.error_message error)

let read_message reader =
  let rec read_with_retry attempts =
    match Riot_lsp.Framing.read reader with
    | Ok payload -> Ok payload
    | Error message when attempts > 0 && String.contains message "Operation would block" ->
        sleep (Time.Duration.from_millis 5);
        read_with_retry (attempts - 1)
    | Error message -> Error ("failed to read LSP message: " ^ message)
  in
  let* payload = read_with_retry 100 in
  match payload with
  | None -> Error "expected LSP response, got EOF"
  | Some payload ->
      Json.from_string payload
      |> Result.map_err
        ~fn:(fun error -> "invalid LSP JSON response: " ^ Json.error_to_string error)

let field name json =
  match Json.get_field name json with
  | Some value -> Ok value
  | None -> Error ("missing JSON field: " ^ name)

let field_int name json =
  let* value = field name json in
  match Json.get_int value with
  | Some value -> Ok value
  | None -> Error ("expected JSON int field: " ^ name)

let field_string name json =
  let* value = field name json in
  match Json.get_string value with
  | Some value -> Ok value
  | None -> Error ("expected JSON string field: " ^ name)

let field_array name json =
  let* value = field name json in
  match Json.get_array value with
  | Some value -> Ok value
  | None -> Error ("expected JSON array field: " ^ name)

let expect_response_id expected json =
  let* actual = field_int "id" json in
  if Int.equal actual expected then
    Ok ()
  else
    Error ("expected response id " ^ Int.to_string expected ^ " but got " ^ Int.to_string actual)

let expect_notification method_ json =
  let* actual = field_string "method" json in
  if String.equal actual method_ then
    Ok ()
  else
    Error ("expected notification " ^ method_ ^ " but got " ^ actual)

let expect_empty_diagnostics json =
  let* () = expect_notification "textDocument/publishDiagnostics" json in
  let* params = field "params" json in
  let* diagnostics = field_array "diagnostics" params in
  match diagnostics with
  | [] -> Ok ()
  | _ -> Error ("expected no diagnostics, got " ^ Json.to_string json)

let expect_hover_type expected json =
  let* () = expect_response_id 2 json in
  let* result = field "result" json in
  let* contents = field "contents" result in
  let* value = field_string "value" contents in
  if String.equal value expected then
    Ok ()
  else
    Error ("expected hover type " ^ expected ^ " but got " ^ value)

let wait_for_exit process =
  let rec loop attempts =
    match lift_process (Process.try_wait process) with
    | Ok (Some status) -> Ok status
    | Ok None when attempts > 0 ->
        sleep (Time.Duration.from_millis 10);
        loop (attempts - 1)
    | Ok None -> Error "riot-lsp server did not exit"
    | Error error -> Error error
  in
  loop 200

let close_process process =
  let _ = Process.close process in
  ()

let terminate_process process =
  match Process.try_wait process with
  | Ok (Some _) -> close_process process
  | _ ->
      let _ = Process.kill process ~signal:9 in
      close_process process

let with_server (ctx: Test.ctx) fn =
  let* binary =
    match ctx.binary_path with
    | Some path -> Ok path
    | None -> Error "test binary path is unavailable"
  in
  let workspace = workspace_path ctx in
  let stdio = {
    Process.stdin = Process.Stdin.Pipe;
    stdout = Process.Stdout.Pipe;
    stderr = Process.Stderr.Null;
  }
  in
  let* process =
    lift_process
      (Process.spawn
        ~program:(Path.to_string binary)
        ~args:[|server_mode_arg|]
        ~current_dir:workspace
        ~stdio
        ())
  in
  match (Process.stdin process, Process.stdout process) with
  | (Some stdin, Some stdout) ->
      let result =
        fn ~process ~stdin:(Fs.File.to_writer stdin) ~stdout:(Fs.File.to_reader stdout) ~workspace
      in
      terminate_process process;
      result
  | _ ->
      terminate_process process;
      Error "riot-lsp server pipes were not available"

let test_server_starts_on_workspace_and_serves_hover _ctx =
  with_server
    _ctx
    (fun ~process ~stdin ~stdout ~workspace ->
      let source = "let luffy = 56\n" in
      let uri = Lsp.Uri.from_path (app_path workspace) in
      let* () = write_message stdin (initialize_request workspace) in
      let* initialize_response = read_message stdout in
      let* () = expect_response_id 1 initialize_response in
      let* () = write_message stdin (did_open_notification uri source) in
      let* diagnostics = read_message stdout in
      let* () = expect_empty_diagnostics diagnostics in
      let* () = write_message stdin (hover_request uri) in
      let* hover = read_message stdout in
      let* () = expect_hover_type "int" hover in
      let* () = write_message stdin shutdown_request in
      let* shutdown = read_message stdout in
      let* () = expect_response_id 3 shutdown in
      let* () = write_message stdin exit_notification in
      let* status = wait_for_exit process in
      match status with
      | Process.Running -> Error "riot-lsp was still running after wait"
      | Process.Exited 0 -> Ok ()
      | Process.Exited code -> Error ("riot-lsp exited with status " ^ Int.to_string code)
      | Process.Signaled signal -> Error ("riot-lsp was signaled " ^ Int.to_string signal)
      | Process.Stopped signal -> Error ("riot-lsp was stopped " ^ Int.to_string signal))

let tests =
  Test.[
    case
      "server starts on workspace and serves hover type"
      test_server_starts_on_workspace_and_serves_hover;
  ]

let main ~args =
  if List.any args ~fn:(String.equal server_mode_arg) then
    Riot_lsp.run ~log_path:(Path.v "/tmp/riot-lsp-server-loop-tests.log") ()
  else
    Test.Cli.main ~name:"riot-lsp server loop tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
