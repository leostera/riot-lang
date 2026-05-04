open Std
open Std.Data
open Std.Result.Syntax

let fixture_root = Path.v "packages/lsp/tests/protocol_fixtures"

let keep_json = fun path ->
  match Path.extension path with
  | Some ".json" -> Test.FixtureRunner.Keep
  | _ -> Test.FixtureRunner.Skip

let decode_fixture_json = fun path ->
  let* source =
    Fs.read path
    |> Result.map_err ~fn:IO.error_message
  in
  Json.from_string source
  |> Result.map_err ~fn:Json.error_to_string

let render_json = fun json -> Json.to_string_pretty json ^ "\n"

let roundtrip_request:
  type params res. (params, res) Lsp.Method.request ->
  Json.t ->
  (Json.t, string) result = fun method_ ->
  fun json ->
    let* (id, params) = Lsp.request_of_json method_ json in
    Ok (Lsp.request_to_json ~id method_ params)

let roundtrip_notification:
  type params. params Lsp.Method.notification ->
  Json.t ->
  (Json.t, string) result = fun method_ ->
  fun json ->
    let* params = Lsp.notification_of_json method_ json in
    Ok (Lsp.notification_to_json method_ params)

let roundtrip_response:
  type params res. (params, res) Lsp.Method.request ->
  Json.t ->
  (Json.t, string) result = fun method_ ->
  fun json ->
    let* (id, result) = Lsp.response_of_json method_ json in
    Ok (Lsp.response_to_json ~id method_ result)

let roundtrip_error_response = fun json ->
  let* (id, error) = Lsp.error_response_of_json json in
  Ok (Lsp.error_response_to_json ~id error)

let roundtrip_fixture = fun relpath ->
  fun json ->
    match Path.to_string relpath with
    | "requests/initialize.json" -> roundtrip_request Lsp.Initialize.request json
    | "requests/shutdown.json" -> roundtrip_request Lsp.Shutdown.request json
    | "requests/hover.json" -> roundtrip_request Lsp.Text_document_methods.Hover.request json
    | "requests/definition.json" ->
        roundtrip_request Lsp.Text_document_methods.Definition.request json
    | "requests/document_symbol.json" ->
        roundtrip_request Lsp.Text_document_methods.Document_symbol.request json
    | "requests/formatting.json" ->
        roundtrip_request Lsp.Text_document_methods.Formatting.request json
    | "requests/code_action.json" ->
        roundtrip_request Lsp.Text_document_methods.Code_action.request json
    | "notifications/initialized.json" -> roundtrip_notification Lsp.Initialized.notification json
    | "notifications/exit.json" -> roundtrip_notification Lsp.Exit.notification json
    | "notifications/did_open.json" ->
        roundtrip_notification Lsp.Text_document_methods.Did_open.notification json
    | "notifications/did_change.json" ->
        roundtrip_notification Lsp.Text_document_methods.Did_change.notification json
    | "notifications/did_close.json" ->
        roundtrip_notification Lsp.Text_document_methods.Did_close.notification json
    | "notifications/publish_diagnostics.json" ->
        roundtrip_notification Lsp.Text_document_methods.Publish_diagnostics.notification json
    | "responses/initialize.json" -> roundtrip_response Lsp.Initialize.request json
    | "responses/shutdown.json" -> roundtrip_response Lsp.Shutdown.request json
    | "responses/hover.json" -> roundtrip_response Lsp.Text_document_methods.Hover.request json
    | "responses/definition.json" ->
        roundtrip_response Lsp.Text_document_methods.Definition.request json
    | "responses/document_symbol.json" ->
        roundtrip_response Lsp.Text_document_methods.Document_symbol.request json
    | "responses/formatting.json" ->
        roundtrip_response Lsp.Text_document_methods.Formatting.request json
    | "responses/code_action.json" ->
        roundtrip_response Lsp.Text_document_methods.Code_action.request json
    | "responses/error.json" -> roundtrip_error_response json
    | other -> Error ("unsupported lsp protocol fixture: " ^ other)

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* json = decode_fixture_json ctx.fixture_path in
  let* actual = roundtrip_fixture ctx.fixture_relpath json in
  Test.Snapshot.assert_text ~ctx:ctx.test ~actual:(render_json actual)

let main ~args =
  let tests =
    Test.FixtureRunner.cases
      ()
      ~dir:fixture_root
      ~filter:keep_json
      ~run:(fun ctx -> test_fixture ~ctx)
  in
  Test.Cli.main ~name:"protocol-fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
