module Raml_config = Config
module Raml_compilation = Compilation
module Raml_event = Event
module Raml_example_pipeline = Example_pipeline
module Raml_target = Target
open Std
open Std.Data

let json_field = Json.get_field

let json_field_string = fun name json ->
  match json_field name json with
  | Some value -> Json.get_string value
  | None -> None

let json_field_int = fun name json ->
  match json_field name json with
  | Some value -> Json.get_int value
  | None -> None

let json_field_array_length = fun name json ->
  match json_field name json with
  | Some value -> (
      match Json.get_array value with
      | Some items -> List.length items
      | None -> 0
    )
  | None -> 0

let event_status_of_stage_json = fun json ->
  match json_field_string "status" json with
  | Some "ok" -> Raml_event.Ok
  | Some "error" -> Raml_event.Error
  | Some "blocked" -> Raml_event.Blocked
  | Some "unavailable" -> Raml_event.Unavailable
  | _ -> Raml_event.Error

let emit_pipeline_events = fun config ~path compilation ->
  let pipeline = Raml_example_pipeline.to_json compilation in
  let source = json_field "source" pipeline |> Option.unwrap_or ~default:Json.null in
  let typing = json_field "typing" pipeline |> Option.unwrap_or ~default:Json.null in
  let lowered = json_field "lowered" pipeline |> Option.unwrap_or ~default:Json.null in
  let codegen = json_field "codegen" pipeline |> Option.unwrap_or ~default:Json.null in
  let selected_backend = Raml_target.select_backend
    ~host:Raml_config.(config.host)
    ~target:Raml_config.(config.target) in
  let unit_name = json_field_string "unit_name" source |> Option.unwrap_or ~default:"Unknown" in
  let source_bytes = json_field_int "source_bytes" source |> Option.unwrap_or ~default:0 in
  let completeness = json_field_string "completeness" typing |> Option.unwrap_or ~default:"partial" in
  let core_ir = json_field "core_ir" lowered |> Option.unwrap_or ~default:Json.null in
  let jir = json_field "jir" lowered |> Option.unwrap_or ~default:Json.null in
  let nir = json_field "nir" lowered |> Option.unwrap_or ~default:Json.null in
  let mir = json_field "mir" lowered |> Option.unwrap_or ~default:Json.null in
  let lir = json_field "lir" lowered |> Option.unwrap_or ~default:Json.null in
  let wasm_lowering = json_field "wasm" lowered |> Option.unwrap_or ~default:Json.null in
  let js = json_field "js" codegen |> Option.unwrap_or ~default:Json.null in
  let native = json_field "native" codegen |> Option.unwrap_or ~default:Json.null in
  let wasm = json_field "wasm" codegen |> Option.unwrap_or ~default:Json.null in
  Raml_config.emit_event config (fun () -> Raml_event.SourceLoaded { path; unit_name; source_bytes });
  Raml_config.emit_event config
    (fun () ->
      Raml_event.TypingFinished {
        path;
        unit_name;
        completeness;
        parse_diagnostic_count = json_field_array_length "parse_diagnostics" typing;
        lowering_diagnostic_count = json_field_array_length "lowering_diagnostics" typing;
        typing_diagnostic_count = json_field_array_length "typing_diagnostics" typing;
      });
  Raml_config.emit_event
    config
    (fun () ->
      Raml_event.LoweringFinished {
        path;
        backend = Raml_event.CoreIr;
        status = event_status_of_stage_json core_ir;
        error_count = json_field_array_length "errors" core_ir
      });
  (
    match selected_backend with
    | Raml_target.Js ->
        Raml_config.emit_event
          config
          (fun () ->
            Raml_event.LoweringFinished {
              path;
              backend = Raml_event.Jir;
              status = event_status_of_stage_json jir;
              error_count = json_field_array_length "errors" jir
            });
        Raml_config.emit_event
          config
          (fun () ->
            Raml_event.CodegenFinished {
              path;
              target = config.target;
              status = event_status_of_stage_json js
            })
    | Raml_target.Native ->
        Raml_config.emit_event
          config
          (fun () ->
            Raml_event.LoweringFinished {
              path;
              backend = Raml_event.Nir;
              status = event_status_of_stage_json nir;
              error_count = json_field_array_length "errors" nir
            });
        Raml_config.emit_event
          config
          (fun () ->
            Raml_event.LoweringFinished {
              path;
              backend = Raml_event.Mir;
              status = event_status_of_stage_json mir;
              error_count = json_field_array_length "errors" mir
            });
        Raml_config.emit_event
          config
          (fun () ->
            Raml_event.LoweringFinished {
              path;
              backend = Raml_event.Lir;
              status = event_status_of_stage_json lir;
              error_count = json_field_array_length "errors" lir
            });
        Raml_config.emit_event
          config
          (fun () ->
            Raml_event.CodegenFinished {
              path;
              target = config.target;
              status = event_status_of_stage_json native
            })
    | Raml_target.Wasm ->
        Raml_config.emit_event
          config
          (fun () ->
            Raml_event.LoweringFinished {
              path;
              backend = Raml_event.Wasm;
              status = event_status_of_stage_json wasm_lowering;
              error_count = json_field_array_length "errors" wasm_lowering
            });
        Raml_config.emit_event
          config
          (fun () ->
            Raml_event.CodegenFinished {
              path;
              target = config.target;
              status = event_status_of_stage_json wasm
            })
  )

let compile_source = fun ?(config = Raml_config.default) ~relpath source ->
  Raml_config.emit_event config (fun () -> Raml_event.CompileStarted { path = relpath });
  match Raml_example_pipeline.compile_source ~host:config.host ~target:config.target ~relpath ~source with
  | Ok compilation ->
      emit_pipeline_events config ~path:relpath compilation;
      Raml_config.emit_event config (fun () -> Raml_event.CompileFinished { path = relpath });
      Ok (Raml_compilation.of_pipeline_json (Raml_example_pipeline.to_json compilation))
  | Error message ->
      Raml_config.emit_event config (fun () -> Raml_event.CompileFailed { path = relpath; message });
      Error message

let compile = fun ?(config = Raml_config.default) path ->
  Raml_config.emit_event config (fun () -> Raml_event.CompileStarted { path });
  match Fs.read path with
  | Error err ->
      let message = IO.error_message err in
      Raml_config.emit_event config (fun () -> Raml_event.CompileFailed { path; message });
      Error message
  | Ok source -> (
      match Raml_example_pipeline.compile_source
        ~host:config.host
        ~target:config.target
        ~relpath:path
        ~source with
      | Ok compilation ->
          emit_pipeline_events config ~path compilation;
          Raml_config.emit_event config (fun () -> Raml_event.CompileFinished { path });
          Ok (Raml_compilation.of_pipeline_json (Raml_example_pipeline.to_json compilation))
      | Error message ->
          Raml_config.emit_event config (fun () -> Raml_event.CompileFailed { path; message });
          Error message
    )
