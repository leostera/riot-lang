module Raml_compilation = Raml_core.Compilation
module Raml_config = Raml_core.Config
module Raml_event = Raml_core.Event
module Raml_target = Raml_core.Target
module Frontend_pipeline = Raml_core.Frontend_pipeline
module Backend_result = Raml_core.Backend_result
open Std

let ( let* ) result fn = Result.and_then result ~fn

let compilation_of_frontend_and_backend = fun (frontend: Frontend_pipeline.t) backend_result ->
  let frontend_diagnostics = (frontend.typing.parse_diagnostics
  |> List.map ~fn:(fun diagnostic -> Raml_compilation.Parse diagnostic))
  @ (frontend.typing.lowering_diagnostics
  |> List.map ~fn:(fun diagnostic -> Raml_compilation.Lowering diagnostic))
  @ (frontend.typing.typing_diagnostics
  |> List.map ~fn:(fun diagnostic -> Raml_compilation.Typing diagnostic)) in
  Raml_compilation.create
    ~targeting:frontend.targeting
    ~source:frontend.source
    ~typing:frontend.typing.json
    ~core_ir:frontend.core_ir.json
    ~frontend_diagnostics
    ~lowering_fields:backend_result.Backend_result.lowered_fields
    ~codegen_fields:backend_result.Backend_result.codegen_fields

let compile_loaded_source = fun ~config ~relpath ~source ->
  let* frontend = Frontend_pipeline.compile_source ~config ~relpath ~source in
  let backend_result =
    match Raml_config.select_backend config with
    | Raml_target.Js -> Raml_js.Backend.compile ~config ~frontend
    | Raml_target.Native -> Raml_native.Backend.compile ~config ~frontend
    | Raml_target.Wasm -> Raml_wasm.Backend.compile ~config ~frontend
  in
  Ok (frontend, backend_result)

let compile_source = fun ?(config = Raml_config.default) ~relpath source ->
  Raml_config.emit_event config (fun () -> Raml_event.CompileStarted { path = relpath });
  match Raml_config.validate config with
  | Error reason ->
      Raml_config.emit_event
        config
        (fun () ->
          Raml_event.CompileFailed {
            path = relpath;
            failure = Raml_event.ConfigValidationFailed { reason }
          });
      Error reason
  | Ok () -> (
      match compile_loaded_source ~config ~relpath ~source with
      | Error reason ->
          Raml_config.emit_event
            config
            (fun () ->
              Raml_event.CompileFailed {
                path = relpath;
                failure = Raml_event.SourceUnitRejected { reason }
              });
          Error reason
      | Ok (frontend, backend_result) ->
          Frontend_pipeline.emit_events config ~path:relpath frontend;
          Backend_result.emit_events config ~path:relpath backend_result;
          Raml_config.emit_event config (fun () -> Raml_event.CompileFinished { path = relpath });
          Ok (compilation_of_frontend_and_backend frontend backend_result)
    )

let compile = fun ?(config = Raml_config.default) path ->
  Raml_config.emit_event config (fun () -> Raml_event.CompileStarted { path });
  match Raml_config.validate config with
  | Error reason ->
      Raml_config.emit_event
        config
        (fun () ->
          Raml_event.CompileFailed { path; failure = Raml_event.ConfigValidationFailed { reason } });
      Error reason
  | Ok () -> (
      match Fs.read path with
      | Error err ->
          let reason = IO.error_message err in
          Raml_config.emit_event
            config
            (fun () ->
              Raml_event.CompileFailed { path; failure = Raml_event.SourceReadFailed { reason } });
          Error reason
      | Ok source -> (
          match compile_loaded_source ~config ~relpath:path ~source with
          | Error reason ->
              Raml_config.emit_event
                config
                (fun () ->
                  Raml_event.CompileFailed {
                    path;
                    failure = Raml_event.SourceUnitRejected { reason }
                  });
              Error reason
          | Ok (frontend, backend_result) ->
              Frontend_pipeline.emit_events config ~path frontend;
              Backend_result.emit_events config ~path backend_result;
              Raml_config.emit_event config (fun () -> Raml_event.CompileFinished { path });
              Ok (compilation_of_frontend_and_backend frontend backend_result)
        )
    )
