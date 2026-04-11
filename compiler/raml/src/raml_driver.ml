module Raml_compilation = Compilation
module Raml_config = Config
module Raml_event = Event
open Std

let ( let* ) = Result.and_then

let compilation_of_frontend_and_backend = fun (frontend: Frontend_pipeline.t) backend_result ->
  Raml_compilation.create
    ~targeting:frontend.targeting
    ~source:frontend.source
    ~typing:frontend.typing.json
    ~core_ir:frontend.core_ir.json
    ~lowering_fields:backend_result.Backend_result.lowered_fields
    ~codegen_fields:backend_result.Backend_result.codegen_fields

let compile_loaded_source = fun ~config ~relpath ~source ->
  let* () = Raml_config.validate config in
  let* frontend = Frontend_pipeline.compile_source ~config ~relpath ~source in
  let module Backend = (val Backend_compile.select ~config) in
  let backend_result = Backend.compile ~config ~frontend in
  Ok (frontend, backend_result)

let compile_source = fun ?(config = Raml_config.default) ~relpath source ->
  Raml_config.emit_event config (fun () -> Raml_event.CompileStarted { path = relpath });
  match compile_loaded_source ~config ~relpath ~source with
  | Error message ->
      Raml_config.emit_event config (fun () -> Raml_event.CompileFailed { path = relpath; message });
      Error message
  | Ok (frontend, backend_result) ->
      Frontend_pipeline.emit_events config ~path:relpath frontend;
      Backend_result.emit_events config ~path:relpath backend_result;
      Raml_config.emit_event config (fun () -> Raml_event.CompileFinished { path = relpath });
      Ok (compilation_of_frontend_and_backend frontend backend_result)

let compile = fun ?(config = Raml_config.default) path ->
  Raml_config.emit_event config (fun () -> Raml_event.CompileStarted { path });
  match Fs.read path with
  | Error err ->
      let message = IO.error_message err in
      Raml_config.emit_event config (fun () -> Raml_event.CompileFailed { path; message });
      Error message
  | Ok source -> (
      match compile_loaded_source ~config ~relpath:path ~source with
      | Error message ->
          Raml_config.emit_event config (fun () -> Raml_event.CompileFailed { path; message });
          Error message
      | Ok (frontend, backend_result) ->
          Frontend_pipeline.emit_events config ~path frontend;
          Backend_result.emit_events config ~path backend_result;
          Raml_config.emit_event config (fun () -> Raml_event.CompileFinished { path });
          Ok (compilation_of_frontend_and_backend frontend backend_result)
    )
