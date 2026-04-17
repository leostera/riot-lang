module Frontend_pipeline = Raml_core.Frontend_pipeline
module Backend_result = Raml_core.Backend_result
module Compilation_context = Raml_core.Compilation_context
module Pipeline_stage = Raml_core.Pipeline_stage
module Event = Raml_core.Event
open Std

let compile = fun ~config ~(frontend:Frontend_pipeline.t) ->
  let context = Compilation_context.make ~config ~source:frontend.source_unit in
  let core_ir = Frontend_pipeline.core_ir frontend in
  match core_ir.value with
  | None -> Backend_result.blocked_js ~blocked_on:"core_ir" core_ir.errors
  | Some compilation_unit ->
      let jir =
        match Js.Jir.Lowering.lower_compilation_unit ~context compilation_unit with
        | Ok program -> Pipeline_stage.ok ~key:"program" ~render:Js.Jir.Program.to_json program
        | Error errors -> Pipeline_stage.error
          ~stage:"jir"
          (List.map errors ~fn:Js.Jir.Lowering.error_to_json)
      in
      let js =
        match jir.value with
        | None -> Pipeline_stage.blocked ~blocked_on:"jir" jir.errors
        | Some program ->
            let program = Js.Jst.Lowering.lower_program ~context program in
            Pipeline_stage.ok
              ~key:"output"
              ~render:Std.Data.Json.string
              (Js.Jst.Emitter.emit_program ~context program)
      in
      Backend_result.make ~lowered_fields:[
        ("jir", jir.json);
        ("nir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("mir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("lir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("wasm", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
      ] ~codegen_fields:[
        ("js", js.json);
        ("native", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("wasm", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
      ] ~lowering_events:[ Backend_result.lowering_event_of_stage Event.Jir jir ] ~codegen_status:(Pipeline_stage.status
        js)
        ~message:(
          if Pipeline_stage.status js = Event.Ok then
            None
          else
            Some (Pipeline_stage.error_message ~default:"js backend failed" js)
        )
