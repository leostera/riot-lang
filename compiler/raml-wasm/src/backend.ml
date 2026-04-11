open Std
module Backend_result = Raml_core.Backend_result
module Frontend_pipeline = Raml_core.Frontend_pipeline
module Pipeline_stage = Raml_core.Pipeline_stage
module Event = Raml_core.Event
module Wir = Wir

let compile = fun ~config:_ ~(frontend:Frontend_pipeline.t) ->
  let core_ir = Frontend_pipeline.core_ir frontend in
  match core_ir.value with
  | None -> Backend_result.blocked_wasm ~blocked_on:"core_ir" core_ir.errors
  | Some compilation_unit ->
      let program = Wir.Lowering.lower_compilation_unit compilation_unit in
      let summary = Wir.Artifacts.Module_summary.of_compilation_unit program in
      let wasm = Pipeline_stage.ok_with_json
        ~json:Std.Data.Json.(obj
          [
            ("program", Wir.Types.Compilation_unit.to_json program);
            ("summary", Wir.Artifacts.Module_summary.to_json summary);
          ])
        program in
      let wasm_codegen = Pipeline_stage.unavailable ~reason:"wasm_codegen_not_implemented" in
      Backend_result.make
        ~lowered_fields:[
          ("jir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
          ("nir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
          ("mir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
          ("lir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
          ("wasm", wasm.json);
        ]
        ~codegen_fields:[
          ("js", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
          ("native", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
          ("wasm", wasm_codegen.json);
        ]
        ~lowering_events:[ Backend_result.lowering_event_of_stage Event.Wasm wasm ]
        ~codegen_status:(Pipeline_stage.status wasm_codegen)
        ~message:(Some "wasm lowering is scaffolded; wasm codegen is not implemented yet")
