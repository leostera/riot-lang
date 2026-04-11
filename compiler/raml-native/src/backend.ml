module Compiler_config = RamlCore.Config
module Frontend_pipeline = RamlCore.Frontend_pipeline
module Backend_result = RamlCore.Backend_result
module Pipeline_stage = RamlCore.Pipeline_stage
module Event = RamlCore.Event
open Std

let compile = fun ~config ~(frontend: Frontend_pipeline.t) ->
  let core_ir = Frontend_pipeline.core_ir frontend in
  match core_ir.value with
  | None -> Backend_result.blocked_native ~blocked_on:"core_ir" core_ir.errors
  | Some compilation_unit ->
      let nir =
        match Native.Nir.Lowering.lower_compilation_unit_with_trace compilation_unit with
        | Ok trace -> Pipeline_stage.ok_with_json ~json:(Native.Nir.Lowering.trace_to_json trace) trace.final
        | Error errors -> Pipeline_stage.error
          ~stage:"nir"
          (List.map Native.Nir.Lowering.error_to_json errors)
      in
      let mir =
        match nir.value with
        | None -> Pipeline_stage.blocked ~blocked_on:"nir" nir.errors
        | Some program ->
            let trace = Native.Mir.Lowering.lower_program_with_trace program in
            Pipeline_stage.ok_with_json ~json:(Native.Mir.Lowering.trace_to_json trace) trace.final
      in
      let lir =
        match mir.value with
        | None -> Pipeline_stage.blocked ~blocked_on:"mir" mir.errors
        | Some program ->
            let trace = Native.Lir.Lowering.lower_program_with_trace program in
            Pipeline_stage.ok_with_json ~json:(Native.Lir.Lowering.trace_to_json trace) trace.final
      in
      let native =
        match lir.value with
        | None -> Pipeline_stage.blocked ~blocked_on:"lir" lir.errors
        | Some program -> (
            match Native.Emitter.emit_program
              ~host:(Compiler_config.host config)
              ~target:(Compiler_config.target config)
              program with
            | Ok output -> Pipeline_stage.ok ~key:"output" ~render:Std.Data.Json.string output
            | Error error -> Pipeline_stage.error
              ~stage:"native_codegen"
              [ Native.Emitter.error_to_json error ]
          )
      in
      Backend_result.make ~lowered_fields:[
        ("jir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("nir", nir.json);
        ("mir", mir.json);
        ("lir", lir.json);
        ("wasm", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
      ] ~codegen_fields:[
        ("js", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("native", native.json);
        ("wasm", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
      ] ~lowering_events:[
        Backend_result.lowering_event_of_stage Event.Nir nir;
        Backend_result.lowering_event_of_stage Event.Mir mir;
        Backend_result.lowering_event_of_stage Event.Lir lir;
      ] ~codegen_status:(Pipeline_stage.status native)
        ~message:(
          if Pipeline_stage.status native = Event.Ok then
            None
          else
            Some (Pipeline_stage.error_message ~default:"native backend failed" native)
        )
