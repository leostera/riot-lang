open Std
module Backend_result = Raml_core.Backend_result
module Frontend_pipeline = Raml_core.Frontend_pipeline
module Pipeline_stage = Raml_core.Pipeline_stage
module Event = Raml_core.Event
module Wir = Wir
module Codegen = Codegen
module Artifact_store = Artifact_store

let artifact_store_error_to_json = fun error ->
  Std.Data.Json.obj
    [
      ("kind", Std.Data.Json.string "artifact_store_error");
      ("error", Artifact_store.error_to_json error);
    ]

let compile = fun ~config ~(frontend:Frontend_pipeline.t) ->
  let core_ir = Frontend_pipeline.core_ir frontend in
  match core_ir.value with
  | None -> Backend_result.blocked_wasm ~blocked_on:"core_ir" core_ir.errors
  | Some compilation_unit ->
      let trace = Wir.Lowering.lower_compilation_unit_with_trace compilation_unit in
      let object_ = Wir.Artifacts.Object.from_compilation_unit trace.final in
      let linked_program = Wir.Artifacts.Linked_program.link [ object_ ] in
      let persisted_object, wasm =
        match Artifact_store.from_config config with
        | None -> (
          None,
          Pipeline_stage.ok_with_json
            ~json:Std.Data.Json.(obj
              [
                ("trace", Wir.Lowering.trace_to_json trace);
                ("object", Wir.Artifacts.Object.to_json object_);
                ("linked_program", Wir.Artifacts.Linked_program.to_json linked_program);
              ])
            trace.final
        )
        | Some store -> (
            match (
              Artifact_store.save_object store ~object_,
              Artifact_store.save_linked_program store ~linked_program
            ) with
            | Ok stored_object, Ok stored_linked_program -> (
              Some stored_object,
              Pipeline_stage.ok_with_json
                ~json:Std.Data.Json.(obj
                  [
                    ("trace", Wir.Lowering.trace_to_json trace);
                    ("object", Wir.Artifacts.Object.to_json object_);
                    ("linked_program", Wir.Artifacts.Linked_program.to_json linked_program);
                    ("stored_object_id", string stored_object.id);
                    ("stored_linked_program_id", string stored_linked_program.id);
                  ])
                trace.final
            )
            | (Error error, _)
            | (_, Error error) -> (
              None,
              Pipeline_stage.error
                ~stage:"wasm_artifact_store"
                [ artifact_store_error_to_json error ]
            )
          )
      in
      let wasm_codegen =
        match wasm.value with
        | None -> Pipeline_stage.blocked ~blocked_on:"wasm" wasm.errors
        | Some _ -> (
            match Codegen.emit_linked_program linked_program with
            | Ok artifact -> (
                match Artifact_store.from_config config with
                | None -> Pipeline_stage.ok_with_json ~json:(Codegen.artifact_to_json artifact) artifact
                | Some store -> (
                    let unit_name =
                      match persisted_object with
                      | Some stored_object -> Some stored_object.unit_name
                      | None -> Some object_.unit_name
                    in
                    match Artifact_store.save_module store ?unit_name artifact with
                    | Ok stored_module ->
                        let json =
                          match Codegen.artifact_to_json artifact with
                          | Std.Data.Json.Object fields -> Std.Data.Json.Object (fields
                          @ [ ("stored_module_id", Std.Data.Json.string stored_module.id) ])
                          | json -> json
                        in
                        Pipeline_stage.ok_with_json ~json artifact
                    | Error error -> Pipeline_stage.error
                      ~stage:"wasm_artifact_store"
                      [ artifact_store_error_to_json error ]
                  )
              )
            | Error errors -> Pipeline_stage.error
              ~stage:"wasm_codegen"
              (List.map errors ~fn:Codegen.error_to_json)
          )
      in
      Backend_result.make ~lowered_fields:[
        ("jir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("nir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("mir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("lir", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("wasm", wasm.json);
      ] ~codegen_fields:[
        ("js", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("native", Backend_result.unavailable_stage_json ~reason:"backend_not_selected");
        ("wasm", wasm_codegen.json);
      ] ~lowering_events:[ Backend_result.lowering_event_of_stage Event.Wasm wasm ] ~codegen_status:(Pipeline_stage.status
        wasm_codegen)
        ~message:(
          if Pipeline_stage.status wasm_codegen = Event.Ok then
            None
          else
            Some (Pipeline_stage.error_message ~default:"wasm backend failed" wasm_codegen)
        )
