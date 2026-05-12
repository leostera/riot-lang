module Compiler_config = Raml_core.Config
module Frontend_pipeline = Raml_core.Frontend_pipeline
module Target = Raml_core.Target
open Std
open Std.Data

type t = {
  targeting: Json.t;
  source: Json.t;
  typing: Json.t;
  lowered: Json.t;
  codegen: Json.t;
}

let compile_source = fun ~config ~relpath ~source ->
  Result.map
    (Frontend_pipeline.compile_source ~config ~relpath ~source)
    ~fn:(fun (frontend: Frontend_pipeline.t) ->
      let backend_result =
        match Compiler_config.select_backend config with
        | Target.Js -> Raml_js.Backend.compile ~config ~frontend
        | Target.Native -> Raml_native.Backend.compile ~config ~frontend
        | Target.Wasm -> Raml_wasm.Backend.compile ~config ~frontend
      in
      {
        targeting = frontend.targeting;
        source = frontend.source;
        typing = frontend.typing.json;
        lowered = Json.obj (("core_ir", frontend.core_ir.json) :: backend_result.lowered_fields);
        codegen = Json.obj backend_result.codegen_fields;
      })

let to_json = fun pipeline ->
  Json.obj
    [
      ("targeting", pipeline.targeting);
      ("source", pipeline.source);
      ("typing", pipeline.typing);
      ("lowered", pipeline.lowered);
      ("codegen", pipeline.codegen);
    ]

let lowering_to_json = fun pipeline ->
  Json.obj
    [
      ("targeting", pipeline.targeting);
      ("source", pipeline.source);
      ("typing", pipeline.typing);
      ("lowered", pipeline.lowered);
    ]

let codegen_to_json = to_json
