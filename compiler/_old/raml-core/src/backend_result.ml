module Compiler_config = Config
open Std
open Std.Data

type lowering_event = {
  backend: Event.backend;
  status: Event.status;
  error_count: int;
}

type t = {
  lowered_fields: (string * Json.t) list;
  codegen_fields: (string * Json.t) list;
  lowering_events: lowering_event list;
  codegen_status: Event.status;
  message: string option;
}

let make = fun ~lowered_fields ~codegen_fields ~lowering_events ~codegen_status ~message ->
  {
    lowered_fields;
    codegen_fields;
    lowering_events;
    codegen_status;
    message;
  }

let lowering_event_of_stage = fun backend (stage: 'value Pipeline_stage.t) ->
  { backend; status = Pipeline_stage.status stage; error_count = List.length stage.errors }

let unavailable_stage_json = fun ~reason -> (Pipeline_stage.unavailable ~reason).json

let blocked_js = fun ~blocked_on errors ->
  let jir = Pipeline_stage.blocked ~blocked_on errors in
  let js = Pipeline_stage.blocked ~blocked_on:"jir" jir.errors in
  {
    lowered_fields = [
      ("jir", jir.json);
      ("nir", unavailable_stage_json ~reason:"backend_not_selected");
      ("mir", unavailable_stage_json ~reason:"backend_not_selected");
      ("lir", unavailable_stage_json ~reason:"backend_not_selected");
      ("wasm", unavailable_stage_json ~reason:"backend_not_selected");
    ];
    codegen_fields = [
      ("js", js.json);
      ("native", unavailable_stage_json ~reason:"backend_not_selected");
      ("wasm", unavailable_stage_json ~reason:"backend_not_selected");
    ];
    lowering_events = [ lowering_event_of_stage Event.Jir jir ];
    codegen_status = Pipeline_stage.status js;
    message = Some (Pipeline_stage.error_message ~default:"js backend blocked" js);
  }

let blocked_native = fun ~blocked_on errors ->
  let nir = Pipeline_stage.blocked ~blocked_on errors in
  let mir = Pipeline_stage.blocked ~blocked_on:"nir" nir.errors in
  let lir = Pipeline_stage.blocked ~blocked_on:"mir" mir.errors in
  let native = Pipeline_stage.blocked ~blocked_on:"lir" lir.errors in
  {
    lowered_fields = [
      ("jir", unavailable_stage_json ~reason:"backend_not_selected");
      ("nir", nir.json);
      ("mir", mir.json);
      ("lir", lir.json);
      ("wasm", unavailable_stage_json ~reason:"backend_not_selected");
    ];
    codegen_fields = [
      ("js", unavailable_stage_json ~reason:"backend_not_selected");
      ("native", native.json);
      ("wasm", unavailable_stage_json ~reason:"backend_not_selected");
    ];
    lowering_events = [
      lowering_event_of_stage Event.Nir nir;
      lowering_event_of_stage Event.Mir mir;
      lowering_event_of_stage Event.Lir lir;
    ];
    codegen_status = Pipeline_stage.status native;
    message = Some (Pipeline_stage.error_message ~default:"native backend blocked" native);
  }

let blocked_wasm = fun ~blocked_on errors ->
  let wasm_lowering = Pipeline_stage.blocked ~blocked_on errors in
  let wasm_codegen = Pipeline_stage.blocked ~blocked_on:"wasm" wasm_lowering.errors in
  {
    lowered_fields = [
      ("jir", unavailable_stage_json ~reason:"backend_not_selected");
      ("nir", unavailable_stage_json ~reason:"backend_not_selected");
      ("mir", unavailable_stage_json ~reason:"backend_not_selected");
      ("lir", unavailable_stage_json ~reason:"backend_not_selected");
      ("wasm", wasm_lowering.json);
    ];
    codegen_fields = [
      ("js", unavailable_stage_json ~reason:"backend_not_selected");
      ("native", unavailable_stage_json ~reason:"backend_not_selected");
      ("wasm", wasm_codegen.json);
    ];
    lowering_events = [ lowering_event_of_stage Event.Wasm wasm_lowering ];
    codegen_status = Pipeline_stage.status wasm_codegen;
    message = Some (Pipeline_stage.error_message ~default:"wasm backend blocked" wasm_codegen);
  }

let unavailable_wasm = fun ~message ->
  let wasm_lowering = Pipeline_stage.unavailable ~reason:"wasm_lowering_not_implemented" in
  let wasm_codegen = Pipeline_stage.unavailable ~reason:"wasm_codegen_not_implemented" in
  {
    lowered_fields = [
      ("jir", unavailable_stage_json ~reason:"backend_not_selected");
      ("nir", unavailable_stage_json ~reason:"backend_not_selected");
      ("mir", unavailable_stage_json ~reason:"backend_not_selected");
      ("lir", unavailable_stage_json ~reason:"backend_not_selected");
      ("wasm", wasm_lowering.json);
    ];
    codegen_fields = [
      ("js", unavailable_stage_json ~reason:"backend_not_selected");
      ("native", unavailable_stage_json ~reason:"backend_not_selected");
      ("wasm", wasm_codegen.json);
    ];
    lowering_events = [ lowering_event_of_stage Event.Wasm wasm_lowering ];
    codegen_status = Pipeline_stage.status wasm_codegen;
    message = Some message;
  }

let emit_events = fun config ~path result ->
  List.for_each
    result.lowering_events
    ~fn:(fun lowering_event ->
      Compiler_config.emit_event
        config
        (fun () ->
          Event.LoweringFinished {
            path;
            backend = lowering_event.backend;
            status = lowering_event.status;
            error_count = lowering_event.error_count
          }));
  Compiler_config.emit_event
    config
    (fun () ->
      Event.CodegenFinished {
        path;
        target = Compiler_config.target config;
        status = result.codegen_status
      })
