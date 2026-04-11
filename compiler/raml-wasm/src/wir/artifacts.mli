(** This module sketches the separate-compilation boundary for `raml-wasm`.

    The current algorithm is just summary extraction: walk the lowered `WIR`
    compilation unit and record the imports, exports, and rough shape counts we
    would need for a real wasm object artifact later on.

    The effect is that the package already has a place for per-module wasm
    metadata without pretending that the final object format exists yet.

    The rationale comes from both Grain and `wasm_of_ocaml`: separate
    compilation and linking are backend concerns, so `raml-wasm` should own a
    summary artifact early instead of bolting it on after codegen. *)
module Wasm_types = Types

module Module_summary: sig
  type t = {
    unit_name: string;
    imports: string list;
    exports: string list;
    global_count: int;
    function_count: int;
    init_item_count: int;
  }
  val of_compilation_unit: Wasm_types.Compilation_unit.t -> t

  val to_json: t -> Std.Data.Json.t
end
