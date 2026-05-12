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
    function_table_element_count: int;
    has_indirect_calls: bool;
    needs_closure_runtime: bool;
  }
  val from_compilation_unit: Wasm_types.Compilation_unit.t -> t

  val to_json: t -> Std.Data.Json.t
end

module Object: sig
  type t = {
    unit_name: string;
    summary: Module_summary.t;
    program: Wasm_types.Compilation_unit.t;
  }
  val from_compilation_unit: Wasm_types.Compilation_unit.t -> t

  val to_json: t -> Std.Data.Json.t
end

module Linked_program: sig
  type t = {
    objects: Object.t list;
    imports: Wasm_types.Import.t list;
    exports: Raml_core.Core_ir.Export.t list;
    function_table_elements: Raml_core.Core_ir.Entity_id.t list;
    needs_closure_runtime: bool;
  }
  val link: Object.t list -> t

  val to_json: t -> Std.Data.Json.t
end
