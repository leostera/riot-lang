open Std

(** `raml-wasm` owns the meaning of wasm artifacts.

    The caller provides one raw [Contentstore.t] through [Raml_core.Config].
    This module builds wasm-specific semantics on top of it: where wasm objects,
    linked programs, and runnable modules live, how they are keyed, and what
    metadata comes back when one is loaded. The serialization format stays
    internal here. *)
type t
type error =
  | Save_failed of { namespace: string; key: string; message: string }
  | Decode_failed of { namespace: string; key: string; message: string }
module Module_summary: sig
  type t = Wir.Artifacts.Module_summary.t = {
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
end

module Object_artifact: sig
  type t = {
    id: string;
    unit_name: string;
    summary: Module_summary.t;
    payload: Std.Data.Json.t;
  }
end

module Linked_program_artifact: sig
  type t = {
    id: string;
    unit_names: string list;
    imports: string list;
    exports: string list;
    needs_closure_runtime: bool;
    payload: Std.Data.Json.t;
  }
end

module Module_artifact: sig
  type t = {
    id: string;
    unit_name: string option;
    size_bytes: int;
    memory_pages: int;
    wasm_base64: string;
    node_runner: string;
    payload: Std.Data.Json.t;
  }
end

val create: Contentstore.t -> target:Raml_core.Target.t -> unit -> t

val from_config: Raml_core.Config.t -> t option

val target: t -> Raml_core.Target.t

val error_to_json: error -> Std.Data.Json.t

val save_object: t -> object_:Wir.Artifacts.Object.t -> (Object_artifact.t, error) result

val load_object: t -> id:string -> Object_artifact.t option

val find_object_by_unit_name: t -> unit_name:string -> Object_artifact.t option

val save_linked_program:
  t -> linked_program:Wir.Artifacts.Linked_program.t -> (Linked_program_artifact.t, error) result

val load_linked_program: t -> id:string -> Linked_program_artifact.t option

val find_linked_program_by_unit_name: t -> unit_name:string -> Linked_program_artifact.t option

val save_module: t -> ?unit_name:string -> Codegen.artifact -> (Module_artifact.t, error) result

val load_module: t -> id:string -> Module_artifact.t option

val find_module_by_unit_name: t -> unit_name:string -> Module_artifact.t option
