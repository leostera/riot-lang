(** This is the first wasm lowering step.

    The algorithm is intentionally direct:

    - traverse `Core_ir`
    - split top-level lambdas into wasm functions
    - keep non-lambda bindings as globals or init items
    - thread the raw lowered unit through explicit wasm passes

    The effect is a backend-owned `WIR` unit that already exposes wasm concerns
    native does not care about, especially explicit imports and a clearer split
    between functions, globals, and initialization.

    The rationale is to give `raml-wasm` a real boundary of its own before we
    talk about Binaryen, Wasm GC, or object artifacts. *)
module Core = Raml_core.Core_ir

module Wasm_types = Types

type pass_snapshot = {
  name: string;
  program: Wasm_types.Compilation_unit.t;
}
type trace = {
  initial: Wasm_types.Compilation_unit.t;
  passes: pass_snapshot list;
  final: Wasm_types.Compilation_unit.t;
}
val lower_compilation_unit: Core.Compilation_unit.t -> Wasm_types.Compilation_unit.t

val lower_compilation_unit_with_trace: Core.Compilation_unit.t -> trace

val trace_to_json: trace -> Std.Data.Json.t
