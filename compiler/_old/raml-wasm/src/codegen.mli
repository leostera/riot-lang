open Std

(** This module turns the linked `WIR` program into the first runnable wasm
    artifact for `raml-wasm`.

    The algorithm is intentionally narrow. It emits a single wasm module with:
    - imported runtime memory
    - imported runtime functions for the supported printing surface
    - static data segments for string literals
    - one `_start` function that runs the ordered top-level init items

    The effect is that the wasm backend can already produce something runnable
    for simple programs like `hello_world`, without waiting for Binaryen,
    closure conversion, or a real separate-compilation linker.

    The rationale is speed. The backend already has enough backend-owned shape
    in `WIR` and `Linked_program` to produce a real executable slice, so the
    fastest honest next step is a minimal binary emitter with explicit
    unsupported cases. *)
module Artifacts = Wir.Artifacts

module Types = Wir.Types

type artifact = {
  wasm_base64: string;
  size_bytes: int;
  memory_pages: int;
  node_runner: string;
}
type error =
  | Unsupported_import of Types.Import.t
  | Unsupported_function of Types.Function.t
  | Unsupported_global of Types.Global.t
  | Unsupported_expr of { context: string; expr: Types.Expr.t }
  | Unsupported_indirect_calls
  | Unsupported_closure_runtime
  | Unsupported_integer of { context: string; value: int }
  | Unsupported_char of { value: string }
val emit_linked_program: Artifacts.Linked_program.t -> (artifact, error list) Result.t

val artifact_to_json: artifact -> Std.Data.Json.t

val error_to_json: error -> Std.Data.Json.t
