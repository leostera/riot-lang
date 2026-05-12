(** This module is the first wasm-specific runtime boundary.

    The algorithm here is intentionally simple: look at the surface name of a
    direct callee or primitive and, when it is one of the runtime services we
    already know about, classify it as a wasm runtime import.

    The effect is that `WIR` can carry explicit imports even before wasm codegen
    exists. That keeps runtime obligations out of the emitter and makes them
    visible in snapshots.

    The rationale is the same one we already applied on the native side: if the
    backend depends on runtime helpers, that dependency should appear in the
    backend IR, not be guessed later by codegen. *)
module Core = Raml_core.Core_ir

module Wasm_types = Types

val classify_primitive: Core.Primitive.t -> Wasm_types.Primitive_kind.t

val import_of_primitive: Core.Primitive.t -> Wasm_types.Import.t option

val import_of_direct_callee: Core.Entity_id.t -> Wasm_types.Import.t option
