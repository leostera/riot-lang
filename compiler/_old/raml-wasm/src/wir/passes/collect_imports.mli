(** This pass discovers runtime and host imports from an already-lowered `WIR`.

    The algorithm walks globals, functions, and init expressions, asks
    `Runtime_imports` whether a direct callee or primitive implies an import,
    and accumulates the answers in a deduplicated ordered set.

    The effect is that import discovery becomes an explicit wasm backend step
    instead of an incidental side effect of lowering.

    The rationale is simple: imports are part of the wasm backend contract. They
    should be visible as a pass result, easy to snapshot, and easy to replace
    later with a richer artifact story. *)
module Types = Types

val program: Types.Compilation_unit.t -> Types.Compilation_unit.t
