(** This is a small structural cleanup pass over `WIR`.

    The algorithm is local and recursive:

    - normalize every expression tree
    - fold `if` when the condition is a known boolean constant
    - drop `sequence` nodes whose first expression is the unit constant
    - remove top-level `Eval ()` items that have no effect

    The effect is a cleaner wasm-facing IR before later passes inspect it for
    imports, summaries, or codegen obligations.

    The rationale is to keep later wasm passes focused on backend obligations
    rather than on trivial source-shaped clutter. *)
module Types = Types

val program: Types.Compilation_unit.t -> Types.Compilation_unit.t
