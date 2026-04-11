(** This pass removes wasm-level definitions that are not reachable from the
    module's live roots.

    The algorithm is conservative:

    - treat exports and init items as roots
    - walk reachable globals and functions
    - follow direct calls and top-level value references
    - keep init-bound globals even if they are otherwise unused, because init is
      part of module semantics

    The effect is that dead top-level functions disappear before later wasm
    passes or artifact generation inspect the program.

    The rationale is the same as on the native side: once the backend owns a
    low-level IR, it should stop carrying obviously unreachable definitions
    through every later stage. *)
module Types = Types

val program: Types.Compilation_unit.t -> Types.Compilation_unit.t
