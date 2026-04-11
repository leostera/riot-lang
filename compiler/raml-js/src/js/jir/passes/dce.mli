(** Performs local dead code elimination on late JIR.

    Algorithm:
    - walk blocks backwards and compute a set of used [Entity_id] values
    - keep return values, impure expressions, and anything reachable from them
    - drop [const] declarations whose binder is unused, not protected, and
      whose initializer is pure
    - drop pure expression statements entirely
    - preserve exported entities through the [protected] set

    Effect:
    - unused pure declarations and no-op statements are removed from the final
      JIR body
    - observable behavior is preserved because impure work is retained

    Rationale:
    - earlier passes intentionally expose direct data flow and remove aliases
    - this pass is the local cleanup step that removes the dead scaffolding
      those earlier transforms uncovered
*)
val program: Types.Program.t -> Types.Program.t
