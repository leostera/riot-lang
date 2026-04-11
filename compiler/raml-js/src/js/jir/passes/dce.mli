(** Performs local dead code elimination on late JIR.

    Algorithm:
    - walk blocks backwards and compute a set of used [Entity_id] values
    - collect the set of entities assigned anywhere in the program
    - treat assignment expressions as uses of their right-hand side, not as
      reads of the assigned target
    - keep return values, impure expressions, and anything reachable from them
    - drop [const] declarations whose binder is unused, not protected, and
      whose initializer is pure
    - drop dead [let] declarations only when the binder is also never assigned,
      so the declaration is not needed as storage for a later write
    - in effect position, erase dead local assignments or keep only their
      right-hand-side effects when the assigned local is never read afterwards
    - collapse [if] statements whose branches become empty, so pure conditions
      stop keeping dead dependencies alive until a later normalization pass
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
val program: context:Raml_core.Compilation_context.t -> Types.Program.t -> Types.Program.t
