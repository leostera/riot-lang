(** Removes imports whose local binder is no longer referenced.

    Algorithm:
    - collect the [Entity_id] values used by the final body and exports
    - keep only import requirements whose local binder entity is in that used
      set

    Effect:
    - dead imports introduced earlier in lowering or preserved through
      materialization/alias removal disappear from the final program

    Rationale:
    - import materialization and late alias cleanup can strand imports that are
      no longer read anywhere
    - this is local import cleanup, not full cross-module tree shaking
*)
val program: context:Raml_core.Compilation_context.t -> Types.Program.t -> Types.Program.t
