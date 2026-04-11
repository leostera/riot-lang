(** This pass makes the first wasm runtime obligations explicit.

    The algorithm walks the lowered program and records three things:

    - whether the unit contains any indirect calls
    - whether nested lambdas mean closure runtime support is required
    - which top-level functions escape as values and therefore need stable
      function-table entries

    The effect is a `WIR` program whose runtime-facing obligations are no
    longer implicit in arbitrary expression trees.

    The rationale is that wasm codegen should not have to rediscover these
    facts while emitting code. They are backend planning facts, so they belong
    in an explicit pass. *)
module Types = Types

val program: Types.Compilation_unit.t -> Types.Compilation_unit.t
