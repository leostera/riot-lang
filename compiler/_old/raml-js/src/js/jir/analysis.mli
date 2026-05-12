(** Shared entity-set utilities used by late JIR analysis passes.

    The late JIR passes reason over semantic ids, not printable names, so the
    analysis substrate exposes set operations over [Entity_id]. *)
module Entity_set: sig
  type t
  val empty: t

  val add: Raml_core.Core_ir.Entity_id.t -> t -> t

  val singleton: Raml_core.Core_ir.Entity_id.t -> t

  val mem: Raml_core.Core_ir.Entity_id.t -> t -> bool

  val union: t -> t -> t

  val filter: (Raml_core.Core_ir.Entity_id.t -> bool) -> t -> t
end

(** Returns whether an expression is free of observable effects. *)
val is_pure_expr: Types.Expr.t -> bool

(** Collects entities read by an expression. Assignment targets are not treated
    as reads; only the right-hand side contributes read dependencies. *)
val expr_read_entities: Types.Expr.t -> Entity_set.t -> Entity_set.t

val statement_read_entities: Types.Statement.t -> Entity_set.t -> Entity_set.t

val statements_read_entities: Types.Statement.t list -> Entity_set.t -> Entity_set.t

(** Collects entities read by a whole program, including exported locals. *)
val program_read_entities: Types.Program.t -> Entity_set.t

(** Collects entities assigned anywhere inside an expression. *)
val expr_assigned_entities: Types.Expr.t -> Entity_set.t -> Entity_set.t

val statement_assigned_entities: Types.Statement.t -> Entity_set.t -> Entity_set.t

val statements_assigned_entities: Types.Statement.t list -> Entity_set.t -> Entity_set.t

(** Collects entities assigned by a whole program body. *)
val program_assigned_entities: Types.Program.t -> Entity_set.t
