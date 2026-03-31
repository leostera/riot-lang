open Std

(** Direct child expressions of an expression node.

    This only returns immediate expression children. Wrapper families such as
    module expressions are traversed by the higher-level folds below.
*)

(** Shared traversal helpers over the typed CST.

    These helpers are deliberately mechanical. They answer questions like
    "what are the direct child expressions of this expression?" and provide
    reusable folds over recursive CST families so downstream tools do not need
    to hand-roll visitors for every rule.
*)
val children_of_expression: Cst.Expression.t -> Cst.Expression.t list

(** Pre-order fold over all expressions reachable from the root expression. *)

(** Iterate over all expressions reachable from the root expression in pre-order. *)
val fold_expression: ('acc -> Cst.Expression.t -> 'acc) -> 'acc -> Cst.Expression.t -> 'acc

val iter_expression: (Cst.Expression.t -> unit) -> Cst.Expression.t -> unit

(** [exists_expression predicate expr] returns [true] if any reachable
    expression satisfies [predicate]. *)
val exists_expression: (Cst.Expression.t -> bool) -> Cst.Expression.t -> bool

(** Direct child core types of a core type node. *)
val children_of_core_type: Cst.CoreType.t -> Cst.CoreType.t list

(** Pre-order fold over all core types reachable from the root core type. *)

(** Iterate over all core types reachable from the root core type in pre-order. *)
val fold_core_type: ('acc -> Cst.CoreType.t -> 'acc) -> 'acc -> Cst.CoreType.t -> 'acc

val iter_core_type: (Cst.CoreType.t -> unit) -> Cst.CoreType.t -> unit

(** [exists_core_type predicate type_] returns [true] if any reachable core
    type satisfies [predicate]. *)
val exists_core_type: (Cst.CoreType.t -> bool) -> Cst.CoreType.t -> bool
