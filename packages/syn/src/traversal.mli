open Std

(** Shared traversal helpers over the typed CST.

    These helpers are deliberately mechanical. They answer questions like
    "what are the direct child expressions of this expression?" and provide
    reusable folds over recursive CST families so downstream tools do not need
    to hand-roll visitors for every rule.
*)

val children_of_expression : Cst.Expression.t -> Cst.Expression.t list
(** Direct child expressions of an expression node.

    This only returns immediate expression children. Wrapper families such as
    module expressions are traversed by the higher-level folds below.
*)

val fold_expression :
  ('acc -> Cst.Expression.t -> 'acc) -> 'acc -> Cst.Expression.t -> 'acc
(** Pre-order fold over all expressions reachable from the root expression. *)

val iter_expression : (Cst.Expression.t -> unit) -> Cst.Expression.t -> unit
(** Iterate over all expressions reachable from the root expression in pre-order. *)

val exists_expression : (Cst.Expression.t -> bool) -> Cst.Expression.t -> bool
(** [exists_expression predicate expr] returns [true] if any reachable
    expression satisfies [predicate]. *)

val children_of_core_type : Cst.CoreType.t -> Cst.CoreType.t list
(** Direct child core types of a core type node. *)

val fold_core_type :
  ('acc -> Cst.CoreType.t -> 'acc) -> 'acc -> Cst.CoreType.t -> 'acc
(** Pre-order fold over all core types reachable from the root core type. *)

val iter_core_type : (Cst.CoreType.t -> unit) -> Cst.CoreType.t -> unit
(** Iterate over all core types reachable from the root core type in pre-order. *)

val exists_core_type : (Cst.CoreType.t -> bool) -> Cst.CoreType.t -> bool
(** [exists_core_type predicate type_] returns [true] if any reachable core
    type satisfies [predicate]. *)
