open Std

type package = string

type derivation_tree =
  | External of Incompatibility.external_cause * Term.t list
  | Derived of {
      terms : Term.t list;
      cause1 : derivation_tree;
      cause2 : derivation_tree;
      shared_id : int option;
    }

val build_derivation_tree : Incompatibility.t -> derivation_tree
val explain_conflict : Incompatibility.t -> string
val format_derivation_tree : derivation_tree -> string
