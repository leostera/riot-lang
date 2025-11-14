open Std

(** {1 Substitution - Variable Bindings}
    
    A substitution maps variable names to concrete values.
    Used during:
    - Unification (pattern matching)
    - Query evaluation (binding query variables)
    - Rule evaluation (binding variables in rule bodies)
    
    {2 Example}
    
    {[
      let sub = empty () in
      let sub = bind sub ~var:"X" ~value:(Value.Int 42) in
      let sub = bind sub ~var:"Y" ~value:(Value.String "hello") in
      
      (* Apply to term *)
      apply_to_term sub (Term.Var "X")  (* Term.Const (Value.Int 42) *)
      
      (* Apply to atom *)
      let atom = Ast.atom ~predicate:"foo" 
        ~args:[Term.Var "X"; Term.Var "Y"] in
      apply_to_atom sub atom
      (* Returns: foo(42, "hello") *)
    ]}
*)

type t
(** A variable substitution (variable name → value) *)

(** {2 Construction} *)

val empty : unit -> t
(** Empty substitution (no bindings) *)

val singleton : var:string -> value:Value.t -> t
(** Single binding *)

val of_list : (string * Value.t) list -> t
(** Create from association list *)

(** {2 Binding} *)

val bind : t -> var:string -> value:Value.t -> t
(** Add a binding. Returns new substitution.
    If variable already bound, overwrites the binding.
*)

val lookup : t -> var:string -> Value.t option
(** Get binding for variable *)

val mem : t -> var:string -> bool
(** Check if variable is bound *)

val unbind : t -> var:string -> t
(** Remove a binding *)

(** {2 Operations} *)

val merge : t -> t -> t option
(** Merge two substitutions if compatible.
    Returns [None] if substitutions conflict (same var, different values).
    
    Example:
    {[
      let s1 = of_list ["X", Int 1; "Y", Int 2] in
      let s2 = of_list ["Y", Int 2; "Z", Int 3] in
      merge s1 s2  (* Some {X→1, Y→2, Z→3} *)
      
      let s3 = of_list ["X", Int 1] in
      let s4 = of_list ["X", Int 2] in
      merge s3 s4  (* None - conflict on X *)
    ]}
*)

val extend : t -> (string * Value.t) list -> t option
(** Extend substitution with new bindings.
    Returns [None] if any binding conflicts with existing ones.
*)

(** {2 Application} *)

val apply_to_term : t -> Term.t -> Term.t
(** Apply substitution to term.
    - [Var x] → [Const v] if x is bound to v
    - [Var x] → [Var x] if x is not bound
    - [Const v] → [Const v] (unchanged)
    - [Wildcard] → [Wildcard] (unchanged)
*)

val apply_to_atom : t -> Ast.atom -> Ast.atom
(** Apply substitution to all terms in atom *)

val apply_to_tuple : t -> Term.t list -> Value.t list option
(** Apply substitution to tuple of terms.
    Returns [Some values] if all terms become constants.
    Returns [None] if any term remains a variable.
    
    Example:
    {[
      let sub = of_list ["X", Int 1; "Y", Int 2] in
      apply_to_tuple sub [Var "X"; Var "Y"]  
      (* Some [Int 1; Int 2] *)
      
      apply_to_tuple sub [Var "X"; Var "Z"]  
      (* None - Z not bound *)
    ]}
*)

(** {2 Introspection} *)

val bindings : t -> (string * Value.t) list
(** All bindings as list *)

val vars : t -> string list
(** All bound variable names *)

val is_empty : t -> bool
(** Check if substitution has no bindings *)

val size : t -> int
(** Number of bindings *)

(** {2 Utilities} *)

val to_string : t -> string
(** Convert to string for debugging: {X→1, Y→"foo"} *)

val equal : t -> t -> bool
(** Check if two substitutions have identical bindings *)
