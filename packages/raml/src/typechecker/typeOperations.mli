(** {1 Type Operations}

    Basic operations on type expressions for type checking and inference.

    This module provides the fundamental operations needed for Hindley-Milner
    type inference with let-polymorphism.

    {b For beginners:} Think of types as having "links" (like pointers) that
    connect type variables to their actual types during unification. This module
    helps us follow those links, check for circular types, and manage type
    levels for polymorphism. *)

(** {2 Following Type Links} *)

val follow_links : Types.type_expr -> Types.type_expr
(** Follow chains of type links to get the canonical type.

    During unification, type variables get "linked" to other types. This
    function follows those links to find the actual type.

    Example:
    {[
      (* After unifying 'a with int *)
      let actual_type = follow_links type_variable_a in
      (* actual_type is now int, not 'a *)
    ]}

    @param type_expr A type expression (possibly a linked variable)
    @return The canonical type (with all links followed) *)

(** {2 Type Traversal} *)

val iter_type_expr : (Types.type_expr -> unit) -> Types.type_expr -> unit
(** Apply a function to every node in a type expression tree.

    Recursively walks through the entire type structure, applying the given
    function to each type node. Useful for:
    - Setting type levels
    - Collecting free variables
    - Checking type properties

    Example:
    {[
      (* Print all type variables in a type *)
      iter_type_expr
        (fun t ->
          match (follow_links t).desc with
          | Variable (Some name) -> print_endline name
          | _ -> ())
        my_type
    ]}

    @param f Function to apply to each type node
    @param type_expr The type to traverse *)

(** {2 Occurs Check} *)

val occurs_in_type : int -> Types.type_expr -> bool
(** Check if a type variable occurs within a type (the "occurs check").

    This prevents infinite types during unification. When trying to unify a type
    variable 'a with a type T, we must verify that 'a doesn't occur in T,
    otherwise we'd create a circular definition.

    {b Why this matters:} Without the occurs check, we could create nonsense
    types like:
    {[
      let f x = f x
      (* Would create: 'a = 'a -> 'b *)
      (* This means 'a equals a function taking 'a, which equals
         a function taking a function taking 'a, ... infinite! *)
    ]}

    Example:
    {[
      let var_id = type_var.id in
      if occurs_in_type var_id other_type then
        Error "Occurs check: would create infinite type"
      else
        (* Safe to unify *)
        Ok ()
    ]}

    @param id The unique ID of the type variable to search for
    @param type_expr The type expression to search in
    @return true if the variable occurs in the type, false otherwise *)

(** {2 Level Management} *)

val set_level : int -> Types.type_expr -> unit
(** Set the level of all type variables in a type expression.

    Type levels implement let-polymorphism. Variables at higher levels can be
    generalized (made polymorphic) when leaving their scope.

    {b Key insight:} Type levels track nesting depth of let-bindings. Variables
    introduced in inner scopes have higher levels.

    Example:
    {[
      (* Level 0: top-level *)
      let id = fun x -> x

      (* x has level 1 *)
      (* Generalize level 1 variables: id : forall 'a. 'a -> 'a *)
      let _ = id 42 (* Instantiate 'a to int *)
      let _ = id "hi" (* Instantiate 'a to string *)
    ]}

    @param level The level to set for all variables
    @param type_expr The type expression to update *)

val update_level : int -> Types.type_expr -> unit
(** Lower type variable levels to be no higher than the given level.

    During unification, if we link a level-L1 variable to a type containing
    level-L2 variables (where L2 > L1), we must lower L2 to L1. This prevents
    type variables from "escaping" their scope.

    {b Why this matters:} Without level updating, polymorphic types could escape
    their scope:
    {[
      let f () =
        let r = ref None in              (* 'a ref at level 1 *)
        let g x = r := Some x in         (* x at level 2 *)
        (* If we allowed this, x could escape its scope via r *)
        (* ERROR: level 2 variable can't be stored in level 1 ref *)
    ]}

    @param current_level Maximum level allowed
    @param type_expr The type expression to update *)

(** {2 Type Creation} *)

val new_type_variable :
  ctx:Types.context -> int -> Types.type_expr * Types.context
(** Create a fresh type variable at the specified level.

    Type variables represent unknown types during type inference. Each variable
    has:
    - A unique ID (never reused, prevents confusion)
    - A level (for let-polymorphism)
    - Initially no link (will be filled in during unification)

    Example:
    {[
      let ctx = Types.create_context () in
      let arg_var, ctx = new_type_variable ~ctx 1 in
      let ret_var, ctx = new_type_variable ~ctx 1 in
      (* Now we have two distinct unknowns at level 1 *)
    ]}

    @param ctx The typing context (provides fresh IDs, no global state!)
    @param level The level for the new variable
    @return Tuple of (fresh type variable, updated context) *)

val new_generic_type :
  ctx:Types.context -> Types.type_desc -> Types.type_expr * Types.context
(** Create a new type expression with the given description.

    Unlike [new_type_variable], this can create any kind of type: arrows,
    tuples, constructors, etc. Used when building complex types during
    instantiation and type construction.

    Example:
    {[
      (* Create an int -> string function type *)
      let arrow, ctx = new_generic_type ~ctx
        (Types.Arrow (Nolabel, int_type, string_type)) in
    ]}

    @param ctx The typing context
    @param desc The type description (Variable, Arrow, Tuple, Constructor, etc.)
    @return Tuple of (new type expression, updated context) *)
