open Std

(** {1 Type Operations}

    Basic operations on type expressions for type checking and inference.
    
    This module provides utilities for:
    - Following type variable links (unification chains)
    - Checking for circular type references (occurs check)
    - Managing type levels for let-polymorphism
    - Traversing type expressions
    
    {b Key Concepts:}
    
    {b Type Links:} During unification, type variables can be "linked" to other
    types. For example, when we unify ['a] with [int], we create a link:
    ['a] -> [int]. The [follow_links] function follows these chains to get
    the actual type.
    
    {b Type Levels:} Used for let-polymorphism. Each type variable has a level
    indicating where it was introduced. This prevents premature generalization.
    For example:
    {[
      let id = fun x -> x      (* level 0, can be generalized *)
      let _ = id 42            (* instantiate: 'a -> 'a becomes int -> int *)
      let _ = id "hello"       (* instantiate again: 'a -> 'a becomes string -> string *)
    ]}
    
    {b Occurs Check:} Prevents infinite types like ['a = 'a -> 'b].
    Essential for soundness of the type system.
*)

(** {2 Following Links} *)

let follow_links t =
  (** Follow type variable links to get the actual type.
      
      When types are unified, we create links from type variables to their
      actual types. This function follows the chain of links to get the
      final, canonical type.
      
      Example:
      {[
        let t1 = Variable "a"      (* Initial type variable *)
        (* After unifying with int: *)
        t1.desc <- Link int_type
        (* follow_links t1 returns int_type, not the Variable *)
      ]}
      
      @param t The type expression to follow
      @return The canonical type (without intermediate links)
  *)
  let rec follow t =
    match t.Types.desc with
    | Types.Link t' -> follow t'  (* Keep following links *)
    | _ -> t                       (* Found the actual type *)
  in
  follow t

(** {2 Type Traversal} *)

let rec iter_type_expr f ty =
  (** Apply a function to every type node in the type expression tree.
      
      Traverses the entire type expression, applying [f] to each node.
      Useful for operations like:
      - Setting levels on all type variables
      - Collecting free type variables
      - Checking properties of types
      
      Example:
      {[
        (* Count how many type variables are in a type *)
        let count = ref 0 in
        iter_type_expr 
          (fun t -> match t.desc with Variable _ -> Cell.incr count | _ -> ())
          my_type;
        !count
      ]}
      
      @param f Function to apply to each type node
      @param ty The type expression to traverse
  *)
  f ty;
  match (follow_links ty).Types.desc with
  | Types.Variable _ | Types.UniversalVariable _ -> 
      (* Leaf nodes - nothing to recurse into *)
      ()
  | Types.Arrow (_, t1, t2) ->
      (* Function type: traverse argument and return types *)
      iter_type_expr f t1;
      iter_type_expr f t2
  | Types.Tuple tys -> 
      (* Tuple: traverse all element types *)
      List.iter (iter_type_expr f) tys
  | Types.Constructor (_, args) -> 
      (* Type constructor (e.g., list, option): traverse type arguments *)
      List.iter (iter_type_expr f) args
  | Types.Link t -> 
      (* Shouldn't happen (follow_links should handle this), but traverse anyway *)
      iter_type_expr f t
  | Types.Substitution t -> 
      (* Substitution: traverse the substituted type *)
      iter_type_expr f t
  | Types.Polymorphic (t, vars) ->
      (* Polymorphic type (forall): traverse body and bound variables *)
      iter_type_expr f t;
      List.iter (iter_type_expr f) vars

(** {2 Occurs Check} *)

let rec occurs_in_type id ty =
  (** Check if a type variable occurs in a type expression.
      
      This is the famous "occurs check" that prevents infinite types.
      When unifying a type variable ['a] with a type [t], we must check
      that ['a] doesn't occur in [t], otherwise we'd create a circular type.
      
      Example of what we're preventing:
      {[
        (* BAD: Would create infinite type *)
        let f x = f x    (* Tries to unify 'a with 'a -> 'b *)
        (* This would mean: 'a = 'a -> 'b = ('a -> 'b) -> 'b = ... forever *)
      ]}
      
      @param id The ID of the type variable to search for
      @param ty The type expression to search in
      @return true if the variable occurs in the type, false otherwise
  *)
  match (follow_links ty).Types.desc with
  | Types.Variable _ -> 
      (* Check if this is the variable we're looking for *)
      (follow_links ty).Types.id = id
  | Types.Arrow (_, t1, t2) -> 
      (* Check both argument and return type *)
      occurs_in_type id t1 || occurs_in_type id t2
  | Types.Tuple tys -> 
      (* Check if it occurs in any tuple element *)
      List.exists (occurs_in_type id) tys
  | Types.Constructor (_, args) -> 
      (* Check if it occurs in any type argument *)
      List.exists (occurs_in_type id) args
  | Types.Link t -> 
      (* Follow the link and check *)
      occurs_in_type id t
  | Types.Substitution t -> 
      (* Check the substituted type *)
      occurs_in_type id t
  | Types.Polymorphic (t, vars) ->
      (* Check both the body and bound variables *)
      occurs_in_type id t || List.exists (occurs_in_type id) vars
  | Types.UniversalVariable _ -> 
      (* Universal variables are distinct from regular variables *)
      false

(** {2 Level Management} *)

let set_level level ty =
  (** Set the level of all type variables in a type expression.
      
      Type levels are used for let-polymorphism. When we enter a new scope
      (like a let-binding), we increase the level. This helps us know which
      type variables can be generalized.
      
      Example:
      {[
        (* At level 0 (top-level) *)
        let id x = x              (* Type variables at level 1 *)
        (* After checking, generalize variables at level > 0 *)
        (* Result: id : forall 'a. 'a -> 'a *)
      ]}
      
      @param level The level to set
      @param ty The type expression to update
  *)
  iter_type_expr (fun t -> (follow_links t).Types.level <- level) ty

let update_level current_level ty =
  (** Update levels of type variables to be no higher than current_level.
      
      When we unify a type variable at level L1 with a type containing
      variables at level L2 > L1, we must lower L2 to L1. This prevents
      escaping the scope where the type was introduced.
      
      Example of what we're preventing:
      {[
        let f () =
          let r = ref None in           (* 'a ref at level 1 *)
          let g x = r := Some x in      (* x at level 2, try to store in r *)
          (* ERROR: can't let level 2 variable escape into level 1 ref *)
      ]}
      
      @param current_level The maximum level allowed
      @param ty The type expression to update
  *)
  iter_type_expr
    (fun t ->
      let t = follow_links t in
      if t.Types.level > current_level then 
        t.Types.level <- current_level)
    ty

(** {2 Type Creation} *)

let new_type_variable ~ctx level =
  (** Create a fresh type variable at a specific level.
      
      Type variables are the unknowns in type inference. Each variable has:
      - A unique ID (for identification)
      - A level (for let-polymorphism)
      - A description (initially Variable, later may become Link)
      
      Example:
      {[
        let ctx = Types.create_context () in
        let var, ctx = new_type_variable ~ctx 1 in
        (* Now var represents an unknown type at level 1 *)
      ]}
      
      @param ctx The typing context (provides fresh IDs)
      @param level The level for the new variable
      @return A tuple of (new type variable, updated context)
  *)
  let ty, ctx = Types.newvar ~ctx None in
  ty.Types.level <- level;
  (ty, ctx)

let new_generic_type ~ctx desc =
  (** Create a new generic type expression with the given description.
      
      Used when constructing complex types during instantiation.
      Unlike [new_type_variable], this can create any kind of type,
      not just variables.
      
      Example:
      {[
        (* Create an arrow type during instantiation *)
        let arrow, ctx = new_generic_type ~ctx 
          (Types.Arrow (Nolabel, arg_type, ret_type)) in
      ]}
      
      @param ctx The typing context
      @param desc The type description (Variable, Arrow, Tuple, etc.)
      @return A tuple of (new type expression, updated context)
  *)
  let ty, ctx = Types.newty ~ctx desc in
  (ty, ctx)
