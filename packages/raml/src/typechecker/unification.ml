open Std

(** {1 Type Unification}

    Hindley-Milner type unification with occurs check and let-polymorphism.

    This is the heart of type inference! Unification determines if two types can
    be made equal by finding substitutions for type variables.

    {b For beginners:} Think of unification as solving type equations. When we
    see [f 42], we know:
    - [f] has type ['a -> 'b] (unknown function)
    - [42] has type [int]
    - So ['a] must equal [int]

    Unification solves these constraints automatically.

    {b Key concepts:}
    - {b Unification:} Make two types equal by substituting type variables
    - {b Instantiation:} Create a fresh copy of a polymorphic type
    - {b Generalization:} Make a type polymorphic (add forall) *)

(** {2 Unification Errors} *)

type unification_error =
  | TypeMismatch of Types.type_expr * Types.type_expr
      (** Two types that can't be unified. Example: trying to unify [int] with
          [string] *)
  | OccursCheck of Types.type_expr * Types.type_expr
      (** A type variable occurs in a type we're trying to unify it with.
          Example: trying to unify ['a] with ['a -> int] (infinite type!) *)
  | ArityMismatch of { expected : int; got : int; path : ModulePath.t }
      (** Type constructor applied to wrong number of arguments. Example: [list]
          expects 1 argument, but got [list int string] *)

let rec unify ~ctx t1 t2 =
  let t1 = TypeOperations.follow_links t1 in
  let t2 = TypeOperations.follow_links t2 in

  if Ptr.equal t1 t2 then Ok ctx
  else
    match (t1.Types.desc, t2.Types.desc) with
    | Types.Variable _, Types.Variable _ ->
        if t1.Types.level < t2.Types.level then (
          t2.Types.desc <- Types.Link t1;
          Ok ctx)
        else (
          t1.Types.desc <- Types.Link t2;
          Ok ctx)
    | Types.Variable _, _ ->
        if TypeOperations.occurs_in_type t1.Types.id t2 then
          Error (OccursCheck (t1, t2))
        else (
          TypeOperations.update_level t1.Types.level t2;
          t1.Types.desc <- Types.Link t2;
          Ok ctx)
    | _, Types.Variable _ ->
        if TypeOperations.occurs_in_type t2.Types.id t1 then
          Error (OccursCheck (t2, t1))
        else (
          TypeOperations.update_level t2.Types.level t1;
          t2.Types.desc <- Types.Link t1;
          Ok ctx)
    | Types.Arrow (l1, arg1, ret1), Types.Arrow (l2, arg2, ret2) when l1 = l2 ->
        Result.and_then (unify ~ctx arg1 arg2) (fun ctx -> unify ~ctx ret1 ret2)
    | Types.Tuple t1s, Types.Tuple t2s when List.length t1s = List.length t2s ->
        List.fold_left2
          (fun acc t1 t2 -> Result.and_then acc (fun ctx -> unify ~ctx t1 t2))
          (Ok ctx) t1s t2s
    | Types.Constructor (p1, args1), Types.Constructor (p2, args2)
      when ModulePath.same p1 p2 ->
        if List.length args1 != List.length args2 then
          Error
            (ArityMismatch
               {
                 expected = List.length args1;
                 got = List.length args2;
                 path = p1;
               })
        else
          List.fold_left2
            (fun acc t1 t2 -> Result.and_then acc (fun ctx -> unify ~ctx t1 t2))
            (Ok ctx) args1 args2
    | _ -> Error (TypeMismatch (t1, t2))

let instance ~ctx ty =
  let rec inst subst ty =
    let ty = TypeOperations.follow_links ty in
    match ty.Types.desc with
    | Types.Variable _ -> (
        match Collections.HashMap.get subst ty.Types.id with
        | Some t -> t
        | None ->
            let fresh, ctx_unused =
              TypeOperations.new_type_variable ~ctx ty.Types.level
            in
            let _ = Collections.HashMap.insert subst ty.Types.id fresh in
            fresh)
    | Types.Arrow (l, t1, t2) ->
        let t1' = inst subst t1 in
        let t2' = inst subst t2 in
        let ty', _ctx =
          TypeOperations.new_generic_type ~ctx (Types.Arrow (l, t1', t2'))
        in
        ty'
    | Types.Tuple ts ->
        let ts' = List.map (inst subst) ts in
        let ty', _ctx =
          TypeOperations.new_generic_type ~ctx (Types.Tuple ts')
        in
        ty'
    | Types.Constructor (path, args) ->
        let args' = List.map (inst subst) args in
        let ty', _ctx =
          TypeOperations.new_generic_type ~ctx (Types.Constructor (path, args'))
        in
        ty'
    | Types.Link t -> inst subst t
    | Types.Substitution t -> inst subst t
    | Types.UniversalVariable _ -> ty
    | Types.Polymorphic (t, _vars) -> inst subst t
  in
  let subst = Collections.HashMap.create () in
  let ty' = inst subst ty in
  (ty', ctx)

let generalize ~level ty =
  let rec gen ty =
    let ty = TypeOperations.follow_links ty in
    if ty.Types.level > level then ty.Types.desc <- Types.UniversalVariable None
  in
  TypeOperations.iter_type_expr gen ty

let error_to_string = function
  | TypeMismatch (t1, t2) ->
      "Type mismatch: " ^ Types.type_expr_to_string t1 ^ " vs " ^ 
        Types.type_expr_to_string t2
  | OccursCheck (t1, t2) ->
      "Occurs check: " ^ Types.type_expr_to_string t1 ^ " occurs in " ^
        Types.type_expr_to_string t2
  | ArityMismatch { expected; got; path } ->
      "Arity mismatch for " ^ ModulePath.name path ^ ": expected " ^
        Int.to_string expected ^ " args, got " ^ Int.to_string got
