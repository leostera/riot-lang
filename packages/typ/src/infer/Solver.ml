open Std
open Diagnostics
open Model

type frame = Region.frame

type generalization_group = {
  roots: TypeRepr.t list;
  expansive_roots: TypeRepr.t list;
}

type t = {
  regions: Region.t;
  mutable next_type_var_id: int;
}

let create = fun () -> { regions = Region.create (); next_type_var_id = 0 }

let next_mark = fun (solver: t) -> Region.next_mark solver.regions

let track_node = fun (solver: t) node ->
  Region.track_node solver.regions node

let make_type = fun (solver: t) desc -> TypeRepr.of_desc desc |> track_node solver

let fresh_var = fun (solver: t) ->
  let id = solver.next_type_var_id in
  let () =
    solver.next_type_var_id <- id + 1
  in
  TypeRepr.make_var ~level:(Region.current_level solver.regions) id |> track_node solver

let group = fun ?(expansive_roots = []) roots -> { roots; expansive_roots }

let lower_expansive_var = fun (solver: t) (frame: frame) ~variance_of_named ty ->
  let boundary_level = Region.boundary_level frame in
  let generation = Region.mark_roots solver.regions [ ty ] in
  let seen = Collections.HashMap.with_capacity 16 in
  let rec lower = function
    | [] -> ()
    | (variance, ty) :: rest ->
        let ty = TypeRepr.prune ty in
        if not (Int.equal ty.mark generation) then
          lower rest
        else
          let order = ty.mark_order in
          let (should_process, variance) =
            match Collections.HashMap.get seen order with
            | Some seen_variance ->
                let joined = TypeDecl.join_variance seen_variance variance in
                if joined = seen_variance then
                  (false, seen_variance)
                else
                  (
                    let _ = Collections.HashMap.insert seen order joined in
                    (true, joined)
                  )
            | None ->
                let _ = Collections.HashMap.insert seen order variance in
                (true, variance)
          in
          if not should_process || TypeRepr.level ty <= boundary_level then
            lower rest
          else
            let () =
              match variance with
              | TypeDecl.Covariant -> ()
              | TypeDecl.Contravariant
              | TypeDecl.Invariant ->
                  if TypeRepr.level ty > boundary_level then
                    (
                      TypeRepr.set_level ty boundary_level;
                      Region.add_to_pool solver.regions ~level:boundary_level ty |> ignore
                    )
            in
            let rest =
              match TypeRepr.view ty with
              | TypeRepr.Int
              | TypeRepr.Float
              | TypeRepr.Bool
              | TypeRepr.String
              | TypeRepr.Char
              | TypeRepr.Unit
              | TypeRepr.Hole _
              | TypeRepr.Var _ ->
                  rest
              | TypeRepr.Option element
              | TypeRepr.List element
              | TypeRepr.Seq element ->
                  (variance, element) :: rest
              | TypeRepr.Result (ok_ty, error_ty) ->
                  (variance, ok_ty) :: (variance, error_ty) :: rest
              | TypeRepr.Array element ->
                  (TypeDecl.Invariant, element) :: rest
              | TypeRepr.Named { head; arguments } ->
                  let parameter_variances = variance_of_named head arguments in
                  let rec add_arguments acc arguments parameter_variances =
                    match (arguments, parameter_variances) with
                    | (argument :: rest_arguments, parameter_variance :: rest_variances) -> add_arguments
                      ((TypeDecl.compose_variance variance parameter_variance, argument) :: acc)
                      rest_arguments
                      rest_variances
                    | _ -> acc
                  in
                  add_arguments rest arguments parameter_variances
              | TypeRepr.PolyVariant { tags; inherited; _ } ->
                  let rest =
                    List.fold_left
                      (fun acc inherited_type -> (variance, inherited_type) :: acc)
                      rest
                      inherited
                  in
                  tags |> List.fold_left
                    (fun acc (tag: TypeRepr.poly_variant_tag) ->
                      match tag.payload_type with
                      | Some payload_type -> (variance, payload_type) :: acc
                      | None -> acc)
                    rest
              | TypeRepr.Tuple members ->
                  List.fold_left (fun acc member -> (variance, member) :: acc) rest members
              | TypeRepr.Arrow { lhs; rhs; _ } ->
                  (TypeDecl.flip_variance variance, lhs) :: (variance, rhs) :: rest
              | TypeRepr.Package signature ->
                  List.fold_left
                    (fun acc (value: TypeRepr.package_value) -> (variance, value.scheme) :: acc)
                    rest
                    signature.values
            in
            lower rest
  in
  let initial = ref [] in
  let () =
    Region.iter_owned_nodes frame
      (fun node ->
        let node = TypeRepr.prune node in
        if Int.equal node.mark generation then
          initial := (TypeDecl.Covariant, node) :: !initial)
  in
  lower !initial

let finalize_groups = fun (solver: t) (frame: frame) ~variance_of_named groups ->
  groups |> List.map
    (fun (group: generalization_group) ->
      let () = List.iter (lower_expansive_var solver frame ~variance_of_named) group.expansive_roots in
      let () = Region.generalize_reachable_vars solver.regions frame group.roots in
      List.map TypeScheme.of_type group.roots)

let with_local_level_gen = fun (solver: t) ~variance_of_named f ->
  Region.with_region_finalize solver.regions
    ~finalize:(fun frame (result, groups) ->
      let schemes = finalize_groups solver frame ~variance_of_named groups in
      (result, schemes))
    (fun _frame -> f ())

let instantiate = fun (solver: t) scheme ->
  TypeScheme.instantiate
    ~fresh_var:(fun () -> fresh_var solver)
    ~make:(make_type solver)
    ~next_mark:(fun () -> next_mark solver)
    scheme

let labels_match = fun left right ->
  match (left, right) with
  | (TypeRepr.Nolabel, TypeRepr.Nolabel) -> true
  | (TypeRepr.Labelled left, TypeRepr.Labelled right) -> String.equal left right
  | (TypeRepr.Optional left, TypeRepr.Optional right) -> String.equal left right
  | _ -> false

let same_named_head = fun left right ->
  TypeConstructorId.equal left.TypeRepr.type_constructor_id right.TypeRepr.type_constructor_id

let same_poly_variant_bound = fun left right ->
  match (left, right) with
  | (TypeRepr.Exact, TypeRepr.Exact)
  | (TypeRepr.UpperBound, TypeRepr.UpperBound)
  | (TypeRepr.LowerBound, TypeRepr.LowerBound) -> true
  | _ -> false

let mismatch = fun left right ->
  Diagnostic.ExpectedActual {
    expected = TypePrinter.type_to_string left;
    actual = TypePrinter.type_to_string right
  }

let unify = fun (solver: t) ~left ~right ->
  let pair_generation = next_mark solver in
  let next_node_order =
    let order = ref 0 in
    fun () ->
      let current = !order in
      let () =
        order := current + 1
      in
      current
  in
  let node_order ty =
    let ty = TypeRepr.prune ty in
    if Int.equal (TypeRepr.aux_mark ty) pair_generation then
      TypeRepr.aux_order ty
    else
      let order = next_node_order () in
      let () =
        TypeRepr.set_aux_mark ty pair_generation;
        TypeRepr.set_aux_order ty order
      in
      order
  in
  let seen_pairs = Collections.HashSet.with_capacity 128 in
  let mark_pair_seen left right =
    let left_order = node_order left in
    let right_order = node_order right in
    let key =
      if left_order <= right_order then
        (left_order, right_order)
      else
        (right_order, left_order)
    in
    if Collections.HashSet.contains seen_pairs key then
      true
    else
      let () = Collections.HashSet.insert seen_pairs key |> ignore in
      false
  in
  let rec loop = function
    | [] -> Ok ()
    | (left, right) :: rest ->
        let left = TypeRepr.prune left in
        let right = TypeRepr.prune right in
        if Std.Ptr.equal left right || mark_pair_seen left right then
          loop rest
        else
          match (TypeRepr.view left, TypeRepr.view right) with
          | (TypeRepr.Int, TypeRepr.Int)
          | (TypeRepr.Float, TypeRepr.Float)
          | (TypeRepr.Bool, TypeRepr.Bool)
          | (TypeRepr.String, TypeRepr.String)
          | (TypeRepr.Char, TypeRepr.Char)
          | (TypeRepr.Unit, TypeRepr.Unit) ->
              loop rest
          | (TypeRepr.Option left_element, TypeRepr.Option right_element)
          | (TypeRepr.Array left_element, TypeRepr.Array right_element)
          | (TypeRepr.List left_element, TypeRepr.List right_element)
          | (TypeRepr.Seq left_element, TypeRepr.Seq right_element) ->
              loop ((left_element, right_element) :: rest)
          | TypeRepr.Result (left_ok, left_error), TypeRepr.Result (right_ok, right_error) ->
              loop ((left_ok, right_ok) :: (left_error, right_error) :: rest)
          | TypeRepr.Package left_signature, TypeRepr.Package right_signature ->
              let sort_values values =
                values
                |> List.sort
                  (fun (left: TypeRepr.package_value) (right: TypeRepr.package_value) ->
                    String.compare left.name right.name)
              in
              let left_values = sort_values left_signature.values in
              let right_values = sort_values right_signature.values in
              let rec add_value_pairs acc left_values right_values =
                match (left_values, right_values) with
                | ([], []) -> Some acc
                | ((left_value: TypeRepr.package_value) :: left_rest, (
                  right_value: TypeRepr.package_value
                ) :: right_rest) when String.equal left_value.name right_value.name -> add_value_pairs
                  ((left_value.scheme, right_value.scheme) :: acc)
                  left_rest
                  right_rest
                | _ -> None
              in
              (
                match add_value_pairs [] left_values right_values with
                | Some value_pairs -> loop (List.rev_append value_pairs rest)
                | None -> Error (mismatch left right)
              )
          | (TypeRepr.Hole _, _)
          | (_, TypeRepr.Hole _) ->
              loop rest
          | TypeRepr.Named { head=left_head; arguments=left_arguments; _ }, TypeRepr.Named {
            head=right_head;
            arguments=right_arguments;
            _
          } ->
              if not (same_named_head left_head right_head) then
                Error (mismatch left right)
              else if List.length left_arguments != List.length right_arguments then
                Error (mismatch left right)
              else
                loop (List.rev_append (List.combine left_arguments right_arguments) rest)
          | TypeRepr.PolyVariant { bound=left_bound; tags=left_tags; inherited=left_inherited }, TypeRepr.PolyVariant {
            bound=right_bound;
            tags=right_tags;
            inherited=right_inherited
          } ->
              if
                not (same_poly_variant_bound left_bound right_bound)
                || List.length left_inherited != List.length right_inherited
              then
                Error (mismatch left right)
              else
                let sort_tags tags =
                  tags
                  |> List.sort
                    (fun (left: TypeRepr.poly_variant_tag) (right: TypeRepr.poly_variant_tag) ->
                      String.compare left.name right.name)
                in
                let left_tags = sort_tags left_tags in
                let right_tags = sort_tags right_tags in
                let rec add_tag_pairs acc left_tags right_tags =
                  match (left_tags, right_tags) with
                  | ([], []) ->
                      Some acc
                  | ((left_tag: TypeRepr.poly_variant_tag) :: rest_left, (
                    right_tag: TypeRepr.poly_variant_tag
                  ) :: rest_right) when String.equal left_tag.name right_tag.name -> (
                      match (left_tag.payload_type, right_tag.payload_type) with
                      | (None, None) -> add_tag_pairs acc rest_left rest_right
                      | (Some left_payload, Some right_payload) -> add_tag_pairs
                        ((left_payload, right_payload) :: acc)
                        rest_left
                        rest_right
                      | _ -> None
                    )
                  | _ ->
                      None
                in
                (
                  match add_tag_pairs [] left_tags right_tags with
                  | None -> Error (mismatch left right)
                  | Some tag_pairs -> loop
                    (List.rev_append
                      tag_pairs
                      (List.rev_append (List.combine left_inherited right_inherited) rest))
                )
          | TypeRepr.Tuple left_members, TypeRepr.Tuple right_members ->
              if List.length left_members != List.length right_members then
                Error (Diagnostic.TupleArityMismatch {
                  left = TypePrinter.type_to_string left;
                  right = TypePrinter.type_to_string right;
                  left_arity = List.length left_members;
                  right_arity = List.length right_members
                })
              else
                loop (List.rev_append (List.combine left_members right_members) rest)
          | TypeRepr.Arrow { label=left_label; lhs=left_arg; rhs=left_res }, TypeRepr.Arrow {
            label=right_label;
            lhs=right_arg;
            rhs=right_res
          } ->
              if not (labels_match left_label right_label) then
                Error (mismatch left right)
              else
                loop ((left_arg, right_arg) :: (left_res, right_res) :: rest)
          | TypeRepr.Var left_var, TypeRepr.Var right_var when left_var.id = right_var.id ->
              loop rest
          | (TypeRepr.Var var, _)
          | (_, TypeRepr.Var var) ->
              let (var_ty, other_ty) =
                match TypeRepr.view left with
                | TypeRepr.Var _ -> (left, right)
                | _ -> (right, left)
              in
              let level = TypeRepr.level var_ty in
              let lowered =
                TypeRepr.occurs_or_lower
                  ~generation:(next_mark solver)
                  ~needle:var.id
                  ~level
                  ~on_lower:(fun ty ->
                    Region.add_to_pool solver.regions ~level:(TypeRepr.level ty) ty |> ignore)
                  other_ty
              in
              if lowered then
                Error (Diagnostic.OccursCheckFailed {
                  variable_id = var.id;
                  in_type = TypePrinter.type_to_string other_ty
                })
              else (
                var.link <- Some other_ty;
                loop rest
              )
          | _ ->
              Error (mismatch left right)
  in
  loop [ (left, right) ]
