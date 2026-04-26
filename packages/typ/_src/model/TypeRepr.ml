open Std

type scheme = {
  quantified: int list;
  body: t;
}

and label =
  | Nolabel
  | Labelled of string
  | Optional of string

and var_kind =
  | Flexible
  | Rigid

and var = {
  id: int;
  kind: var_kind;
  mutable link: t option;
}

and named_type_head = {
  type_constructor_id: TypeConstructorId.t;
  name: SurfacePath.t;
}

and package_value = { name: string; scheme: scheme }

and package_signature = {
  values: package_value list;
}

and poly_variant_bound =
  | Exact
  | UpperBound
  | LowerBound

and poly_variant_tag = {
  name: string;
  payload_type: t option;
}

and desc =
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Option of t
  | Result of t * t
  | Array of t
  | List of t
  | Seq of t
  | Named of {
      head: named_type_head;
      arguments: t list;
    }
  | Package of package_signature
  | PolyVariant of {
      bound: poly_variant_bound;
      tags: poly_variant_tag list;
      inherited: t list;
    }
  | Tuple of t list
  | Arrow of { label: label; lhs: t; rhs: t }
  | Var of var
  | Hole of int

and t = {
  mutable desc: desc;
  mutable level: int;
  mutable pool_level: int option;
  mutable mark: int;
  mutable mark_order: int;
  mutable aux_mark: int;
  mutable aux_order: int;
  mutable walk_mark: int;
}

let max_level = fun xs -> List.fold_left (fun acc ty -> Int.max acc ty.level) 0 xs

let poly_variant_max_level = fun tags inherited ->
  let tag_max =
    tags
    |> List.fold_left
      (fun acc (tag: poly_variant_tag) ->
        match tag.payload_type with
        | Some payload_type -> Int.max acc payload_type.level
        | None -> acc)
      0
  in
  Int.max tag_max (max_level inherited)

let package_signature_max_level = fun (signature: package_signature) ->
  signature.values
  |> List.fold_left (fun acc (value: package_value) -> Int.max acc value.scheme.body.level) 0

let level_of_desc = function
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Hole _ -> 0
  | Option element
  | Array element
  | List element
  | Seq element -> element.level
  | Result (ok_ty, error_ty) -> Int.max ok_ty.level error_ty.level
  | Named { arguments; _ }
  | Tuple arguments -> max_level arguments
  | Package signature -> package_signature_max_level signature
  | PolyVariant { tags; inherited; _ } -> poly_variant_max_level tags inherited
  | Arrow { lhs; rhs; _ } -> Int.max lhs.level rhs.level
  | Var _ -> 0

let of_desc = fun ?level desc ->
  let level =
    match level with
    | Some level -> level
    | None -> level_of_desc desc
  in
  {
    desc;
    level;
    pool_level = None;
    mark = (-1);
    mark_order = (-1);
    aux_mark = (-1);
    aux_order = (-1);
    walk_mark = (-1);
  }

let shell = fun ?level () -> of_desc ?level (Hole (-1))

let int = of_desc Int

let float = of_desc Float

let bool = of_desc Bool

let string = of_desc String

let char = of_desc Char

let unit_ = of_desc Unit

let option = fun element -> of_desc (Option element)

let result = fun ok_ty error_ty -> of_desc (Result (ok_ty, error_ty))

let array = fun element -> of_desc (Array element)

let list = fun element -> of_desc (List element)

let seq = fun element -> of_desc (Seq element)

let named_head = fun ~type_constructor_id ~name -> { type_constructor_id; name }

let named = fun ~head ~arguments -> of_desc (Named { head; arguments })

let named_path = fun ~name ~arguments ->
  let head = named_head ~type_constructor_id:(TypeConstructorId.of_path name) ~name in
  named ~head ~arguments

let make_scheme = fun ~quantified ~body -> { quantified; body }

let scheme_quantified = fun scheme -> scheme.quantified

let scheme_body = fun scheme -> scheme.body

let package_value = fun ~name ~scheme -> { name; scheme }

let package = fun ~values -> of_desc (Package { values })

let poly_variant_tag = fun ?payload_type name -> { name; payload_type }

let poly_variant = fun ~bound ~tags ~inherited -> of_desc (PolyVariant { bound; tags; inherited })

let tuple = fun members -> of_desc (Tuple members)

let arrow = fun ~label ~lhs ~rhs -> of_desc (Arrow { label; lhs; rhs })

let hole = fun hole_id -> of_desc (Hole hole_id)

let rec prune = fun ty ->
  match ty.desc with
  | Var ({ link = Some linked; _ } as var) ->
      let linked = prune linked in
      var.link <- Some linked;
      linked
  | _ -> ty

let view = fun ty -> ty.desc

let level = fun ty -> ty.level

let set_level = fun ty level -> ty.level <- level

let pool_level = fun ty -> ty.pool_level

let set_pool_level = fun ty pool_level -> ty.pool_level <- pool_level

let mark = fun ty -> ty.mark

let set_mark = fun ty mark -> ty.mark <- mark

let mark_order = fun ty -> ty.mark_order

let set_mark_order = fun ty mark_order -> ty.mark_order <- mark_order

let aux_mark = fun ty -> ty.aux_mark

let set_aux_mark = fun ty aux_mark -> ty.aux_mark <- aux_mark

let aux_order = fun ty -> ty.aux_order

let set_aux_order = fun ty aux_order -> ty.aux_order <- aux_order

let generic_level = Int.max_int

let is_generic_level = fun level -> Int.equal level generic_level

let next_walk_generation =
  let generation = ref 0 in
  fun () ->
    let current = !generation in
    generation := current + 1;
    current

let make_var = fun ?(level = 0) id -> of_desc ~level (Var { id; kind = Flexible; link = None })

let make_rigid_var = fun ?(level = 0) id -> of_desc ~level (Var { id; kind = Rigid; link = None })

let is_rigid_var = fun ty ->
  match view (prune ty) with
  | Var { kind = Rigid; _ } -> true
  | _ -> false

let is_generic_var = fun ty ->
  let ty = prune ty in
  match ty.desc with
  | Var _ -> is_generic_level ty.level
  | _ -> false

let set_generic_var = fun ty ->
  let ty = prune ty in
  match ty.desc with
  | Var _ -> ty.level <- generic_level
  | _ -> ()

let union = fun left right ->
  if List.is_empty right then
    left
  else
    let seen = Collections.HashSet.of_list left in
    List.fold_left
      (fun acc value ->
        if Collections.HashSet.contains seen value then
          acc
        else
          (
            Collections.HashSet.insert seen value
            |> ignore;
            value :: acc
          ))
      left
      right

let diff = fun left right ->
  if List.is_empty left || List.is_empty right then
    left
  else
    let right_values = Collections.HashSet.of_list right in
    List.filter (fun value -> not (Collections.HashSet.contains right_values value)) left

type variance =
  | Covariant
  | Contravariant
  | Invariant

let flip_variance = function
  | Covariant -> Contravariant
  | Contravariant -> Covariant
  | Invariant -> Invariant

let join_variance = fun left right ->
  match (left, right) with
  | (Invariant, _)
  | (_, Invariant) -> Invariant
  | (Covariant, Covariant) -> Covariant
  | (Contravariant, Contravariant) -> Contravariant
  | (Covariant, Contravariant)
  | (Contravariant, Covariant) -> Invariant

let free_vars =
  let rec collect generation seen acc ty =
    let ty = prune ty in
    if Int.equal ty.walk_mark generation then
      acc
    else
      (
        ty.walk_mark <- generation;
        match ty.desc with
        | Int
        | Float
        | Bool
        | String
        | Char
        | Unit
        | Hole _ -> acc
        | Option element -> collect generation seen acc element
        | Result (ok_ty, error_ty) ->
            let acc = collect generation seen acc ok_ty in
            collect generation seen acc error_ty
        | Array element -> collect generation seen acc element
        | List element -> collect generation seen acc element
        | Seq element -> collect generation seen acc element
        | Package signature ->
            List.fold_left
              (fun acc (value: package_value) ->
                collect generation seen acc value.scheme.body)
              acc
              signature.values
        | Named { arguments; _ } ->
            List.fold_left (fun acc argument -> collect generation seen acc argument) acc arguments
        | PolyVariant { tags; inherited; _ } ->
            let acc =
              tags
              |> List.fold_left
                (fun acc (tag: poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type -> collect generation seen acc payload_type
                  | None -> acc)
                acc
            in
            List.fold_left
              (fun acc inherited_type ->
                collect generation seen acc inherited_type)
              acc
              inherited
        | Tuple members ->
            List.fold_left (fun acc member -> collect generation seen acc member) acc members
        | Arrow { lhs; rhs; _ } ->
            let acc = collect generation seen acc lhs in
            collect generation seen acc rhs
        | Var { id; link = None; _ } ->
            if Collections.HashSet.contains seen id then
              acc
            else
              (
                Collections.HashSet.insert seen id
                |> ignore;
                id :: acc
              )
        | Var { link = Some linked; _ } -> collect generation seen acc linked
      )
  in
  fun ty -> collect (next_walk_generation ()) (Collections.HashSet.create ()) [] ty

let seal_levels =
  let rec loop generation ty =
    let ty = prune ty in
    if Int.equal ty.walk_mark generation then
      ty.level
    else
      (
        ty.walk_mark <- generation;
        let child_level =
          match ty.desc with
          | Int
          | Float
          | Bool
          | String
          | Char
          | Unit
          | Hole _ -> 0
          | Option element
          | Array element
          | List element
          | Seq element -> loop generation element
          | Result (ok_ty, error_ty) -> Int.max (loop generation ok_ty) (loop generation error_ty)
          | Named { arguments; _ }
          | Tuple arguments ->
              List.fold_left
                (fun acc argument -> Int.max acc (loop generation argument))
                0
                arguments
          | Package signature ->
              signature.values
              |> List.fold_left
                (fun acc (value: package_value) -> Int.max acc (loop generation value.scheme.body))
                0
          | PolyVariant { tags; inherited; _ } ->
              let tag_level =
                tags
                |> List.fold_left
                  (fun acc (tag: poly_variant_tag) ->
                    match tag.payload_type with
                    | Some payload_type -> Int.max acc (loop generation payload_type)
                    | None -> acc)
                  0
              in
              Int.max
                tag_level
                (List.fold_left
                  (fun acc inherited_type -> Int.max acc (loop generation inherited_type))
                  0
                  inherited)
          | Arrow { lhs; rhs; _ } -> Int.max (loop generation lhs) (loop generation rhs)
          | Var { link = None; _ } -> ty.level
          | Var { link = Some linked; _ } -> loop generation linked
        in
        let sealed_level =
          match ty.desc with
          | Var _ -> ty.level
          | _ -> Int.max ty.level child_level
        in
        ty.level <- sealed_level;
        sealed_level
      )
  in
  fun ty ->
    let _ = loop (next_walk_generation ()) ty in
    ()

let generalize_ids =
  let rec loop generation generalized_ids ty =
    let ty = prune ty in
    if not (Int.equal ty.walk_mark generation) then
      (
        ty.walk_mark <- generation;
        match ty.desc with
        | Int
        | Float
        | Bool
        | String
        | Char
        | Unit
        | Hole _ -> ()
        | Option element -> loop generation generalized_ids element
        | Result (ok_ty, error_ty) ->
            loop generation generalized_ids ok_ty;
            loop generation generalized_ids error_ty
        | Array element -> loop generation generalized_ids element
        | List element -> loop generation generalized_ids element
        | Seq element -> loop generation generalized_ids element
        | Package signature ->
            List.iter
              (fun (value: package_value) ->
                loop generation generalized_ids value.scheme.body)
              signature.values
        | Named { arguments; _ } -> List.iter (loop generation generalized_ids) arguments
        | PolyVariant { tags; inherited; _ } ->
            tags
            |> List.iter
              (fun (tag: poly_variant_tag) ->
                match tag.payload_type with
                | Some payload_type -> loop generation generalized_ids payload_type
                | None -> ());
            List.iter (loop generation generalized_ids) inherited
        | Tuple members -> List.iter (loop generation generalized_ids) members
        | Arrow { lhs; rhs; _ } ->
            loop generation generalized_ids lhs;
            loop generation generalized_ids rhs
        | Var { id; link = None; _ } ->
            if Collections.HashSet.contains generalized_ids id then
              set_generic_var ty
        | Var { link = Some linked; _ } -> loop generation generalized_ids linked
      )
  in
  fun ids ty ->
    if not (List.is_empty ids) then
      (
        loop (next_walk_generation ()) (Collections.HashSet.of_list ids) ty;
        seal_levels ty
      )

let generic_var_ids =
  let rec collect generation seen acc ty =
    let ty = prune ty in
    if Int.equal ty.walk_mark generation then
      acc
    else
      (
        ty.walk_mark <- generation;
        match ty.desc with
        | Int
        | Float
        | Bool
        | String
        | Char
        | Unit
        | Hole _ -> acc
        | Option element -> collect generation seen acc element
        | Result (ok_ty, error_ty) ->
            let acc = collect generation seen acc ok_ty in
            collect generation seen acc error_ty
        | Array element -> collect generation seen acc element
        | List element -> collect generation seen acc element
        | Seq element -> collect generation seen acc element
        | Package signature ->
            List.fold_left
              (fun acc (value: package_value) ->
                collect generation seen acc value.scheme.body)
              acc
              signature.values
        | Named { arguments; _ } ->
            List.fold_left (fun acc argument -> collect generation seen acc argument) acc arguments
        | PolyVariant { tags; inherited; _ } ->
            let acc =
              tags
              |> List.fold_left
                (fun acc (tag: poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type -> collect generation seen acc payload_type
                  | None -> acc)
                acc
            in
            List.fold_left
              (fun acc inherited_type ->
                collect generation seen acc inherited_type)
              acc
              inherited
        | Tuple members ->
            List.fold_left (fun acc member -> collect generation seen acc member) acc members
        | Arrow { lhs; rhs; _ } ->
            let acc = collect generation seen acc lhs in
            collect generation seen acc rhs
        | Var { id; link = None; _ } ->
            if is_generic_var ty && not (Collections.HashSet.contains seen id) then
              (
                Collections.HashSet.insert seen id
                |> ignore;
                id :: acc
              )
            else
              acc
        | Var { link = Some linked; _ } -> collect generation seen acc linked
      )
  in
  fun ty ->
    collect (next_walk_generation ()) (Collections.HashSet.create ()) [] ty
    |> List.rev

let mark_reachable_vars = fun ~generation ~next_order ty ->
  let rec loop = function
    | [] -> ()
    | ty :: rest ->
        let ty = prune ty in
        if Int.equal ty.mark generation then
          loop rest
        else
          (
            ty.mark <- generation;
            ty.mark_order <- next_order ();
            let rest =
              match ty.desc with
              | Int
              | Float
              | Bool
              | String
              | Char
              | Unit
              | Hole _
              | Var _ -> rest
              | Option element
              | Array element
              | List element
              | Seq element -> element :: rest
              | Package signature ->
                  List.fold_right
                    (fun (value: package_value) acc -> value.scheme.body :: acc)
                    signature.values
                    rest
              | Result (ok_ty, error_ty) -> ok_ty :: error_ty :: rest
              | Named { arguments; _ }
              | Tuple arguments ->
                  List.fold_right (fun argument acc -> argument :: acc) arguments rest
              | PolyVariant { tags; inherited; _ } ->
                  let rest =
                    List.fold_right (fun inherited_type acc -> inherited_type :: acc) inherited rest
                  in
                  List.fold_right
                    (fun (tag: poly_variant_tag) acc ->
                      match tag.payload_type with
                      | Some payload_type -> payload_type :: acc
                      | None -> acc)
                    tags
                    rest
              | Arrow { lhs; rhs; _ } -> lhs :: rhs :: rest
            in
            loop rest
          )
  in
  loop [ ty ]

let add_variance = fun acc var_id variance ->
  match List.assoc_opt var_id acc with
  | Some existing -> (var_id, join_variance existing variance) :: List.remove_assoc var_id acc
  | None -> (var_id, variance) :: acc

let merge_variances = fun left right ->
  List.fold_left
    (fun acc (var_id, variance) ->
      add_variance acc var_id variance)
    left
    right

let collect_variances = fun variance ty ->
  let generation = next_walk_generation () in
  let rec loop variance ty =
    let ty = prune ty in
    if Int.equal ty.walk_mark generation then
      []
    else
      (
        ty.walk_mark <- generation;
        match ty.desc with
        | Int
        | Float
        | Bool
        | String
        | Char
        | Unit
        | Hole _ -> []
        | Option element -> loop variance element
        | Result (ok_ty, error_ty) -> merge_variances (loop variance ok_ty) (loop variance error_ty)
        | Array element -> loop Invariant element
        | List element -> loop variance element
        | Seq element -> loop variance element
        | Package signature ->
            List.fold_left
              (fun acc (value: package_value) ->
                merge_variances
                  acc
                  (loop variance value.scheme.body))
              []
              signature.values
        | Named { arguments; _ } ->
            List.fold_left
              (fun acc argument -> merge_variances acc (loop Invariant argument))
              []
              arguments
        | PolyVariant { tags; inherited; _ } ->
            let acc =
              tags
              |> List.fold_left
                (fun acc (tag: poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type -> merge_variances acc (loop variance payload_type)
                  | None -> acc)
                []
            in
            List.fold_left
              (fun acc inherited_type -> merge_variances acc (loop variance inherited_type))
              acc
              inherited
        | Tuple members ->
            List.fold_left (fun acc member -> merge_variances acc (loop variance member)) [] members
        | Arrow { lhs; rhs; _ } ->
            merge_variances (loop (flip_variance variance) lhs) (loop variance rhs)
        | Var { id; link = None; _ } -> [ (id, variance); ]
        | Var { link = Some linked; _ } -> loop variance linked
      )
  in
  loop variance ty

let covariant_vars = fun ty ->
  collect_variances Covariant ty
  |> List.filter_map
    (fun (var_id, variance) ->
      match variance with
      | Covariant -> Some var_id
      | Contravariant
      | Invariant -> None)

let rec occurs = fun needle ty ->
  let ty = prune ty in
  match ty.desc with
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Hole _ -> false
  | Option element -> occurs needle element
  | Result (ok_ty, error_ty) -> occurs needle ok_ty || occurs needle error_ty
  | Array element -> occurs needle element
  | List element -> occurs needle element
  | Seq element -> occurs needle element
  | Package signature ->
      List.exists (fun (value: package_value) -> occurs needle value.scheme.body) signature.values
  | Named { arguments; _ } -> List.exists (occurs needle) arguments
  | PolyVariant _ -> false
  | Tuple members -> List.exists (occurs needle) members
  | Arrow { lhs; rhs; _ } -> occurs needle lhs || occurs needle rhs
  | Var { id; link = None; _ } -> Int.equal id needle
  | Var { link = Some linked; _ } -> occurs needle linked

let occurs_check = fun ~generation ~needle ~minimum_level ty ->
  let rec loop = function
    | [] -> false
    | ty :: rest ->
        let ty = prune ty in
        if Int.equal (mark ty) generation then
          loop rest
        else if ty.level < minimum_level then
          (
            set_mark ty generation;
            loop rest
          )
        else
          (
            set_mark ty generation;
            match ty.desc with
            | Var { id; link = None; _ } when Int.equal id needle -> true
            | Int
            | Float
            | Bool
            | String
            | Char
            | Unit
            | Hole _
            | Var _ -> loop rest
            | Option element
            | Array element
            | List element
            | Seq element -> loop (element :: rest)
            | Package signature ->
                loop
                  (List.fold_right
                    (fun (value: package_value) acc -> value.scheme.body :: acc)
                    signature.values
                    rest)
            | Result (ok_ty, error_ty) -> loop (ok_ty :: error_ty :: rest)
            | Named { arguments; _ }
            | Tuple arguments ->
                loop (List.fold_right (fun argument acc -> argument :: acc) arguments rest)
            | PolyVariant _ -> loop rest
            | Arrow { lhs; rhs; _ } -> loop (lhs :: rhs :: rest)
          )
  in
  loop [ ty ]

let lower_level = fun ~generation ~level ~on_lower ty ->
  let rec loop = function
    | [] -> ()
    | ty :: rest ->
        let ty = prune ty in
        if Int.equal (mark ty) generation then
          loop rest
        else if ty.level < level then
          (
            set_mark ty generation;
            loop rest
          )
        else
          (
            set_mark ty generation;
            if ty.level > level then
              (
                ty.level <- level;
                on_lower ty
              );
            let rest =
              match ty.desc with
              | Int
              | Float
              | Bool
              | String
              | Char
              | Unit
              | Hole _
              | Var _ -> rest
              | Option element
              | Array element
              | List element
              | Seq element -> element :: rest
              | Package signature ->
                  List.fold_right
                    (fun (value: package_value) acc -> value.scheme.body :: acc)
                    signature.values
                    rest
              | Result (ok_ty, error_ty) -> ok_ty :: error_ty :: rest
              | Named { arguments; _ }
              | Tuple arguments ->
                  List.fold_right (fun argument acc -> argument :: acc) arguments rest
              | PolyVariant { tags; inherited; _ } ->
                  let rest =
                    List.fold_right (fun inherited_type acc -> inherited_type :: acc) inherited rest
                  in
                  List.fold_right
                    (fun (tag: poly_variant_tag) acc ->
                      match tag.payload_type with
                      | Some payload_type -> payload_type :: acc
                      | None -> acc)
                    tags
                    rest
              | Arrow { lhs; rhs; _ } -> lhs :: rhs :: rest
            in
            loop rest
          )
  in
  loop [ ty ]

let occurs_or_lower = fun ~generation ~needle ~level ~on_lower ty ->
  let rec loop = function
    | [] -> false
    | ty :: rest ->
        let ty = prune ty in
        if Int.equal (mark ty) generation then
          loop rest
        else if ty.level < level then
          (
            set_mark ty generation;
            loop rest
          )
        else
          (
            set_mark ty generation;
            match ty.desc with
            | Var { id; link = None; _ } when Int.equal id needle -> true
            | _ ->
                if ty.level > level then
                  (
                    ty.level <- level;
                    on_lower ty
                  );
                let rest =
                  match ty.desc with
                  | Int
                  | Float
                  | Bool
                  | String
                  | Char
                  | Unit
                  | Hole _
                  | Var _ -> rest
                  | Option element
                  | Array element
                  | List element
                  | Seq element -> element :: rest
                  | Package signature ->
                      List.fold_right
                        (fun (value: package_value) acc -> value.scheme.body :: acc)
                        signature.values
                        rest
                  | Result (ok_ty, error_ty) -> ok_ty :: error_ty :: rest
                  | Named { arguments; _ }
                  | Tuple arguments ->
                      List.fold_right (fun argument acc -> argument :: acc) arguments rest
                  | PolyVariant _ -> rest
                  | Arrow { lhs; rhs; _ } -> lhs :: rhs :: rest
                in
                loop rest
          )
  in
  loop [ ty ]
