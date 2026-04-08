open Std

type label =
  | Nolabel
  | Labelled of string
  | Optional of string

type var = {
  id: int;
  mutable link: t option;
}

and named_type_head = {
  type_constructor_id: TypeConstructorId.t;
  name: IdentPath.t;
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
  | Named of { head: named_type_head; arguments: t list }
  | PolyVariant of { bound: poly_variant_bound; tags: poly_variant_tag list; inherited: t list }
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
}

let max_level = fun xs ->
  List.fold_left
    (fun acc ty ->
      Int.max acc ty.level)
    0
    xs

let poly_variant_max_level = fun tags inherited ->
  let tag_max =
    tags |> List.fold_left
      (fun acc (tag: poly_variant_tag) ->
        match tag.payload_type with
        | Some payload_type -> Int.max acc payload_type.level
        | None -> acc)
      0
  in
  Int.max tag_max (max_level inherited)

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
  }

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

let poly_variant_tag = fun ?payload_type name -> { name; payload_type }

let poly_variant = fun ~bound ~tags ~inherited -> of_desc (PolyVariant { bound; tags; inherited })

let tuple = fun members -> of_desc (Tuple members)

let arrow = fun ~label ~lhs ~rhs -> of_desc (Arrow { label; lhs; rhs })

let hole = fun hole_id -> of_desc (Hole hole_id)

let rec prune = fun ty ->
  match ty.desc with
  | Var ({ link=Some linked; _ } as var) ->
      let linked = prune linked in
      let () =
        var.link <- Some linked
      in
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

let is_generic_level = fun level ->
  Int.equal level generic_level

let make_var = fun ?(level = 0) id -> of_desc ~level (Var { id; link = None })

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
          let () = Collections.HashSet.insert seen value |> ignore in
          value :: acc)
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
  let rec collect seen acc ty =
    let ty = prune ty in
    match ty.desc with
    | Int
    | Float
    | Bool
    | String
    | Char
    | Unit
    | Hole _ ->
        acc
    | Option element ->
        collect seen acc element
    | Result (ok_ty, error_ty) ->
        let acc = collect seen acc ok_ty in
        collect seen acc error_ty
    | Array element ->
        collect seen acc element
    | List element ->
        collect seen acc element
    | Seq element ->
        collect seen acc element
    | Named { arguments; _ } ->
        List.fold_left (fun acc argument -> collect seen acc argument) acc arguments
    | PolyVariant { tags; inherited; _ } ->
        let acc =
          tags |> List.fold_left
            (fun acc (tag: poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type -> collect seen acc payload_type
              | None -> acc)
            acc
        in
        List.fold_left (fun acc inherited_type -> collect seen acc inherited_type) acc inherited
    | Tuple members ->
        List.fold_left (fun acc member -> collect seen acc member) acc members
    | Arrow { lhs; rhs; _ } ->
        let acc = collect seen acc lhs in
        collect seen acc rhs
    | Var { id; link=None; _ } ->
        if Collections.HashSet.contains seen id then
          acc
        else
          let () = Collections.HashSet.insert seen id |> ignore in
          id :: acc
    | Var { link=Some linked; _ } ->
        collect seen acc linked
  in
  fun ty -> collect (Collections.HashSet.create ()) [] ty

let seal_levels =
  let rec loop ty =
    let ty = prune ty in
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
      | Seq element -> loop element
      | Result (ok_ty, error_ty) -> Int.max (loop ok_ty) (loop error_ty)
      | Named { arguments; _ }
      | Tuple arguments ->
          List.fold_left
            (fun acc argument ->
              Int.max acc (loop argument))
            0
            arguments
      | PolyVariant { tags; inherited; _ } ->
          let tag_level =
            tags |> List.fold_left
              (fun acc (tag: poly_variant_tag) ->
                match tag.payload_type with
                | Some payload_type -> Int.max acc (loop payload_type)
                | None -> acc)
              0
          in
          Int.max
            tag_level
            (List.fold_left
              (fun acc inherited_type ->
                Int.max acc (loop inherited_type))
              0
              inherited)
      | Arrow { lhs; rhs; _ } -> Int.max (loop lhs) (loop rhs)
      | Var { link=None; _ } -> ty.level
      | Var { link=Some linked; _ } -> loop linked
    in
    let sealed_level =
      match ty.desc with
      | Var _ -> ty.level
      | _ -> Int.max ty.level child_level
    in
    let () =
      ty.level <- sealed_level
    in
    sealed_level
  in
  fun ty ->
    let _ = loop ty in
    ()

let generalize_ids =
  let rec loop generalized_ids ty =
    let ty = prune ty in
    match ty.desc with
    | Int
    | Float
    | Bool
    | String
    | Char
    | Unit
    | Hole _ ->
        ()
    | Option element ->
        loop generalized_ids element
    | Result (ok_ty, error_ty) ->
        let () = loop generalized_ids ok_ty in
        loop generalized_ids error_ty
    | Array element ->
        loop generalized_ids element
    | List element ->
        loop generalized_ids element
    | Seq element ->
        loop generalized_ids element
    | Named { arguments; _ } ->
        List.iter (loop generalized_ids) arguments
    | PolyVariant { tags; inherited; _ } ->
        let () =
          tags |> List.iter
            (fun (tag: poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type -> loop generalized_ids payload_type
              | None -> ())
        in
        List.iter (loop generalized_ids) inherited
    | Tuple members ->
        List.iter (loop generalized_ids) members
    | Arrow { lhs; rhs; _ } ->
        let () = loop generalized_ids lhs in
        loop generalized_ids rhs
    | Var { id; link=None; _ } ->
        if Collections.HashSet.contains generalized_ids id then
          set_generic_var ty
    | Var { link=Some linked; _ } ->
        loop generalized_ids linked
  in
  fun ids ty ->
    if not (List.is_empty ids) then
      (
        let () = loop (Collections.HashSet.of_list ids) ty in
        seal_levels ty
      )

let generic_var_ids =
  let rec collect seen acc ty =
    let ty = prune ty in
    match ty.desc with
    | Int
    | Float
    | Bool
    | String
    | Char
    | Unit
    | Hole _ ->
        acc
    | Option element ->
        collect seen acc element
    | Result (ok_ty, error_ty) ->
        let acc = collect seen acc ok_ty in
        collect seen acc error_ty
    | Array element ->
        collect seen acc element
    | List element ->
        collect seen acc element
    | Seq element ->
        collect seen acc element
    | Named { arguments; _ } ->
        List.fold_left (fun acc argument -> collect seen acc argument) acc arguments
    | PolyVariant { tags; inherited; _ } ->
        let acc =
          tags |> List.fold_left
            (fun acc (tag: poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type -> collect seen acc payload_type
              | None -> acc)
            acc
        in
        List.fold_left (fun acc inherited_type -> collect seen acc inherited_type) acc inherited
    | Tuple members ->
        List.fold_left (fun acc member -> collect seen acc member) acc members
    | Arrow { lhs; rhs; _ } ->
        let acc = collect seen acc lhs in
        collect seen acc rhs
    | Var { id; link=None; _ } ->
        if is_generic_var ty && not (Collections.HashSet.contains seen id) then
          let () = Collections.HashSet.insert seen id |> ignore in
          id :: acc
        else
          acc
    | Var { link=Some linked; _ } ->
        collect seen acc linked
  in
  fun ty -> collect (Collections.HashSet.create ()) [] ty |> List.rev

let mark_reachable_vars = fun ~generation ~next_order ty ->
  let rec loop = function
    | [] -> ()
    | ty :: rest ->
        let ty = prune ty in
        if Int.equal ty.mark generation then
          loop rest
        else
          let () =
            ty.mark <- generation;
            ty.mark_order <- next_order ()
          in
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
            | Result (ok_ty, error_ty) -> ok_ty :: error_ty :: rest
            | Named { arguments; _ }
            | Tuple arguments -> List.fold_right (fun argument acc -> argument :: acc) arguments rest
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
  in
  loop [ ty ]

let add_variance = fun acc var_id variance ->
  match List.assoc_opt var_id acc with
  | Some existing -> (var_id, join_variance existing variance) :: List.remove_assoc var_id acc
  | None -> (var_id, variance) :: acc

let merge_variances = fun left right ->
  List.fold_left (fun acc (var_id, variance) -> add_variance acc var_id variance) left right

let rec collect_variances = fun variance ty ->
  let ty = prune ty in
  match ty.desc with
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Hole _ -> []
  | Option element -> collect_variances variance element
  | Result (ok_ty, error_ty) -> merge_variances
    (collect_variances variance ok_ty)
    (collect_variances variance error_ty)
  | Array element -> collect_variances Invariant element
  | List element -> collect_variances variance element
  | Seq element -> collect_variances variance element
  | Named { arguments; _ } -> List.fold_left
    (fun acc argument -> merge_variances acc (collect_variances Invariant argument))
    []
    arguments
  | PolyVariant { tags; inherited; _ } ->
      let acc = tags |> List.fold_left
        (fun acc (tag: poly_variant_tag) ->
          match tag.payload_type with
          | Some payload_type -> merge_variances acc (collect_variances variance payload_type)
          | None -> acc)
        []
      in
      List.fold_left
        (fun acc inherited_type -> merge_variances acc (collect_variances variance inherited_type))
        acc
        inherited
  | Tuple members -> List.fold_left
    (fun acc member -> merge_variances acc (collect_variances variance member))
    []
    members
  | Arrow { lhs; rhs; _ } -> merge_variances
    (collect_variances (flip_variance variance) lhs)
    (collect_variances variance rhs)
  | Var { id; link=None; _ } -> [ (id, variance) ]
  | Var { link=Some linked; _ } -> collect_variances variance linked

let covariant_vars = fun ty ->
  collect_variances Covariant ty |> List.filter_map
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
  | Named { arguments; _ } -> List.exists (occurs needle) arguments
  | PolyVariant { tags; inherited; _ } ->
      List.exists
        (fun (tag: poly_variant_tag) ->
          match tag.payload_type with
          | Some payload_type -> occurs needle payload_type
          | None -> false)
        tags
      || List.exists (occurs needle) inherited
  | Tuple members -> List.exists (occurs needle) members
  | Arrow { lhs; rhs; _ } -> occurs needle lhs || occurs needle rhs
  | Var { id; link=None; _ } -> Int.equal id needle
  | Var { link=Some linked; _ } -> occurs needle linked

let occurs_check = fun ~generation ~needle ~minimum_level ty ->
  let rec loop = function
    | [] -> false
    | ty :: rest ->
        let ty = prune ty in
        if Int.equal (mark ty) generation then
          loop rest
        else if ty.level < minimum_level then
          let () = set_mark ty generation in
          loop rest
        else
          let () = set_mark ty generation in
          match ty.desc with
          | Var { id; link=None; _ } when Int.equal id needle -> true
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
          | Result (ok_ty, error_ty) -> loop (ok_ty :: error_ty :: rest)
          | Named { arguments; _ }
          | Tuple arguments -> loop
            (List.fold_right (fun argument acc -> argument :: acc) arguments rest)
          | PolyVariant { tags; inherited; _ } ->
              let rest =
                List.fold_right (fun inherited_type acc -> inherited_type :: acc) inherited rest
              in
                let rest =
                  List.fold_right
                    (fun (tag: poly_variant_tag) acc ->
                      match tag.payload_type with
                      | Some payload_type -> payload_type :: acc
                      | None -> acc)
                    tags
                    rest
                in
              loop rest
          | Arrow { lhs; rhs; _ } -> loop (lhs :: rhs :: rest)
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
          let () = set_mark ty generation in
          loop rest
        else
          let () = set_mark ty generation in
          let () =
            if ty.level > level then
              (
                ty.level <- level;
                on_lower ty
              )
          in
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
            | Result (ok_ty, error_ty) -> ok_ty :: error_ty :: rest
            | Named { arguments; _ }
            | Tuple arguments -> List.fold_right (fun argument acc -> argument :: acc) arguments rest
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
          let () = set_mark ty generation in
          loop rest
        else
          let () = set_mark ty generation in
          match ty.desc with
          | Var { id; link=None; _ } when Int.equal id needle -> true
          | _ ->
              let () =
                if ty.level > level then
                  (
                    ty.level <- level;
                    on_lower ty
                  )
              in
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
                | Result (ok_ty, error_ty) -> ok_ty :: error_ty :: rest
                | Named { arguments; _ }
                | Tuple arguments -> List.fold_right
                  (fun argument acc -> argument :: acc)
                  arguments
                  rest
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
  in
  loop [ ty ]
