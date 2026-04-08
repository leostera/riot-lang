open Std

type t = TypeRepr.t

let of_type = fun ty ->
  let () = TypeRepr.seal_levels ty in
  ty

let of_explicit = fun ~quantified body ->
  let () = TypeRepr.generalize_ids quantified body in
  let () = TypeRepr.seal_levels body in
  body

let body = fun scheme -> scheme

let to_explicit = fun scheme -> (TypeRepr.generic_var_ids scheme, scheme)

let instantiate = fun ~fresh_var ~make ~next_mark scheme ->
  let generation = next_mark () in
  let next_order =
    let order = ref 0 in
    fun () ->
      let current = !order in
      let () =
        order := current + 1
      in
      current
  in
  let replacements = Collections.HashMap.with_capacity 16 in
  let order_for_ty ty =
    if Int.equal (TypeRepr.aux_mark ty) generation then
      TypeRepr.aux_order ty
    else
      let order = next_order () in
      let () =
        TypeRepr.set_aux_mark ty generation;
        TypeRepr.set_aux_order ty order
      in
      order
  in
  let lookup_replacement ty =
    if Int.equal (TypeRepr.aux_mark ty) generation then
      Collections.HashMap.get replacements (TypeRepr.aux_order ty)
    else
      None
  in
  let remember_replacement ty replacement =
    let _ = Collections.HashMap.insert replacements (order_for_ty ty) replacement in
    replacement
  in
  let remember_identity ty =
    let _ = Collections.HashMap.insert replacements (order_for_ty ty) ty in
    ty
  in
  let rec map_preserving loop xs =
    let rec walk changed acc = function
      | [] ->
          if changed then
            List.rev acc
          else
            xs
      | x :: rest ->
          let x' = loop x in
          walk (changed || not (Std.Ptr.equal x x')) (x' :: acc) rest
    in
    walk false [] xs
  in
  let generic_replacements = Collections.HashMap.with_capacity 16 in
  let rec copy ty =
    let ty = TypeRepr.prune ty in
    if TypeRepr.level ty < TypeRepr.generic_level then
      ty
    else
      match lookup_replacement ty with
      | Some replacement -> replacement
      | None -> copy_open ty
  and copy_open ty =
    match TypeRepr.view ty with
    | (TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _) ->
        remember_identity ty
    | TypeRepr.Option element ->
        let element' = copy element in
        if Std.Ptr.equal element element' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.Option element'))
    | TypeRepr.Result (ok_ty, error_ty) ->
        let ok_ty' = copy ok_ty in
        let error_ty' = copy error_ty in
        if Std.Ptr.equal ok_ty ok_ty' && Std.Ptr.equal error_ty error_ty' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.Result (ok_ty', error_ty')))
    | TypeRepr.Array element ->
        let element' = copy element in
        if Std.Ptr.equal element element' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.Array element'))
    | TypeRepr.List element ->
        let element' = copy element in
        if Std.Ptr.equal element element' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.List element'))
    | TypeRepr.Seq element ->
        let element' = copy element in
        if Std.Ptr.equal element element' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.Seq element'))
    | TypeRepr.Named { head; arguments } ->
        let arguments' = map_preserving copy arguments in
        if Std.Ptr.equal arguments arguments' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.Named { head; arguments = arguments' }))
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let tags' = map_preserving
          (fun (tag: TypeRepr.poly_variant_tag) ->
            match tag.payload_type with
            | Some payload_type ->
                let payload_type' = copy payload_type in
                if Std.Ptr.equal payload_type payload_type' then
                  tag
                else
                  { tag with payload_type = Some payload_type' }
            | None -> tag)
          tags in
        let inherited' = map_preserving copy inherited in
        if Std.Ptr.equal tags tags' && Std.Ptr.equal inherited inherited' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.PolyVariant { bound; tags = tags'; inherited = inherited' }))
    | TypeRepr.Tuple members ->
        let members' = map_preserving copy members in
        if Std.Ptr.equal members members' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.Tuple members'))
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let lhs' = copy lhs in
        let rhs' = copy rhs in
        if Std.Ptr.equal lhs lhs' && Std.Ptr.equal rhs rhs' then
          remember_identity ty
        else
          remember_replacement ty (make (TypeRepr.Arrow { label; lhs = lhs'; rhs = rhs' }))
    | TypeRepr.Var { id; link=None; _ } ->
        if TypeRepr.is_generic_var ty then
          match Collections.HashMap.get generic_replacements id with
          | Some replacement ->
              remember_replacement ty replacement
          | None ->
              let replacement = fresh_var () in
              let _ = Collections.HashMap.insert generic_replacements id replacement in
              remember_replacement ty replacement
        else
          remember_identity ty
    | TypeRepr.Var { link=Some linked; _ } ->
        let replacement = copy linked in
        remember_replacement ty replacement
  in
  copy scheme

let next_copy_generation =
  let generation = ref 0 in
  fun () ->
    let current = !generation in
    let () =
      generation := current + 1
    in
    current

let copy = fun scheme ->
  let quantified, body = to_explicit scheme in
  let generation = next_copy_generation () in
  let next_order =
    let order = ref 0 in
    fun () ->
      let current = !order in
      let () =
        order := current + 1
      in
      current
  in
  let replacements = Collections.HashMap.with_capacity 16 in
  let var_replacements = Collections.HashMap.with_capacity 16 in
  let order_for_ty ty =
    if Int.equal (TypeRepr.mark ty) generation then
      TypeRepr.mark_order ty
    else
      let order = next_order () in
      let () =
        TypeRepr.set_mark ty generation;
        TypeRepr.set_mark_order ty order
      in
      order
  in
  let lookup_replacement ty =
    if Int.equal (TypeRepr.mark ty) generation then
      Collections.HashMap.get replacements (TypeRepr.mark_order ty)
    else
      None
  in
  let remember ty replacement =
    let _ = Collections.HashMap.insert replacements (order_for_ty ty) replacement in
    replacement
  in
  let rec map_preserving loop xs =
    let rec walk changed acc = function
      | [] ->
          if changed then
            List.rev acc
          else
            xs
      | x :: rest ->
          let x' = loop x in
          walk (changed || not (Std.Ptr.equal x x')) (x' :: acc) rest
    in
    walk false [] xs
  in
  let rec clone ty =
    let ty = TypeRepr.prune ty in
    match lookup_replacement ty with
    | Some replacement -> replacement
    | None ->
        let level = TypeRepr.level ty in
        let replacement =
          match TypeRepr.view ty with
          | TypeRepr.Int -> TypeRepr.int
          | TypeRepr.Float -> TypeRepr.float
          | TypeRepr.Bool -> TypeRepr.bool
          | TypeRepr.String -> TypeRepr.string
          | TypeRepr.Char -> TypeRepr.char
          | TypeRepr.Unit -> TypeRepr.unit_
          | TypeRepr.Option element ->
              TypeRepr.of_desc ~level (TypeRepr.Option (clone element))
          | TypeRepr.Result (ok_ty, error_ty) ->
              TypeRepr.of_desc ~level (TypeRepr.Result (clone ok_ty, clone error_ty))
          | TypeRepr.Array element ->
              TypeRepr.of_desc ~level (TypeRepr.Array (clone element))
          | TypeRepr.List element ->
              TypeRepr.of_desc ~level (TypeRepr.List (clone element))
          | TypeRepr.Seq element ->
              TypeRepr.of_desc ~level (TypeRepr.Seq (clone element))
          | TypeRepr.Named { head; arguments } ->
              TypeRepr.of_desc ~level (TypeRepr.Named { head; arguments = map_preserving clone arguments })
          | TypeRepr.PolyVariant { bound; tags; inherited } ->
              let tags =
                tags
                |> map_preserving
                  (fun (tag: TypeRepr.poly_variant_tag) ->
                    match tag.payload_type with
                    | Some payload_type ->
                        let payload_type' = clone payload_type in
                        if Std.Ptr.equal payload_type payload_type' then
                          tag
                        else
                          { tag with payload_type = Some payload_type' }
                    | None -> tag)
              in
              let inherited = map_preserving clone inherited in
              TypeRepr.of_desc ~level (TypeRepr.PolyVariant { bound; tags; inherited })
          | TypeRepr.Tuple members ->
              TypeRepr.of_desc ~level (TypeRepr.Tuple (map_preserving clone members))
          | TypeRepr.Arrow { label; lhs; rhs } ->
              TypeRepr.of_desc ~level (TypeRepr.Arrow { label; lhs = clone lhs; rhs = clone rhs })
          | TypeRepr.Var { id; link=None; _ } ->
              (
                match Collections.HashMap.get var_replacements id with
                | Some replacement -> replacement
                | None ->
                    let replacement = TypeRepr.make_var ~level id in
                    let _ = Collections.HashMap.insert var_replacements id replacement in
                    replacement
              )
          | TypeRepr.Var { link=Some linked; _ } ->
              clone linked
          | TypeRepr.Hole id ->
              TypeRepr.of_desc ~level (TypeRepr.Hole id)
        in
        remember ty replacement
  in
  of_explicit ~quantified (clone body)

let free_vars = fun scheme ->
  let quantified, body = to_explicit scheme in
  TypeRepr.diff (TypeRepr.free_vars body) quantified
