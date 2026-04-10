open Std

type t = TypeRepr.scheme

let of_type = fun ty ->
  TypeRepr.seal_levels ty;
  TypeRepr.make_scheme ~quantified:(TypeRepr.generic_var_ids ty) ~body:ty

let of_explicit = fun ~quantified body ->
  TypeRepr.generalize_ids quantified body;
  TypeRepr.seal_levels body;
  TypeRepr.make_scheme ~quantified ~body

let body = fun scheme -> TypeRepr.scheme_body scheme

let to_explicit = fun scheme -> (TypeRepr.scheme_quantified scheme, TypeRepr.scheme_body scheme)

let map_preserving = fun loop xs ->
  let rec walk changed acc = function
    | [] ->
        if changed then
          List.rev acc
        else
          xs
    | x :: rest ->
        let mapped_x = loop x in
        walk (changed || not (Std.Ptr.equal x mapped_x)) (mapped_x :: acc) rest
  in
  walk false [] xs

let map_type_preserving = fun map_scheme_body scheme ->
  let quantified, original_body = to_explicit scheme in
  let mapped_body = map_scheme_body original_body in
  if Std.Ptr.equal original_body mapped_body then
    scheme
  else
    of_explicit ~quantified mapped_body

let instantiate = fun ~fresh_var ~make ~next_mark scheme ->
  let scheme_body = body scheme in
  let generation = next_mark () in
  let next_order =
    let order = ref 0 in
    fun () ->
      let current = !order in
      order := current + 1;
      current
  in
  let replacements = Collections.HashMap.with_capacity 16 in
  let order_for_ty ty =
    if Int.equal (TypeRepr.aux_mark ty) generation then
      TypeRepr.aux_order ty
    else
      let order = next_order () in
      TypeRepr.set_aux_mark ty generation;
      TypeRepr.set_aux_order ty order;
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
  let generic_replacements = Collections.HashMap.with_capacity 16 in
  let prepare_shell ty =
    let shell = make (TypeRepr.Hole (-1)) in
    TypeRepr.set_level shell (TypeRepr.level ty);
    let _ = Collections.HashMap.insert replacements (order_for_ty ty) shell in
    shell
  in
  let rec copy_scheme scheme =
    map_type_preserving copy scheme
  and copy ty =
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
        let shell = prepare_shell ty in
        let element' = copy element in
        shell.TypeRepr.desc <- TypeRepr.Option element';
        shell
    | TypeRepr.Result (ok_ty, error_ty) ->
        let shell = prepare_shell ty in
        let ok_ty' = copy ok_ty in
        let error_ty' = copy error_ty in
        shell.TypeRepr.desc <- TypeRepr.Result (ok_ty', error_ty');
        shell
    | TypeRepr.Array element ->
        let shell = prepare_shell ty in
        let element' = copy element in
        shell.TypeRepr.desc <- TypeRepr.Array element';
        shell
    | TypeRepr.List element ->
        let shell = prepare_shell ty in
        let element' = copy element in
        shell.TypeRepr.desc <- TypeRepr.List element';
        shell
    | TypeRepr.Seq element ->
        let shell = prepare_shell ty in
        let element' = copy element in
        shell.TypeRepr.desc <- TypeRepr.Seq element';
        shell
    | TypeRepr.Package signature ->
        let shell = prepare_shell ty in
        let values =
          map_preserving
            (fun (value: TypeRepr.package_value) ->
              let copied_scheme = copy_scheme value.scheme in
              if Std.Ptr.equal value.scheme copied_scheme then
                value
              else
                { value with scheme = copied_scheme })
            signature.values
        in
        shell.TypeRepr.desc <- TypeRepr.Package { values };
        shell
    | TypeRepr.Named { head; arguments } ->
        let shell = prepare_shell ty in
        let arguments' = map_preserving copy arguments in
        shell.TypeRepr.desc <- TypeRepr.Named { head; arguments = arguments' };
        shell
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let shell = prepare_shell ty in
        let tags' =
          map_preserving
            (fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type -> { tag with payload_type = Some (copy payload_type) }
              | None -> tag)
            tags
        in
        let inherited' = map_preserving copy inherited in
        shell.TypeRepr.desc <- TypeRepr.PolyVariant { bound; tags = tags'; inherited = inherited' };
        shell
    | TypeRepr.Tuple members ->
        let shell = prepare_shell ty in
        let members' = map_preserving copy members in
        shell.TypeRepr.desc <- TypeRepr.Tuple members';
        shell
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let shell = prepare_shell ty in
        let lhs' = copy lhs in
        let rhs' = copy rhs in
        shell.TypeRepr.desc <- TypeRepr.Arrow { label; lhs = lhs'; rhs = rhs' };
        shell
    | TypeRepr.Var { id; link=None; _ } ->
        if TypeRepr.is_generic_var ty then
          match Collections.HashMap.get generic_replacements id with
          | Some replacement -> remember_replacement ty replacement
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
  copy scheme_body

let next_copy_generation =
  let generation = ref 0 in
  fun () ->
    let current = !generation in
    generation := current + 1;
    current

let copy = fun scheme ->
  let quantified, original_body = to_explicit scheme in
  let generation = next_copy_generation () in
  let next_order =
    let order = ref 0 in
    fun () ->
      let current = !order in
      order := current + 1;
      current
  in
  let replacements = Collections.HashMap.with_capacity 16 in
  let var_replacements = Collections.HashMap.with_capacity 16 in
  let order_for_ty ty =
    if Int.equal (TypeRepr.mark ty) generation then
      TypeRepr.mark_order ty
    else
      let order = next_order () in
      TypeRepr.set_mark ty generation;
      TypeRepr.set_mark_order ty order;
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
  let prepare_shell ty =
    let shell = TypeRepr.shell ~level:(TypeRepr.level ty) () in
    let _ = Collections.HashMap.insert replacements (order_for_ty ty) shell in
    shell
  in
  let rec clone_scheme scheme =
    map_type_preserving clone scheme
  and clone ty =
    let ty = TypeRepr.prune ty in
    match lookup_replacement ty with
    | Some replacement -> replacement
    | None ->
        let level = TypeRepr.level ty in
        let replacement =
          match TypeRepr.view ty with
          | TypeRepr.Int ->
              TypeRepr.int
          | TypeRepr.Float ->
              TypeRepr.float
          | TypeRepr.Bool ->
              TypeRepr.bool
          | TypeRepr.String ->
              TypeRepr.string
          | TypeRepr.Char ->
              TypeRepr.char
          | TypeRepr.Unit ->
              TypeRepr.unit_
          | TypeRepr.Option element ->
              let shell = prepare_shell ty in
              shell.TypeRepr.desc <- TypeRepr.Option (clone element);
              shell
          | TypeRepr.Result (ok_ty, error_ty) ->
              let shell = prepare_shell ty in
              shell.TypeRepr.desc <- TypeRepr.Result (clone ok_ty, clone error_ty);
              shell
          | TypeRepr.Array element ->
              let shell = prepare_shell ty in
              shell.TypeRepr.desc <- TypeRepr.Array (clone element);
              shell
          | TypeRepr.List element ->
              let shell = prepare_shell ty in
              shell.TypeRepr.desc <- TypeRepr.List (clone element);
              shell
          | TypeRepr.Seq element ->
              let shell = prepare_shell ty in
              shell.TypeRepr.desc <- TypeRepr.Seq (clone element);
              shell
          | TypeRepr.Package signature ->
              let shell = prepare_shell ty in
              let values =
                map_preserving
                  (fun (value: TypeRepr.package_value) ->
                    let copied_scheme = clone_scheme value.scheme in
                    if Std.Ptr.equal value.scheme copied_scheme then
                      value
                    else
                      { value with scheme = copied_scheme })
                  signature.values
              in
              shell.TypeRepr.desc <- TypeRepr.Package { values };
              shell
          | TypeRepr.Named { head; arguments } ->
              let shell = prepare_shell ty in
              shell.TypeRepr.desc <- TypeRepr.Named { head; arguments = map_preserving clone arguments };
              shell
          | TypeRepr.PolyVariant { bound; tags; inherited } ->
              let shell = prepare_shell ty in
              let tags =
                tags
                |> map_preserving
                  (fun (tag: TypeRepr.poly_variant_tag) ->
                    match tag.payload_type with
                    | Some payload_type -> { tag with payload_type = Some (clone payload_type) }
                    | None -> tag)
              in
              let inherited = map_preserving clone inherited in
              shell.TypeRepr.desc <- TypeRepr.PolyVariant { bound; tags; inherited };
              shell
          | TypeRepr.Tuple members ->
              let shell = prepare_shell ty in
              shell.TypeRepr.desc <- TypeRepr.Tuple (map_preserving clone members);
              shell
          | TypeRepr.Arrow { label; lhs; rhs } ->
              let shell = prepare_shell ty in
              shell.TypeRepr.desc <- TypeRepr.Arrow { label; lhs = clone lhs; rhs = clone rhs };
              shell
          | TypeRepr.Var { id; link=None; _ } -> (
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
  of_explicit ~quantified (clone original_body)

let free_vars = fun scheme ->
  let quantified, body = to_explicit scheme in
  TypeRepr.diff (TypeRepr.free_vars body) quantified
