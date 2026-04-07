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
  let lookup_replacement = fun ty ->
    if Int.equal (TypeRepr.mark ty) generation then
      Collections.HashMap.get replacements (TypeRepr.mark_order ty)
    else
      None
  in
  let remember_replacement = fun ty replacement ->
    let order =
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
    let _ = Collections.HashMap.insert replacements order replacement in
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
  let rec loop ty =
    let ty = TypeRepr.prune ty in
    if TypeRepr.level ty < TypeRepr.generic_level then
      ty
    else
      match lookup_replacement ty with
      | Some replacement -> replacement
      | None -> (
          match TypeRepr.view ty with
          | (TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _) ->
              ty
          | TypeRepr.Option element ->
              let element' = loop element in
              if Std.Ptr.equal element element' then
                remember_replacement ty ty
              else
                remember_replacement ty (make (TypeRepr.Option element'))
          | TypeRepr.Result (ok_ty, error_ty) ->
              let ok_ty' = loop ok_ty in
              let error_ty' = loop error_ty in
              if Std.Ptr.equal ok_ty ok_ty' && Std.Ptr.equal error_ty error_ty' then
                remember_replacement ty ty
              else
                remember_replacement ty (make (TypeRepr.Result (ok_ty', error_ty')))
          | TypeRepr.Array element ->
              let element' = loop element in
              if Std.Ptr.equal element element' then
                remember_replacement ty ty
              else
                remember_replacement ty (make (TypeRepr.Array element'))
          | TypeRepr.List element ->
              let element' = loop element in
              if Std.Ptr.equal element element' then
                remember_replacement ty ty
              else
                remember_replacement ty (make (TypeRepr.List element'))
          | TypeRepr.Seq element ->
              let element' = loop element in
              if Std.Ptr.equal element element' then
                remember_replacement ty ty
              else
                remember_replacement ty (make (TypeRepr.Seq element'))
          | TypeRepr.Named { type_constructor; name; arguments } ->
              let arguments' = map_preserving loop arguments in
              if Std.Ptr.equal arguments arguments' then
                remember_replacement ty ty
              else
                remember_replacement ty (make (TypeRepr.Named { type_constructor; name; arguments = arguments' }))
          | TypeRepr.Tuple members ->
              let members' = map_preserving loop members in
              if Std.Ptr.equal members members' then
                remember_replacement ty ty
              else
                remember_replacement ty (make (TypeRepr.Tuple members'))
          | TypeRepr.Arrow { label; lhs; rhs } ->
              let lhs' = loop lhs in
              let rhs' = loop rhs in
              if Std.Ptr.equal lhs lhs' && Std.Ptr.equal rhs rhs' then
                remember_replacement ty ty
              else
                remember_replacement ty (make (TypeRepr.Arrow { label; lhs = lhs'; rhs = rhs' }))
          | TypeRepr.Var { link=None; _ } ->
              if TypeRepr.is_generic_var ty then
                remember_replacement ty (fresh_var ())
              else
                ty
          | TypeRepr.Var { link=Some linked; _ } ->
              loop linked
        )
  in
  loop scheme

let free_vars = fun scheme ->
  let quantified, body = to_explicit scheme in
  TypeRepr.diff (TypeRepr.free_vars body) quantified
