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

let instantiate = fun ~fresh_var ~make scheme ->
  let replacements = Collections.HashMap.with_capacity 8 in
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
      match TypeRepr.view ty with
      | (TypeRepr.Int | TypeRepr.Float | TypeRepr.Bool | TypeRepr.String | TypeRepr.Char | TypeRepr.Unit | TypeRepr.Hole _) ->
          ty
      | TypeRepr.Option element ->
          let element' = loop element in
          if Std.Ptr.equal element element' then
            ty
          else
            make (TypeRepr.Option element')
      | TypeRepr.Result (ok_ty, error_ty) ->
          let ok_ty' = loop ok_ty in
          let error_ty' = loop error_ty in
          if Std.Ptr.equal ok_ty ok_ty' && Std.Ptr.equal error_ty error_ty' then
            ty
          else
            make (TypeRepr.Result (ok_ty', error_ty'))
      | TypeRepr.Array element ->
          let element' = loop element in
          if Std.Ptr.equal element element' then
            ty
          else
            make (TypeRepr.Array element')
      | TypeRepr.List element ->
          let element' = loop element in
          if Std.Ptr.equal element element' then
            ty
          else
            make (TypeRepr.List element')
      | TypeRepr.Seq element ->
          let element' = loop element in
          if Std.Ptr.equal element element' then
            ty
          else
            make (TypeRepr.Seq element')
      | TypeRepr.Named { type_constructor_id; name; arguments } ->
          let arguments' = map_preserving loop arguments in
          if Std.Ptr.equal arguments arguments' then
            ty
          else
            make (TypeRepr.Named { type_constructor_id; name; arguments = arguments' })
      | TypeRepr.Tuple members ->
          let members' = map_preserving loop members in
          if Std.Ptr.equal members members' then
            ty
          else
            make (TypeRepr.Tuple members')
      | TypeRepr.Arrow { label; lhs; rhs } ->
          let lhs' = loop lhs in
          let rhs' = loop rhs in
          if Std.Ptr.equal lhs lhs' && Std.Ptr.equal rhs rhs' then
            ty
          else
            make (TypeRepr.Arrow { label; lhs = lhs'; rhs = rhs' })
      | TypeRepr.Var { id; link=None; _ } ->
          if TypeRepr.is_generic_var ty then
            match Collections.HashMap.get replacements id with
            | Some replacement -> replacement
            | None ->
                let replacement = fresh_var () in
                let _ = Collections.HashMap.insert replacements id replacement in
                replacement
          else
            ty
      | TypeRepr.Var { link=Some linked; _ } ->
          loop linked
  in
  loop scheme

let free_vars = fun scheme ->
  let quantified, body = to_explicit scheme in
  TypeRepr.diff (TypeRepr.free_vars body) quantified
