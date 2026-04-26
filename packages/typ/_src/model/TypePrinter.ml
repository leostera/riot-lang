open Std

type render_state = {
  mutable next_name: int;
  mutable names: (int * string) list;
  generation: int;
  mutable next_order: int;
  aliases: (int, string) Collections.HashMap.t;
  states: (int, int) Collections.HashMap.t;
  rendered_aliases: int Collections.HashSet.t;
}

let make_render_state = fun () ->
  {
    next_name = 0;
    names = [];
    generation = TypeRepr.next_walk_generation ();
    next_order = 0;
    aliases = Collections.HashMap.with_capacity 16;
    states = Collections.HashMap.with_capacity 16;
    rendered_aliases = Collections.HashSet.create ();
  }

let pretty_name = fun index ->
  let suffix =
    if index < 26 then
      ""
    else
      string_of_int (index / 26)
  in
  let letter = Char.chr (Char.code 'a' + (index mod 26)) in
  "'" ^ String.make 1 letter ^ suffix

let fresh_name = fun state ->
  let name = pretty_name state.next_name in
  state.next_name <- state.next_name + 1;
  name

let name_for_var = fun state id ->
  match List.assoc_opt id state.names with
  | Some name -> name
  | None ->
      let name = fresh_name state in
      state.names <- (id, name) :: state.names;
      name

let order_for_ty = fun state ty ->
  let ty = TypeRepr.prune ty in
  if Int.equal (TypeRepr.mark ty) state.generation then
    TypeRepr.mark_order ty
  else
    let order = state.next_order in
    state.next_order <- order + 1;
  TypeRepr.set_mark ty state.generation;
  TypeRepr.set_mark_order ty order;
  order

let alias_for_ty = fun state ty ->
  let key = order_for_ty state ty in
  match Collections.HashMap.get state.aliases key with
  | Some alias -> alias
  | None ->
      let alias = fresh_name state in
      let _ = Collections.HashMap.insert state.aliases key alias in
      alias

let rec visit_children = fun state ty ->
  match TypeRepr.view (TypeRepr.prune ty) with
  | TypeRepr.Int
  | TypeRepr.Float
  | TypeRepr.Bool
  | TypeRepr.String
  | TypeRepr.Char
  | TypeRepr.Unit
  | TypeRepr.Hole _
  | TypeRepr.Var { link = None; _ } -> ()
  | TypeRepr.Option element
  | TypeRepr.Array element
  | TypeRepr.List element
  | TypeRepr.Seq element -> discover_recursive_aliases state element
  | TypeRepr.Result (ok_ty, error_ty) ->
      discover_recursive_aliases state ok_ty;
      discover_recursive_aliases state error_ty
  | TypeRepr.Named { arguments; _ }
  | TypeRepr.Tuple arguments -> List.iter (discover_recursive_aliases state) arguments
  | TypeRepr.Package signature ->
      List.iter
        (fun (value: TypeRepr.package_value) ->
          discover_recursive_aliases
            state
            (TypeScheme.body value.scheme))
        signature.values
  | TypeRepr.PolyVariant { tags; inherited; _ } ->
      List.iter
        (fun (tag: TypeRepr.poly_variant_tag) ->
          tag.payload_type
          |> Option.iter (discover_recursive_aliases state))
        tags;
      List.iter (discover_recursive_aliases state) inherited
  | TypeRepr.Arrow { lhs; rhs; _ } ->
      discover_recursive_aliases state lhs;
      discover_recursive_aliases state rhs
  | TypeRepr.Var { link = Some linked; _ } -> discover_recursive_aliases state linked

and discover_recursive_aliases = fun state ty ->
  let ty = TypeRepr.prune ty in
  let key = order_for_ty state ty in
  match Collections.HashMap.get state.states key with
  | Some 1 -> ignore (alias_for_ty state ty)
  | Some 2 -> ()
  | _ ->
      Collections.HashMap.insert state.states key 1
      |> ignore;
      visit_children state ty;
      Collections.HashMap.insert state.states key 2
      |> ignore

let wrap_alias = fun text alias -> "(" ^ text ^ " as " ^ alias ^ ")"

let render_arrow_label = function
  | TypeRepr.Nolabel -> ""
  | TypeRepr.Labelled label -> "~" ^ label ^ ":"
  | TypeRepr.Optional label -> "?" ^ label ^ ":"

let render_poly_variant_bound = function
  | TypeRepr.Exact -> ""
  | TypeRepr.UpperBound -> ">"
  | TypeRepr.LowerBound -> "<"

let rec render_type = fun state nested active ty ->
  let ty = TypeRepr.prune ty in
  let key = order_for_ty state ty in
  match Collections.HashMap.get state.aliases key with
  | Some alias when List.mem key active -> alias
  | Some alias when Collections.HashSet.contains state.rendered_aliases key -> alias
  | alias ->
      let body = render_type_body state nested (key :: active) ty in
      match alias with
      | Some alias ->
          Collections.HashSet.insert state.rendered_aliases key
          |> ignore;
          wrap_alias body alias
      | None -> body

and render_scheme = fun state active scheme ->
  let (quantified, body) = TypeScheme.to_explicit scheme in
  let quantified_names =
    quantified
    |> List.map (name_for_var state)
  in
  let () = discover_recursive_aliases state body in
  let body = render_type state false active body in
  match quantified_names with
  | [] -> body
  | _ -> String.concat " " quantified_names ^ ". " ^ body

and render_type_body = fun state nested active ty ->
  match TypeRepr.view ty with
  | TypeRepr.Int -> "int"
  | TypeRepr.Float -> "float"
  | TypeRepr.Bool -> "bool"
  | TypeRepr.String -> "string"
  | TypeRepr.Char -> "char"
  | TypeRepr.Unit -> "unit"
  | TypeRepr.Hole hole_id -> "_?" ^ Int.to_string hole_id
  | TypeRepr.Var var -> name_for_var state var.id
  | TypeRepr.Option element ->
      let text = render_type state true active element ^ " option" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Result (ok_ty, error_ty) ->
      let text =
        "("
        ^ render_type state false active ok_ty
        ^ ", "
        ^ render_type state false active error_ty
        ^ ") result"
      in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Array element ->
      let text = render_type state true active element ^ " array" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.List element ->
      let text = render_type state true active element ^ " list" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Seq element ->
      let text = render_type state true active element ^ " Seq.t" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Package signature ->
      let values =
        signature.values
        |> List.map
          (fun (value: TypeRepr.package_value) ->
            "val " ^ value.name ^ " : " ^ render_scheme state active value.scheme)
        |> String.concat "; "
      in
      "(module sig " ^ values ^ " end)"
  | TypeRepr.Named { head = { name; _ }; arguments } -> (
      match arguments with
      | [] -> SurfacePath.to_string name
      | [ argument ] -> render_type state true active argument ^ " " ^ SurfacePath.to_string name
      | arguments ->
          "("
          ^ (
            arguments
            |> List.map (fun argument -> render_type state false active argument)
            |> String.concat ", "
          )
          ^ ") "
          ^ SurfacePath.to_string name
    )
  | TypeRepr.PolyVariant { bound; tags; inherited } ->
      let members =
        (
          tags
          |> List.map (fun tag -> render_poly_variant_tag state active tag)
        )
        @ (
          inherited
          |> List.map (fun inherited_ty -> render_type state false active inherited_ty)
        )
      in
      let prefix = render_poly_variant_bound bound in
      let prefix =
        if String.equal prefix "" then
          ""
        else
          prefix ^ " "
      in
      let text = "[ " ^ prefix ^ String.concat " | " members ^ " ]" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Tuple members ->
      members
      |> List.map (fun member -> render_type state true active member)
      |> String.concat " * "
      |> fun text ->
        if nested then
          "(" ^ text ^ ")"
        else
          text
  | TypeRepr.Arrow { label; lhs; rhs } ->
      let text =
        render_arrow_label label
        ^ render_arrow_argument_type state active lhs
        ^ " -> "
        ^ render_type state false active rhs
      in
      if nested then
        "(" ^ text ^ ")"
      else
        text

and render_arrow_argument_type = fun state active ty ->
  match TypeRepr.view (TypeRepr.prune ty) with
  | TypeRepr.Arrow _
  | TypeRepr.Tuple _
  | TypeRepr.PolyVariant _ -> "(" ^ render_type state false active ty ^ ")"
  | _ -> render_type state false active ty

and render_poly_variant_tag = fun state active (tag: TypeRepr.poly_variant_tag) ->
  match tag.payload_type with
  | Some payload_type -> "`" ^ tag.name ^ " of " ^ render_type state false active payload_type
  | None -> "`" ^ tag.name

let type_to_string = fun ty ->
  let state = make_render_state () in
  discover_recursive_aliases state ty;
  render_type state false [] ty

let scheme_to_string = fun scheme ->
  let state = make_render_state () in
  render_scheme state [] scheme
