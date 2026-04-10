open Std

type render_state = {
  mutable next_name: int;
  mutable names: (int * string) list;
}

let make_render_state = fun () -> { next_name = 0; names = [] }

let pretty_name = fun index ->
  let suffix =
    if index < 26 then
      ""
    else
      string_of_int (index / 26)
  in
  let letter = Char.chr (Char.code 'a' + (index mod 26)) in
  "'" ^ String.make 1 letter ^ suffix

let name_for_var = fun state id ->
  match List.assoc_opt id state.names with
  | Some name -> name
  | None ->
      let name = pretty_name state.next_name in
      state.next_name <- state.next_name + 1;
      state.names <- (id, name) :: state.names;
      name

let render_arrow_label = function
  | TypeRepr.Nolabel -> ""
  | TypeRepr.Labelled label -> "~" ^ label ^ ":"
  | TypeRepr.Optional label -> "?" ^ label ^ ":"

let render_poly_variant_bound = function
  | TypeRepr.Exact -> ""
  | TypeRepr.UpperBound -> ">"
  | TypeRepr.LowerBound -> "<"

let rec render_type = fun state ~nested ty ->
  match TypeRepr.view (TypeRepr.prune ty) with
  | TypeRepr.Int ->
      "int"
  | TypeRepr.Float ->
      "float"
  | TypeRepr.Bool ->
      "bool"
  | TypeRepr.String ->
      "string"
  | TypeRepr.Char ->
      "char"
  | TypeRepr.Unit ->
      "unit"
  | TypeRepr.Hole hole_id ->
      "_?" ^ Int.to_string hole_id
  | TypeRepr.Var var ->
      name_for_var state var.id
  | TypeRepr.Option element ->
      let text = render_type state ~nested:true element ^ " option" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Result (ok_ty, error_ty) ->
      let text = "("
      ^ render_type state ~nested:false ok_ty
      ^ ", "
      ^ render_type state ~nested:false error_ty
      ^ ") result" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Array element ->
      let text = render_type state ~nested:true element ^ " array" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.List element ->
      let text = render_type state ~nested:true element ^ " list" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Seq element ->
      let text = render_type state ~nested:true element ^ " Seq.t" in
      if nested then
        "(" ^ text ^ ")"
      else
        text
  | TypeRepr.Package signature ->
      let values = signature.values
      |> List.map
        (fun (value: TypeRepr.package_value) ->
          "val " ^ value.name ^ " : " ^ render_type state ~nested:false value.scheme)
      |> String.concat "; " in
      "(module sig " ^ values ^ " end)"
  | TypeRepr.Named { head={ name; _ }; arguments } -> (
      match arguments with
      | [] -> IdentPath.to_string name
      | [ argument ] -> render_type state ~nested:true argument ^ " " ^ IdentPath.to_string name
      | arguments -> "("
      ^ (arguments |> List.map (render_type state ~nested:false) |> String.concat ", ")
      ^ ") "
      ^ IdentPath.to_string name
    )
  | TypeRepr.PolyVariant { bound; tags; inherited } ->
      let members = (tags |> List.map (render_poly_variant_tag state))
      @ (inherited |> List.map (render_type state ~nested:false)) in
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
      members |> List.map (render_type state ~nested:true) |> String.concat " * " |> fun text ->
        if nested then
          "(" ^ text ^ ")"
        else
          text
  | TypeRepr.Arrow { label; lhs; rhs } ->
      let text = render_arrow_label label
      ^ render_arrow_argument_type state lhs
      ^ " -> "
      ^ render_type state ~nested:false rhs in
      if nested then
        "(" ^ text ^ ")"
      else
        text

and render_arrow_argument_type = fun state ty ->
  match TypeRepr.view (TypeRepr.prune ty) with
  | TypeRepr.Arrow _
  | TypeRepr.Tuple _
  | TypeRepr.PolyVariant _ -> "(" ^ render_type state ~nested:false ty ^ ")"
  | _ -> render_type state ~nested:false ty

and render_poly_variant_tag = fun state (tag: TypeRepr.poly_variant_tag) ->
  match tag.payload_type with
  | Some payload_type -> "`" ^ tag.name ^ " of " ^ render_type state ~nested:false payload_type
  | None -> "`" ^ tag.name

let type_to_string = fun ty ->
  let state = make_render_state () in
  render_type state ~nested:false ty

let scheme_to_string = fun scheme ->
  let state = make_render_state () in
  let quantified, body = TypeScheme.to_explicit scheme in
  let quantified_names = quantified |> List.map (name_for_var state) in
  let body = render_type state ~nested:false body in
  match quantified_names with
  | [] -> body
  | _ -> String.concat " " quantified_names ^ ". " ^ body
