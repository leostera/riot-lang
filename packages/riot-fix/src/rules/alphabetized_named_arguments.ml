open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "alphabetized-named-arguments"

let rule_description = "Labeled and optional arguments should be alphabetized within their groups"

let rule_explain =
  {|
Alphabetizing named arguments is not about claiming that alphabetical order is
semantically meaningful. It is about removing arbitrary ordering decisions from APIs so
readers do not have to wonder whether the order carries meaning when it does not.

Once a project adopts a stable ordering, small edits become easier to review.
Inserted parameters land in an obvious place, missing ones are easier to spot, and
different modules stop inventing slightly different layouts for the same shape of
function.

This rule only applies within the named-argument groups. It is there to reduce
unproductive bikeshedding and make signatures mechanically easier to scan.
|}

let parameter_name = fun parameter ->
  match H.parameter_name_token parameter with
  | Some token -> Ast.Token.text token
  | None -> ""

let make_diagnostic = fun ~previous_name parameter ->
  let current_name = parameter_name parameter in
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Parameter.as_node parameter))
    ~suggestion:("Place "
    ^ current_name
    ^ " before "
    ^ previous_name
    ^ " so named arguments stay alphabetized")
    ()

type parameter_group =
  | Labeled
  | Optional

let classify_parameter = fun parameter ->
  match H.parameter_kind parameter with
  | Some H.LabeledParameter -> Some Labeled
  | Some H.OptionalParameter -> Some Optional
  | None -> None

let first_out_of_order = fun parameters ->
  let previous_name = ref None in
  let found = ref None in
  Vector.for_each
    parameters
    ~fn:(fun parameter ->
      match !found with
      | Some _ -> ()
      | None -> (
          let name = parameter_name parameter in
          match !previous_name with
          | Some previous when String.compare name previous = Order.LT ->
              found := Some (previous, parameter)
          | _ -> previous_name := Some name
        ));
  !found

let diagnostic_for_binding = fun binding ->
  let labeled = Vector.with_capacity ~size:4 in
  let optional = Vector.with_capacity ~size:4 in
  H.iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun parameter ->
      match classify_parameter parameter with
      | Some Labeled -> Vector.push labeled ~value:parameter
      | Some Optional -> Vector.push optional ~value:parameter
      | None -> ());
  match first_out_of_order labeled with
  | Some (previous_name, parameter) -> Some (make_diagnostic ~previous_name parameter)
  | None -> (
      match first_out_of_order optional with
      | Some (previous_name, parameter) -> Some (make_diagnostic ~previous_name parameter)
      | None -> None
    )

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  H.for_each_let_binding
    root
    ~fn:(fun binding ->
      if H.binding_is_function binding then
        match diagnostic_for_binding binding with
        | Some diagnostic -> H.push_diagnostic diagnostics diagnostic
        | None -> ());
  H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
