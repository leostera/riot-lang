open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "t-first-named-arguments"

let rule_description = "When named arguments are present, keep t as the first positional argument"

let rule_explain =
  {|
In Riot-style APIs, `t` often plays the role of the receiver or primary state value.
When that is true, callers benefit from seeing it in the same place consistently.

Keeping `t` as the first positional argument after any named arguments makes pipeline
use and method-like reading more predictable. A function such as
`render ~width ~height t` reads naturally as "render this `t` with these options".

If `t` is buried after other positional arguments, callers have to relearn the calling
convention for each function instead of relying on a stable pattern.
|}

let make_diagnostic = fun parameter ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Parameter.as_node parameter))
    ~suggestion:"Move t to the front of the positional arguments so the function reads as named configuration followed by the receiver"
    ()

let is_named_parameter = fun parameter -> Option.is_some (H.parameter_kind parameter)

let is_positional_parameter = fun parameter -> not (is_named_parameter parameter)

let parameter_name_is = fun name parameter ->
  match H.parameter_name_token parameter with
  | Some token -> String.equal (Ast.Token.text token) name
  | None -> false

let diagnostic_for_binding = fun binding ->
  let has_named_args = ref false in
  let positional = Vector.with_capacity ~size:4 in
  H.iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun parameter ->
      if is_named_parameter parameter then
        has_named_args := true
      else if is_positional_parameter parameter then
        Vector.push positional ~value:parameter);
  if not !has_named_args then
    None
  else
    match Vector.first positional with
    | Some first when parameter_name_is "t" first -> None
    | _ ->
        let found_t = ref None in
        Vector.for_each
          positional
          ~fn:(fun parameter ->
            match !found_t with
            | Some _ -> ()
            | None ->
                if parameter_name_is "t" parameter then
                  found_t := Some parameter);
        Option.map !found_t ~fn:make_diagnostic

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
