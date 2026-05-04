open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "ordered-argument-kinds"

let rule_description =
  "Function parameters should be ordered as labeled, then optional, then positional"

let rule_explain =
  {|
This rule enforces one stable parameter layout: labeled arguments first, optional
arguments next, and positional arguments last.

That order keeps the configurable surface of a function near the front of the
signature. Readers can see the knobs first and the required positional data afterward.
When positional parameters come first, the named part of the API is easier to miss and
call sites become less uniform.

The goal is not theoretical purity. It is to make function signatures easier to skim
and easier to keep consistent across a codebase.
|}

type parameter_kind =
  | Labeled
  | Optional
  | Positional

let kind_rank = fun kind ->
  match kind with
  | Labeled -> 0
  | Optional -> 1
  | Positional -> 2

let kind_name = fun kind ->
  match kind with
  | Labeled -> "labeled"
  | Optional -> "optional"
  | Positional -> "positional"

let parameter_kind = fun parameter ->
  match H.parameter_kind parameter with
  | Some H.LabeledParameter -> Some Labeled
  | Some H.OptionalParameter -> Some Optional
  | None -> Some Positional

let make_diagnostic = fun parameter kind ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Parameter.as_node parameter))
    ~suggestion:("Move this "
    ^ kind_name kind
    ^ " argument earlier so parameters stay ordered as labeled, optional, then positional")
    ()

let diagnostic_for_binding = fun binding ->
  let highest_rank = ref (-1) in
  let found = ref None in
  H.iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun parameter ->
      match !found with
      | Some _ -> ()
      | None -> (
          match parameter_kind parameter with
          | None -> ()
          | Some kind ->
              let rank = kind_rank kind in
              if rank < !highest_rank then
                found := Some (make_diagnostic parameter kind)
              else
                highest_rank := rank
        ));
  !found

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
