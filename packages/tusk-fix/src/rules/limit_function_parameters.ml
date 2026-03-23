open Std

let rule_id = "limit-function-parameters"
let rule_description =
  "Functions should keep parameter counts small so call sites stay readable"

let rule_explain =
  {|
Functions with too many parameters should be avoided.

Why this rule exists:
- A long parameter list usually hides a record-shaped concept that wants a name.
- Smaller parameter lists are easier to read, harder to mix up, and easier to refactor.

Thresholds:
- Positional-only functions should stay below 5 parameters.
- Named-only functions should stay below 8 parameters.
- Mixed named and positional functions should stay below 10 parameters.

If a function keeps growing parameters like `~purchased_at`, `~quantity`, and `~item`,
that usually means a `Purchase_order.t` or similar record wants to exist.
|}

type parameter_counts = {
  positional_count : int;
  named_count : int;
}

let count_parameter counts parameter =
  match parameter with
  | Syn.Cst.Parameter.Positional _ ->
      { counts with positional_count = counts.positional_count + 1 }
  | Syn.Cst.Parameter.Labeled _ | Syn.Cst.Parameter.Optional _ ->
      { counts with named_count = counts.named_count + 1 }
  | Syn.Cst.Parameter.LocallyAbstract _ ->
      counts

let parameter_counts binding =
  Syn.Cst.LetBinding.parameters binding
  |> List.fold_left count_parameter { positional_count = 0; named_count = 0 }

let exceeds_limit counts =
  let total = counts.positional_count + counts.named_count in
  if counts.named_count = 0 then
    counts.positional_count >= 5
  else if counts.positional_count = 0 then
    counts.named_count >= 8
  else
    total >= 10

let threshold_description counts =
  if counts.named_count = 0 then
    "positional-only functions should stay below 5 parameters"
  else if counts.positional_count = 0 then
    "named-only functions should stay below 8 parameters"
  else
    "mixed named and positional functions should stay below 10 parameters"

let make_diagnostic binding counts =
  let total = counts.positional_count + counts.named_count in
  match Syn.Cst.LetBinding.binding_name_token binding with
  | Some token ->
      Some
        (Diagnostic.make ~severity:Warning
           ~kind:
             (Diagnostic.Known { rule_id; message = rule_description })
           ~span:(Syn.Cst.Token.span token)
           ~suggestion:
             ("This function has "
            ^ Int.to_string total
            ^ " parameters; consider introducing a named record parameter because "
            ^ threshold_description counts)
           ())
  | None -> None

let diagnostic_for_binding binding =
  let counts = parameter_counts binding in
  if exceeds_limit counts then
    make_diagnostic binding counts
  else
    None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.filter Syn.Cst.LetBinding.is_function
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
