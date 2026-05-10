open Std
open Std.Collections

module Api = Fixme
module H = Ast_rule_helpers

let package_name = "std"

let package_rule_id = Api.Rule_id.from_string (package_name ^ ":prefer-iter-over-ignored-map")

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "Ignoring the result of List.map or Iter.map should usually use the corresponding iter form instead.";
    body = {|
`map` is for transforming a collection into a new collection. When the result is
immediately discarded with `ignore`, that transformation result was never the real goal.
What the code is really doing is visiting each element for side effects.

Using `List.iter` or `Iter.iter` makes that intent explicit. It tells the reader that
the traversal matters, not the returned collection, and it avoids allocating a result
that the program then throws away.

This rule only targets the clear cases `ignore (List.map f xs)` and
`ignore (Iter.map f iter)`. In those shapes, the iter form is a better statement of the
program you meant to write.
|};
  }

let explanations = fun () -> [ explanation ]

let make_diagnostic = fun ~iter_name expr ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message;
    })
    ~span:(H.expr_span expr)
    ~suggestion:("Use "
    ^ iter_name
    ^ " when the mapped result is ignored and the traversal exists only for side effects.")
    ()

let diagnostic_for_expression = fun expr ->
  let (head, arguments) = H.flatten_apply expr in
  if not (H.path_matches ~expected:"ignore" head) then
    None
  else
    match arguments with
    | [ mapped ] ->
        let (mapped_head, mapped_arguments) = H.flatten_apply mapped in
        match mapped_arguments with
        | [ _fn; _collection ] when H.path_matches ~expected:"List.map" mapped_head ->
            Some (make_diagnostic ~iter_name:"List.iter" expr)
        | [ _fn; _collection ] when H.path_matches ~expected:"Iter.map" mapped_head ->
            Some (make_diagnostic ~iter_name:"Iter.iter" expr)
        | _ -> None
    | _ -> None

let check_tree = fun (ctx: Api.Rule.context) _red_root ->
  Riot_fix.Rule_query.expressions ctx
  |> List.filter_map ~fn:diagnostic_for_expression

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:explanation.message
    ~explain:explanation.body
    ~run:check_tree
    ()
