open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.of_string "prefer-record-destructuring-parameters"

let rule_description =
  "Functions that immediately destructure a record argument should destructure it in the parameter"

let rule_explain =
  {|
When a helper function immediately starts with `let { ... } = value in ...`, or spends
its whole body reading `value.name`, `value.email`, and `value.role`, the real shape of
the function is already telling you that it wants a destructured record parameter, not
a temporary name plus a second layer of field lookups.

Writing the destructuring at the parameter boundary makes the contract visible earlier.
Readers can see which fields matter before they enter the function body, and adding a
new record field is harder to ignore accidentally because the pattern is part of the
signature-shaped surface of the helper.

This is especially useful for internal helpers such as serializers, encoders, and view
renderers that consume a record immediately and never use the whole record value on its
own.
|}

type usage = {
  fields: string Vector.t;
  mutable has_whole_value_use: bool;
}

let diagnostic = fun parameter_token parameter_name ->
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token:parameter_token
    ~suggestion:("Destructure this record in the parameter list instead of binding "
    ^ parameter_name
    ^ " and immediately unpacking it in the function body")
    ()

let single_positional_parameter_name = fun binding ->
  let parameters = Vector.with_capacity ~size:(Ast.LetBinding.parameter_count binding) in
  H.iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun parameter ->
      match H.parameter_name_token parameter with
      | Some token -> Vector.push parameters ~value:token
      | None -> ());
  if Int.equal (Vector.length parameters) 1 then
    Some (Vector.get_unchecked parameters ~at:0)
  else
    None

let ident_segments = fun ctx ident ->
  ignore ctx;
  Ast.Ident.text ident
  |> String.split ~by:"."

let rec unwrap_record_pattern = fun pattern ->
  let pattern = H.unwrap_pattern pattern in
  match Ast.Pattern.view pattern with
  | Ast.Pattern.Record _ -> Ast.cast_result_to_option (Ast.RecordPattern.cast pattern)
  | Ast.Pattern.Constraint { pattern; _ }
  | Ast.Pattern.Alias { pattern; _ } -> unwrap_record_pattern pattern
  | _ -> None

let record_pattern_field_count = fun record ->
  let count = ref 0 in
  H.iter_fold Ast.RecordPattern.fold_field record ~fn:(fun _ -> count := !count + 1);
  !count

let rec expr_is_parameter_path = fun ctx expected_name expr ->
  match Ast.Expr.view (H.unwrap_expr expr) with
  | Ast.Expr.Ident { ident } -> (
      match ident_segments ctx ident with
      | [ name ] -> String.equal expected_name name
      | _ -> false
    )
  | Ast.Expr.Annotated { expr = inner; _ } -> expr_is_parameter_path ctx expected_name inner
  | _ -> false

let is_immediate_record_destructure = fun ctx expected_name expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Let { first_binding = binding; _ } -> (
      match (Ast.LetBinding.pattern binding, Ast.LetBinding.body binding) with
      | (Some pattern, Some bound_value) -> (
          match unwrap_record_pattern pattern with
          | Some record ->
              record_pattern_field_count record >= 2
              && expr_is_parameter_path ctx expected_name bound_value
          | None -> false
        )
      | _ -> false
    )
  | _ -> false

let usage_has_two_distinct_fields = fun usage ->
  let first = ref None in
  let found_distinct = ref false in
  Vector.for_each
    usage.fields
    ~fn:(fun field ->
      match !first with
      | None -> first := Some field
      | Some first when not (String.equal first field) -> found_distinct := true
      | Some _ -> ());
  !found_distinct

let should_prefer_destructuring = fun ctx expected_name expr ->
  let usage = {
    fields = Vector.with_capacity ~size:(Ast.Expr.child_expr_count expr);
    has_whole_value_use = false;
  }
  in
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_expr =
        Some (fun visitor expr ->
          (
            match Ast.Expr.view expr with
            | Ast.Expr.Ident { ident } -> (
                match ident_segments ctx ident with
                | [ name ] when String.equal expected_name name -> usage.has_whole_value_use <- true
                | [ base; field ] when String.equal expected_name base ->
                    Vector.push usage.fields ~value:field
                | _ -> ()
              )
            | _ -> ()
          );
          (visitor, Syn.Visitor.Continue));
    }
  in
  Syn.Visitor.make ~ctx:() ~hooks
  |> fun visitor ->
    ignore (Syn.Visitor.visit_expr visitor expr);
    usage_has_two_distinct_fields usage && not usage.has_whole_value_use

let check_binding = fun ctx diagnostics binding ->
  if H.binding_is_function binding then
    match single_positional_parameter_name binding with
    | Some parameter_token -> (
        let parameter_name = Ast.Token.text parameter_token in
        match Ast.LetBinding.body binding with
        | Some body when is_immediate_record_destructure ctx parameter_name body
        || should_prefer_destructuring ctx parameter_name body ->
            H.push_diagnostic diagnostics (diagnostic parameter_token parameter_name)
        | _ -> ()
      )
    | None -> ()

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_let_binding =
        Some (fun visitor binding ->
          check_binding ctx diagnostics binding;
          (visitor, Syn.Visitor.Continue));
    }
  in
  Syn.Visitor.make ~ctx:() ~hooks
  |> fun visitor ->
    ignore (Syn.Visitor.visit_node visitor root);
    H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
