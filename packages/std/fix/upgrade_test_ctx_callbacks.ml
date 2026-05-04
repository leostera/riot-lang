open Std
open Std.Collections

module Api = Fixme
module Ast = Syn.Ast
module H = Ast_rule_helpers

let package_name = "std"

let package_rule_id = Api.Rule_id.from_string (package_name ^ ":upgrade-test-ctx-callbacks")

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "Std.Test callbacks should accept a ctx argument instead of unit.";
    body = {|
`Std.Test` callbacks now receive a context record instead of being called with
`()`.

Examples:

Instead of:

```ocaml
Test.case "example" (fun () -> Ok ())

let test_case () = Ok ()
let tests = [ Test.case "example" test_case ]
```

write:

```ocaml
Test.case "example" (fun _ctx -> Ok ())

let test_case _ctx = Ok ()
let tests = [ Test.case "example" test_case ]
```

The first migration pass only rewrites callbacks that are clearly used as
`Std.Test`/`Test` cases in test source files. Ambiguous helper functions are
left alone for manual review.
|};
  }

let explanations = fun () -> [ explanation ]

let is_test_source_path = fun path ->
  let basename =
    match path
    |> String.split_on_char '/'
    |> List.rev with
    | basename :: _ -> basename
    | [] -> path
  in
  (String.contains path "/tests/" || String.starts_with ~prefix:"tests/" path)
  && (String.ends_with ~suffix:"_tests.ml" basename
  || String.ends_with ~suffix:"_test.ml" basename
  || String.starts_with ~prefix:"test_" basename)

let is_test_callback_callee = fun __tmp1 ->
  match __tmp1 with
  | Some "case"
  | Some "property"
  | Some "skip"
  | Some "Test.case"
  | Some "Test.property"
  | Some "Test.skip"
  | Some "Std.Test.case"
  | Some "Std.Test.property"
  | Some "Std.Test.skip" -> true
  | _ -> false

let callback_argument_of_call = fun expr ->
  let (head, arguments) = H.flatten_apply expr in
  if not (is_test_callback_callee (H.expr_name head)) then
    None
  else
    match List.rev arguments with
    | callback :: _ -> Some callback
    | [] -> None

let is_partial_test_invocation = fun expr ->
  let (head, arguments) = H.flatten_apply expr in
  if not (is_test_callback_callee (H.expr_name head)) then
    false
  else
    Int.equal (List.length arguments) 1

let callback_argument = fun expr ->
  match callback_argument_of_call expr with
  | Some callback -> Some callback
  | None -> (
      match Ast.Expr.view (H.unwrap_expr expr) with
      | Infix { left; operator; right } when String.equal (Ast.Token.text operator) "@@" ->
          if is_partial_test_invocation left then
            Some right
          else
            None
      | _ -> None
    )

let parameter_fix = fun parameter ->
  let target = H.parameter_node parameter in
  Api.Fix.make
    ~title:"Replace unit test callback parameter with _ctx"
    ~operations:[ Api.Fix.replace_node_with_text ~target ~text:" _ctx" ]

let make_diagnostic = fun parameter ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message;
    })
    ~span:(H.parameter_span parameter)
    ~suggestion:"Change the callback parameter from () to _ctx."
    ~fix:(parameter_fix parameter)
    ()

type callback_reference =
  | Inline of Ast.Parameter.t
  | Named of string

let callback_reference = fun expr ->
  match callback_argument expr
  |> Option.map ~fn:H.unwrap_expr with
  | Some callback -> (
      match H.fun_parameters callback with
      | [ parameter ] when H.is_unit_parameter parameter -> Some (Inline parameter)
      | _ ->
          H.simple_expr_name callback
          |> Option.map ~fn:(fun name -> Named name)
    )
  | None -> None

let increment_count = fun counts key ->
  let next =
    match HashMap.get counts ~key with
    | Some count -> count + 1
    | None -> 1
  in
  ignore (HashMap.insert counts ~key ~value:next)

let binding_parameter = fun binding ->
  match H.let_binding_parameters binding with
  | [ parameter ] when H.is_unit_parameter parameter -> Some parameter
  | [] -> (
      match H.let_binding_body binding
      |> Option.map ~fn:H.unwrap_expr with
      | Some body -> (
          match H.fun_parameters body with
          | [ parameter ] when H.is_unit_parameter parameter -> Some parameter
          | _ -> None
        )
      | None -> None
    )
  | _ -> None

let check_tree = fun (ctx: Api.Rule.context) _red_root ->
  if not (is_test_source_path ctx.file_path) then
    []
  else
    (
      let expressions = Riot_fix.Rule_query.expressions ctx in
      let inline_diagnostics =
        expressions
        |> List.filter_map
          ~fn:(fun expr ->
            match callback_reference expr with
            | Some (Inline parameter) -> Some (make_diagnostic parameter)
            | Some (Named _)
            | None -> None)
      in
      let named_callback_counts = HashMap.create () in
      expressions
      |> List.for_each
        ~fn:(fun expr ->
          match callback_reference expr with
          | Some (Named name) -> increment_count named_callback_counts name
          | Some (Inline _)
          | None -> ());
      let path_use_counts = HashMap.create () in
      expressions
      |> List.for_each
        ~fn:(fun expr ->
          match H.simple_expr_name expr with
          | Some name -> increment_count path_use_counts name
          | None -> ());
      let named_diagnostics =
        Riot_fix.Rule_query.let_bindings ctx
        |> List.filter_map
          ~fn:(fun binding ->
            match H.let_binding_name binding with
            | None -> None
            | Some name ->
                let callback_uses =
                  HashMap.get named_callback_counts ~key:name
                  |> Option.unwrap_or ~default:0
                in
                let total_uses =
                  HashMap.get path_use_counts ~key:name
                  |> Option.unwrap_or ~default:0
                in
                if callback_uses = 0 || total_uses != callback_uses then
                  None
                else
                  binding_parameter binding
                  |> Option.map ~fn:make_diagnostic)
      in
      inline_diagnostics @ named_diagnostics
    )

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:explanation.message
    ~explain:explanation.body
    ~run:check_tree
    ()
