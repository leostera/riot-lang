open Std

module Api = Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":prefer-result-map-over-manual-match"

let explanation =
  Api.Explanation.
    {
      rule_id = package_rule_id;
      message = "Manual result matches that only rebuild Ok/Error should use Result.map.";
      body =
        {|
`Result.map` already names the pattern where you transform the `Ok` value and leave the
`Error` branch alone.

Writing that out manually with:

- `Ok x -> Ok (...)`
- `Error e -> Error e`

is usually just a longer spelling of the same combinator. `Result.map` makes the intent
clear immediately: success is transformed, failure is preserved.

This rule only targets the narrow case where the error branch is forwarded unchanged and
the success branch rebuilds `Ok`.
|};
    }

let explanations () = [ explanation ]

let rec unwrap_expression expr =
  match expr with
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      unwrap_expression inner
  | Syn.Cst.Expression.Typed { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ }
  | Syn.Cst.Expression.Coerce { expression; _ } ->
      unwrap_expression expression
  | _ ->
      expr

let constructor_name_of_ident ident = Syn.Cst.Ident.name ident

let identifier_name_of_pattern = function
  | Syn.Cst.Pattern.Identifier { name_token; _ } ->
      Some (Syn.Cst.Token.text name_token)
  | _ ->
      None

let result_case_kind (case : Syn.Cst.match_case) =
  if Option.is_some case.guard then
    `Other
  else
    match case.pattern with
    | Syn.Cst.Pattern.Constructor
        {
          constructor_path;
          arguments = [ argument_pattern ];
          _;
        } -> (
            match constructor_name_of_ident constructor_path, identifier_name_of_pattern argument_pattern with
            | Some "Ok", Some name ->
                `OkCase name
            | Some "Error", Some name ->
                `ErrorCase name
            | _ ->
                `Other)
    | _ ->
        `Other

let is_constructor_with_path_name expected name expr =
  match unwrap_expression expr with
  | Syn.Cst.Expression.Constructor
      {
        constructor_path;
        payload = Some payload;
        _;
      } -> (
          match constructor_name_of_ident constructor_path, unwrap_expression payload with
          | Some ctor_name, Syn.Cst.Expression.Path { path; _ } ->
              String.equal ctor_name expected
              &&
              match Syn.Cst.Ident.name path with
              | Some path_name ->
                  String.equal path_name name
              | None ->
                  false
          | _ ->
              false)
  | _ ->
      false

let is_ok_expression expr =
  match unwrap_expression expr with
  | Syn.Cst.Expression.Constructor { constructor_path; payload = Some _; _ } -> (
      match constructor_name_of_ident constructor_path with
      | Some "Ok" ->
          true
      | _ ->
          false)
  | _ ->
      false

let matches_result_map_shape (expr : Syn.Cst.match_expression) =
  match expr.cases with
  | [ first_case; second_case ] -> (
      match result_case_kind first_case, result_case_kind second_case with
      | `OkCase _ok_name, `ErrorCase error_name ->
          is_ok_expression first_case.body
          && is_constructor_with_path_name "Error" error_name second_case.body
      | `ErrorCase error_name, `OkCase _ok_name ->
          is_constructor_with_path_name "Error" error_name first_case.body
          && is_ok_expression second_case.body
      | _ ->
          false)
  | _ ->
      false

let make_diagnostic (expr : Syn.Cst.match_expression) =
  Api.Diagnostic.make ~severity:Warning
    ~kind:
      (Api.Diagnostic.Known
         {
           rule_id = package_rule_id;
           message = explanation.Api.Explanation.message;
         })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Prefer Result.map when the Ok branch changes and Error is forwarded unchanged."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Match expr when matches_result_map_shape expr ->
      Some (make_diagnostic expr)
  | _ ->
      None

let check_tree (ctx : Api.Rule.context) _red_root =
  match ctx.cst with
  | None ->
      []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Api.Traversal.expressions_of_structure_item
      |> List.filter_map diagnostic_for_expression

let rule () =
  Api.Rule.make ~id:package_rule_id
    ~description:explanation.message
    ~explain:explanation.body ~run:check_tree ()
