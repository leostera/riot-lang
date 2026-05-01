open Std

module Ast = Typ.Ast
module Query = Typ.Query

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create typ query test source slice"

let parse_and_check = fun source ->
  let parse_result = Syn.parse ~filename:(Path.v "query_test.ml") (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  let source_file =
    Ast.from_parse_result ~source:model_source parse_result
    |> Result.expect ~msg:"expected typ ast build"
  in
  let infer_result = Typ.Infer.check source_file in
  Query.create ~source_file ~infer_result

let find_substring = fun source needle ->
  let source_len = String.length source in
  let needle_len = String.length needle in
  let rec loop offset =
    if offset + needle_len > source_len then
      None
    else if String.equal (String.sub source ~offset ~len:needle_len) needle then
      Some offset
    else
      loop (offset + 1)
  in
  loop 0

let span_of = fun source needle ->
  match find_substring source needle with
  | Some start -> Syn.Span.make ~start ~end_:(start + String.length needle)
  | None -> panic ("missing test substring " ^ needle)

let point_after = fun source needle ->
  match find_substring source needle with
  | Some start ->
      let offset = start + String.length needle in
      Syn.Span.make ~start:offset ~end_:offset
  | None -> panic ("missing test substring " ^ needle)

let assert_node_is_record_expression node =
  match Query.Node.kind node with
  | Query.Node.Expression { kind = Ast.Record _; _ } -> Ok ()
  | _ -> Error "expected record expression node"

let test_query_context_keeps_typed_file_and_infer_result _ctx =
  let source = {ocaml|let value = 1
|ocaml}
  in
  let context = parse_and_check source in
  match Query.source_file context with
  | Ast.Implementation { items = [ { kind = Let _; _ } ]; _ } ->
      let result: Typ.Infer.infer_result = Query.infer_result context in
      let values =
        result.intf
        |> Typ.Infer.ModuleInterface.values
        |> Iter.Iterator.to_list
      in
      if List.is_empty values then
        Error "expected query context to retain inference result"
      else
        Ok ()
  | _ -> Error "expected query context to retain typed source file"

let test_node_at_returns_leaf_expression_with_record_parents _ctx =
  let source = {ocaml|type point = { x: int; y: int }
let zero = { x = 42; y = 1 }
|ocaml}
  in
  let context = parse_and_check source in
  match Query.node_at context (span_of source "42") with
  | Some {
      Query.Node.kind = Expression { kind = Literal Int; _ };
      parent =
        Some {
          kind = RecordExpressionField field;
          parent =
            Some {
              kind = Expression { kind = Record _; _ };
              parent =
                Some {
                  kind = LetBinding _;
                  parent =
                    Some {
                      kind = LetDeclaration _;
                      parent =
                        Some {
                          kind = StructureItem _;
                          parent = Some { kind = SourceFile _; parent = None };
                        };
                    };
                };
            };
        };
    } when Typ.Model.Surface_path.to_string field.name = "x" ->
      Ok ()
  | Some _ -> Error "expected literal expression under x record field path"
  | None -> Error "expected node at integer literal"

let test_node_at_record_literal_gap_returns_record_expression _ctx =
  let source =
    {ocaml|type straw_hat = { name: string; bounty: int }
let candidate = { name = "Nami"; bounty = 6 }
|ocaml}
  in
  let context = parse_and_check source in
  match Query.node_at context (point_after source "candidate = {") with
  | Some node -> assert_node_is_record_expression node
  | None -> Error "expected record expression at record literal gap"

let test_node_at_field_access_cursor_returns_field_access_expression _ctx =
  let source =
    {ocaml|type point = { x: int; y: int }
let point_value = { x = 1; y = 2 }
let answer = (point_value).x
|ocaml}
  in
  let context = parse_and_check source in
  match Query.node_at context (point_after source "(point_value).") with
  | Some { Query.Node.kind = Expression { kind = Ast.FieldAccess access; _ }; _ } when Typ.Model.Surface_path.to_string
    access.field
  = "x" -> Ok ()
  | Some _ -> Error "expected field access expression at field cursor"
  | None -> Error "expected node at field access cursor"

let test_path_at_returns_root_to_leaf_path _ctx =
  let source = {ocaml|let value = (1, true)
|ocaml}
  in
  let context = parse_and_check source in
  let path = Query.path_at context (span_of source "true") in
  match path with
  | [
      {
        Query.Node.kind = SourceFile _;
        _;
      };
      {
        kind = StructureItem _;
        _;
      };
      {
        kind = LetDeclaration _;
        _;
      };
      {
        kind = LetBinding _;
        _;
      };
      {
        kind = Expression { kind = Tuple _; _ };
        _;
      };
      {
        kind = Expression { kind = Literal Bool; _ };
        _;
      };
    ] -> Ok ()
  | _ -> Error "expected root-to-leaf path for tuple boolean literal"

let tests =
  Test.[
    case
      "query context keeps typed file and infer result"
      test_query_context_keeps_typed_file_and_infer_result;
    case
      "node_at returns leaf expression with record parents"
      test_node_at_returns_leaf_expression_with_record_parents;
    case
      "node_at record literal gap returns record expression"
      test_node_at_record_literal_gap_returns_record_expression;
    case
      "node_at field access cursor returns field access expression"
      test_node_at_field_access_cursor_returns_field_access_expression;
    case "path_at returns root to leaf path" test_path_at_returns_root_to_leaf_path;
  ]

let main ~args = Test.Cli.main ~name:"typ:query" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
