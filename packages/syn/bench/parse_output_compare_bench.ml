open Std
open Std.Collections
open Syn

type fixture = {
  name: string;
  path: Path.t;
  source: string;
  slice: IO.IoVec.IoSlice.t;
}

let checksum = ref 0

let make_slice = fun source ->
  IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create output benchmark source slice"

let load_fixture = fun name path ->
  let source = Fs.read path |> Result.expect ~msg:("failed to read output benchmark fixture: " ^ Path.to_string path) in
  { name; path; source; slice = make_slice source }

let is_source_file = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> true
  | _ -> false

let cst_snapshot_path = fun path ->
  Path.to_string path ^ ".expected_cst.json"
  |> Path.from_string
  |> Result.expect ~msg:"CST snapshot path should stay valid UTF-8"

let has_successful_cst_snapshot = fun path ->
  if is_source_file path then
    let snapshot_path = cst_snapshot_path path in
    match Fs.read snapshot_path with
    | Ok snapshot ->
        not (String.contains snapshot "\"status\": \"parse_error\"")
    | Error _ -> false
  else
    false

let parser2_accepts = fun fixture ->
  let result = parse2 ~filename:fixture.path fixture.slice in
  Vector.length result.Parser2.diagnostics = 0

let load_cst_fixture_corpus = fun () ->
  let fixtures = Vector.with_capacity ~size:1050 in
  Fs.Walker.walk
    ~roots:[ Path.v "packages/syn/tests/fixtures" ]
    ~sort:true
    ~f:(fun item ->
      let path = Fs.Walker.FileItem.path item in
      (
        if has_successful_cst_snapshot path then
          let name = Fs.Walker.FileItem.name item in
          let fixture = load_fixture name path in
          if parser2_accepts fixture then
            Vector.push fixtures ~value:fixture
      );
      Fs.Walker.Continue)
    ()
  |> Result.expect ~msg:"failed to walk syn CST fixture corpus";
  fixtures

let touch_cst = fun cst ->
  let kind_tag =
    match Cst.SourceFile.kind cst with
    | `Implementation -> 1
    | `Interface -> 2
  in
  let item_count =
    match Cst.SourceFile.structure_items cst, Cst.SourceFile.signature_items cst with
    | Some items, _ -> List.length items
    | _, Some items -> List.length items
    | None, None -> 0
  in
  checksum := !checksum
  lxor kind_tag
  lxor item_count
  lxor List.length (Cst.SourceFile.phrase_separator_tokens cst)
  lxor List.length (Cst.SourceFile.trailing_phrase_separator_tokens cst)

let build_cst_error_to_string = function
  | Parse_diagnostics diagnostics ->
      "parse diagnostics: " ^ Int.to_string (List.length diagnostics)
  | Cst_builder_error error ->
      "CST builder error: "
      ^ error.CstBuilder.message
      ^ " @ "
      ^ SyntaxKind.to_string error.CstBuilder.syntax_kind

let bench_parse1_build_cst = fun fixture ->
  let result = parse ~filename:fixture.path fixture.source in
  match build_cst result with
  | Ok cst -> touch_cst cst
  | Error error ->
      panic ("parse1 + build_cst failed for " ^ Path.to_string fixture.path ^ ": " ^ build_cst_error_to_string error)

let touch_ast2_node_option = fun seed (node: Ast2.Node.t option) ->
  match node with
  | Some node -> checksum := !checksum lxor seed lxor node.Ast2.id
  | None -> checksum := !checksum lxor seed

let touch_ast2_token_option = fun seed (token: Ast2.Token.t option) ->
  match token with
  | Some token -> checksum := !checksum lxor seed lxor token.Ast2.id
  | None -> checksum := !checksum lxor seed

let rec touch_ast2_expr_view = fun (expr: Ast2.Expr.t) ->
  match Ast2.Expr.view expr with
  | Let { binding; body } ->
      touch_ast2_node_option 11 binding;
      touch_ast2_node_option 12 body
  | If { condition; then_branch; else_branch } ->
      touch_ast2_node_option 21 condition;
      touch_ast2_node_option 22 then_branch;
      touch_ast2_node_option 23 else_branch
  | Match { scrutinee } ->
      touch_ast2_node_option 31 scrutinee
  | Fun { body } ->
      touch_ast2_node_option 41 body
  | Apply { callee; argument } ->
      touch_ast2_node_option 51 callee;
      touch_ast2_node_option 52 argument
  | Infix { left; operator; right } ->
      touch_ast2_node_option 61 left;
      touch_ast2_token_option 62 operator;
      touch_ast2_node_option 63 right
  | Prefix { operator; operand } ->
      touch_ast2_token_option 71 operator;
      touch_ast2_node_option 72 operand
  | Path -> checksum := !checksum lxor 81
  | Literal -> checksum := !checksum lxor 82
  | Tuple -> checksum := !checksum lxor 83
  | List -> checksum := !checksum lxor 84
  | Array -> checksum := !checksum lxor 85
  | Record -> checksum := !checksum lxor 86
  | Parenthesized { inner } ->
      touch_ast2_node_option 91 inner
  | Unknown node ->
      checksum := !checksum lxor 101 lxor node.Ast2.id

let rec walk_ast2_node = fun (node: Ast2.Node.t) ->
  checksum := !checksum lxor node.Ast2.id;
  (
    match Ast2.Expr.cast node with
    | Some expr -> touch_ast2_expr_view expr
    | None -> ()
  );
  Ast2.Node.for_each_child_node node ~fn:walk_ast2_node

let touch_ast2_views = fun tree ->
  let source_file = Ast2.SourceFile.make tree in
  Ast2.SourceFile.for_each_item source_file
    ~fn:(fun item -> checksum := !checksum lxor 103 lxor item.Ast2.id);
  walk_ast2_node source_file

let bench_parse2_typed_views = fun fixture ->
  let result = parse2 ~filename:fixture.path fixture.slice in
  if Vector.length result.Parser2.diagnostics > 0 then
    panic
      ("parse2 typed views failed for "
      ^ Path.to_string fixture.path
      ^ ": "
      ^ Int.to_string (Vector.length result.Parser2.diagnostics)
      ^ " diagnostics")
  else
    touch_ast2_views result.Parser2.tree

let parse1_build_cst_corpus = fun fixtures ->
  let rec loop index =
    if index < Vector.length fixtures then
      (
        bench_parse1_build_cst (Vector.get_unchecked fixtures ~at:index);
        loop (index + 1)
      )
  in
  loop 0

let parse2_typed_views_corpus = fun fixtures ->
  let rec loop index =
    if index < Vector.length fixtures then
      (
        bench_parse2_typed_views (Vector.get_unchecked fixtures ~at:index);
        loop (index + 1)
      )
  in
  loop 0

let tiny_config: Bench.bench_config = { iterations = 1_000; warmup = 100 }

let small_config: Bench.bench_config = { iterations = 500; warmup = 50 }

let medium_config: Bench.bench_config = { iterations = 100; warmup = 10 }

let large_config: Bench.bench_config = { iterations = 6; warmup = 1 }

let corpus_config: Bench.bench_config = { iterations = 2; warmup = 1 }

let make_output_case = fun ~config name fn ->
  Bench.make_case_with_config ~config name fn

let compare_fixture = fun ~config fixture ->
  Bench.compare
    ("syn parse output: " ^ fixture.name)
    [
      make_output_case ~config "parse1 + build_cst" (fun () -> bench_parse1_build_cst fixture);
      make_output_case ~config "parse2 + typed views" (fun () -> bench_parse2_typed_views fixture);
    ]

let selected_benchmarks = fun () ->
  [
    compare_fixture
      ~config:tiny_config
      (load_fixture "tiny let binding" (Path.v "packages/syn/tests/fixtures/0001_basic.ml"));
    compare_fixture
      ~config:small_config
      (load_fixture
         "complex list ops"
         (Path.v "packages/syn/tests/fixtures/0409_complex_list_ops.ml"));
    compare_fixture
      ~config:medium_config
      (load_fixture "raw identifiers" (Path.v "packages/syn/tests/fixtures/ocaml_rawidents.ml"));
    compare_fixture
      ~config:large_config
      (load_fixture
         "multi indices"
         (Path.v "packages/syn/tests/fixtures/ocaml_multi_indices.ml"));
  ]

let corpus_benchmark = fun () ->
  let fixtures = load_cst_fixture_corpus () in
  Bench.compare
    ("syn parse output: CST fixture corpus (" ^ Int.to_string (Vector.length fixtures) ^ " files)")
    [
      make_output_case
        ~config:corpus_config
        "parse1 + build_cst"
        (fun () -> parse1_build_cst_corpus fixtures);
      make_output_case
        ~config:corpus_config
        "parse2 + typed views"
        (fun () -> parse2_typed_views_corpus fixtures);
    ]

let benchmarks = fun () ->
  selected_benchmarks () @ [ corpus_benchmark () ]

let () =
  Runtime.run
    ~main:(fun ~args ->
      let result = Bench.Cli.main ~name:"syn parse output comparison" ~benchmarks:(benchmarks ()) ~args in
      if !checksum = Int.min_int then
        panic "unreachable parse output benchmark checksum";
      result)
    ~args:Env.args
    ()
