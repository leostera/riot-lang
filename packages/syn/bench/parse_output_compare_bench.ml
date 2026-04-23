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

let make_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create output benchmark source slice"

let load_fixture = fun name path ->
  let source = Fs.read path
  |> Result.expect ~msg:("failed to read output benchmark fixture: " ^ Path.to_string path) in
  { name; path; source; slice = make_slice source }

let is_source_file = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> true
  | _ -> false

let cst_snapshot_path = fun path ->
  Path.to_string path ^ ".expected_cst.json" |> Path.from_string |> Result.expect ~msg:"CST snapshot path should stay valid UTF-8"

let has_successful_cst_snapshot = fun path ->
  if is_source_file path then
    let snapshot_path = cst_snapshot_path path in
    match Fs.read snapshot_path with
    | Ok snapshot -> not (String.contains snapshot "\"status\": \"parse_error\"")
    | Error _ -> false
  else
    false

let parser2_accepts = fun fixture ->
  let result = parse2 ~filename:fixture.path fixture.slice in
  Vector.length result.Parser2.diagnostics = 0

let load_cst_fixture_corpus = fun () ->
  let fixtures = Vector.with_capacity ~size:1_050 in
  Fs.Walker.walk ~roots:[ Path.v "packages/syn/tests/fixtures" ] ~sort:true
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
    () |> Result.expect ~msg:"failed to walk syn CST fixture corpus";
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
  | Parse_diagnostics diagnostics -> "parse diagnostics: " ^ Int.to_string (List.length diagnostics)
  | Cst_builder_error error -> "CST builder error: "
  ^ error.CstBuilder.message
  ^ " @ "
  ^ SyntaxKind.to_string error.CstBuilder.syntax_kind

let bench_parse1_build_cst = fun fixture ->
  let result = parse ~filename:fixture.path fixture.source in
  match build_cst result with
  | Ok cst -> touch_cst cst
  | Error error -> panic
    ("parse1 + build_cst failed for "
    ^ Path.to_string fixture.path
    ^ ": "
    ^ build_cst_error_to_string error)

let touch_ast2_node_option = fun seed (node: Ast2.Node.t option) ->
  match node with
  | Some node -> checksum := !checksum lxor seed lxor node.Ast2.id
  | None -> checksum := !checksum lxor seed

let touch_ast2_token_option = fun seed (token: Ast2.Token.t option) ->
  match token with
  | Some token -> checksum := !checksum lxor seed lxor token.Ast2.id
  | None -> checksum := !checksum lxor seed

let touch_ast2_token = fun seed (token: Ast2.Token.t) ->
  checksum := !checksum lxor seed lxor token.Ast2.id

let touch_ast2_path = fun seed (path: Ast2.Path.t) ->
  checksum := !checksum lxor seed lxor path.Ast2.id;
  Ast2.Path.for_each_ident path ~fn:(fun token -> checksum := !checksum lxor token.Ast2.id)

let touch_ast2_type_expr_option = fun seed (type_expr: Ast2.TypeExpr.t option) ->
  match type_expr with
  | Some type_expr -> checksum := !checksum lxor seed lxor type_expr.Ast2.id
  | None -> checksum := !checksum lxor seed

let touch_ast2_type_expr_view = fun (type_expr: Ast2.TypeExpr.t) ->
  (
    match Ast2.TypeExpr.view type_expr with
    | Path { path } ->
        touch_ast2_path 201 path
    | Var { name } ->
        touch_ast2_token_option 202 name
    | Wildcard ->
        checksum := !checksum lxor 203
    | Arrow { left; right } ->
        touch_ast2_type_expr_option 204 left;
        touch_ast2_type_expr_option 205 right
    | Poly { body } ->
        touch_ast2_type_expr_option 215 body;
        Ast2.TypeExpr.for_each_poly_type_name type_expr ~fn:(touch_ast2_token 216)
    | Labeled { optional_token; label; annotation } ->
        touch_ast2_token_option 217 optional_token;
        touch_ast2_token_option 218 label;
        touch_ast2_type_expr_option 219 annotation
    | Tuple { left; right } ->
        touch_ast2_type_expr_option 206 left;
        touch_ast2_type_expr_option 207 right
    | Apply { argument; constructor } ->
        touch_ast2_type_expr_option 208 argument;
        touch_ast2_type_expr_option 209 constructor
    | Parenthesized { inner } ->
        touch_ast2_type_expr_option 210 inner
    | Opaque node ->
        checksum := !checksum lxor 211 lxor node.Ast2.id
    | Error node ->
        checksum := !checksum lxor 212 lxor node.Ast2.id
    | Unknown node ->
        checksum := !checksum lxor 213 lxor node.Ast2.id
  );
  Ast2.TypeExpr.for_each_child_type
    type_expr
    ~fn:(fun child -> checksum := !checksum lxor 214 lxor child.Ast2.id)

let touch_ast2_pattern_option = fun seed (pattern: Ast2.Pattern.t option) ->
  match pattern with
  | Some pattern -> checksum := !checksum lxor seed lxor pattern.Ast2.id
  | None -> checksum := !checksum lxor seed

let touch_ast2_parameter = fun (parameter: Ast2.Parameter.t) ->
  match Ast2.Parameter.view parameter with
  | Labeled { label; pattern } ->
      touch_ast2_token_option 301 label;
      touch_ast2_pattern_option 302 pattern
  | Optional { label; pattern } ->
      touch_ast2_token_option 303 label;
      touch_ast2_pattern_option 304 pattern
  | OptionalDefault { label; pattern; default } ->
      touch_ast2_token_option 305 label;
      touch_ast2_pattern_option 306 pattern;
      touch_ast2_node_option 307 default
  | Unknown node ->
      checksum := !checksum lxor 308 lxor node.Ast2.id

let touch_ast2_pattern_view = fun (pattern: Ast2.Pattern.t) ->
  (
    match Ast2.Pattern.view pattern with
    | Wildcard ->
        checksum := !checksum lxor 321
    | Path { path } ->
        touch_ast2_path 322 path
    | Apply { callee; argument } ->
        touch_ast2_pattern_option 323 callee;
        touch_ast2_pattern_option 324 argument
    | Literal { token } ->
        touch_ast2_token_option 325 token
    | Parenthesized { inner } ->
        touch_ast2_pattern_option 326 inner
    | Tuple ->
        checksum := !checksum lxor 327
    | List ->
        checksum := !checksum lxor 328
    | Array ->
        checksum := !checksum lxor 329
    | Record ->
        checksum := !checksum lxor 330
    | PolyVariant ->
        checksum := !checksum lxor 331
    | Extension ->
        checksum := !checksum lxor 332
    | Attribute { inner } ->
        touch_ast2_pattern_option 333 inner
    | LocalOpen ->
        checksum := !checksum lxor 334
    | LocallyAbstractType ->
        checksum := !checksum lxor 335
    | FirstClassModule ->
        checksum := !checksum lxor 336
    | Interval { left; right } ->
        touch_ast2_pattern_option 337 left;
        touch_ast2_pattern_option 338 right
    | Constraint { pattern; annotation } ->
        touch_ast2_pattern_option 339 pattern;
        touch_ast2_type_expr_option 340 annotation
    | Alias { pattern; alias } ->
        touch_ast2_pattern_option 341 pattern;
        touch_ast2_pattern_option 342 alias
    | Or { left; right } ->
        touch_ast2_pattern_option 343 left;
        touch_ast2_pattern_option 344 right
    | Cons { head; tail } ->
        touch_ast2_pattern_option 345 head;
        touch_ast2_pattern_option 346 tail
    | Lazy { pattern } ->
        touch_ast2_pattern_option 347 pattern
    | Exception { pattern } ->
        touch_ast2_pattern_option 348 pattern
    | LabeledParam parameter
    | OptionalParam parameter
    | OptionalParamDefault parameter ->
        touch_ast2_parameter parameter
    | Error node ->
        checksum := !checksum lxor 349 lxor node.Ast2.id
    | Unknown node ->
        checksum := !checksum lxor 350 lxor node.Ast2.id
  );
  Ast2.Pattern.for_each_child_pattern
    pattern
    ~fn:(fun child -> checksum := !checksum lxor 351 lxor child.Ast2.id)

let touch_ast2_let_binding_view = fun (binding: Ast2.LetBinding.t) ->
  let view = Ast2.LetBinding.view binding in
  touch_ast2_pattern_option 401 view.pattern;
  touch_ast2_node_option 402 view.body;
  touch_ast2_type_expr_option 403 (Ast2.LetBinding.type_annotation binding);
  Ast2.LetBinding.for_each_parameter
    binding
    ~fn:(fun parameter -> checksum := !checksum lxor 404 lxor parameter.Ast2.id)

let touch_ast2_value_declaration = fun (decl: Ast2.ValueDeclaration.t) ->
  touch_ast2_token_option 421 (Ast2.ValueDeclaration.name decl);
  touch_ast2_type_expr_option 422 (Ast2.ValueDeclaration.type_annotation decl)

let touch_ast2_external_declaration = fun (decl: Ast2.ExternalDeclaration.t) ->
  touch_ast2_token_option 431 (Ast2.ExternalDeclaration.name decl);
  touch_ast2_type_expr_option 432 (Ast2.ExternalDeclaration.type_annotation decl)

let touch_ast2_type_declaration = fun (decl: Ast2.TypeDeclaration.t) ->
  touch_ast2_token_option 441 (Ast2.TypeDeclaration.name decl);
  touch_ast2_type_expr_option 442 (Ast2.TypeDeclaration.manifest decl);
  Ast2.TypeDeclaration.for_each_parameter decl
    ~fn:(
      function
      | Ast2.TypeDeclaration.Named { name; quote; variance; injective } ->
          touch_ast2_token 443 name;
          touch_ast2_token_option 444 quote;
          touch_ast2_token_option 445 variance;
          touch_ast2_token_option 446 injective
      | Ast2.TypeDeclaration.Wildcard { wildcard; variance; injective } ->
          touch_ast2_token 447 wildcard;
          touch_ast2_token_option 448 variance;
          touch_ast2_token_option 449 injective
    )

let touch_ast2_open_declaration = fun (decl: Ast2.OpenDeclaration.t) ->
  touch_ast2_token_option 451 (Ast2.OpenDeclaration.first_path_ident decl);
  touch_ast2_token_option 452 (Ast2.OpenDeclaration.last_path_ident decl);
  Ast2.OpenDeclaration.for_each_path_ident decl ~fn:(touch_ast2_token 453)

let touch_ast2_module_declaration = fun (decl: Ast2.ModuleDeclaration.t) ->
  touch_ast2_token_option 461 (Ast2.ModuleDeclaration.name decl);
  touch_ast2_token_option 462 (Ast2.ModuleDeclaration.rec_token decl)

let rec touch_ast2_expr_view = fun (expr: Ast2.Expr.t) ->
  match Ast2.Expr.view expr with
  | Let { first_binding; body } ->
      touch_ast2_node_option 11 first_binding;
      touch_ast2_node_option 12 body
  | If { condition; then_branch; else_branch } ->
      touch_ast2_node_option 21 condition;
      touch_ast2_node_option 22 then_branch;
      touch_ast2_node_option 23 else_branch
  | Match { scrutinee; first_case } ->
      touch_ast2_node_option 31 scrutinee;
      touch_ast2_node_option 32 first_case
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
  | Path { path } ->
      checksum := !checksum lxor 81 lxor path.Ast2.id
  | Literal { token } ->
      touch_ast2_token_option 82 token
  | Tuple ->
      checksum := !checksum lxor 83
  | List ->
      checksum := !checksum lxor 84
  | Array ->
      checksum := !checksum lxor 85
  | Record ->
      checksum := !checksum lxor 86
  | Parenthesized { inner } ->
      touch_ast2_node_option 91 inner
  | Typed { expr; annotation } ->
      touch_ast2_node_option 92 expr;
      touch_ast2_type_expr_option 93 annotation
  | Error node
  | Unknown node ->
      checksum := !checksum lxor 101 lxor node.Ast2.id
  | _ ->
      checksum := !checksum lxor 109 lxor expr.Ast2.id

let rec walk_ast2_node = fun (node: Ast2.Node.t) ->
  checksum := !checksum lxor node.Ast2.id;
  (
    match Ast2.Expr.cast node with
    | Some expr -> touch_ast2_expr_view expr
    | None -> ()
  );
  (
    match Ast2.Pattern.cast node with
    | Some pattern -> touch_ast2_pattern_view pattern
    | None -> ()
  );
  (
    match Ast2.TypeExpr.cast node with
    | Some type_expr -> touch_ast2_type_expr_view type_expr
    | None -> ()
  );
  (
    match Ast2.LetBinding.cast node with
    | Some binding -> touch_ast2_let_binding_view binding
    | None -> ()
  );
  (
    match Ast2.ValueDeclaration.cast node with
    | Some decl -> touch_ast2_value_declaration decl
    | None -> ()
  );
  (
    match Ast2.TypeDeclaration.cast node with
    | Some decl -> touch_ast2_type_declaration decl
    | None -> ()
  );
  (
    match Ast2.OpenDeclaration.cast node with
    | Some decl -> touch_ast2_open_declaration decl
    | None -> ()
  );
  (
    match Ast2.ModuleDeclaration.cast node with
    | Some decl -> touch_ast2_module_declaration decl
    | None -> ()
  );
  (
    match Ast2.ExternalDeclaration.cast node with
    | Some decl -> touch_ast2_external_declaration decl
    | None -> ()
  );
  Ast2.Node.for_each_child_node node ~fn:walk_ast2_node

let touch_ast2_views = fun tree ->
  let source_file = Ast2.SourceFile.make tree in
  Ast2.SourceFile.for_each_item
    source_file
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

let make_output_case = fun ~config name fn -> Bench.make_case_with_config ~config name fn

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
      (load_fixture "multi indices" (Path.v "packages/syn/tests/fixtures/ocaml_multi_indices.ml"));
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

let benchmarks = fun () -> selected_benchmarks () @ [ corpus_benchmark () ]

let () =
  Runtime.run
    ~main:(fun ~args ->
      let result = Bench.Cli.main ~name:"syn parse output comparison" ~benchmarks:(benchmarks ()) ~args in
      if !checksum = Int.min_int then
        panic "unreachable parse output benchmark checksum";
      result)
    ~args:Env.args
    ()
