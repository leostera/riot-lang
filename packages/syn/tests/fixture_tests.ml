open Std
open Std.Data
open Std.Collections
open Syn

let parse_result_to_json = fun result ->
  let span_to_json span = Json.Object [
    ("start", Json.Int span.Ceibo.Span.start);
    ("end", Json.Int span.Ceibo.Span.end_)
  ] in
  let span_text span =
    let width = span.Ceibo.Span.end_ - span.Ceibo.Span.start in
    if width <= 0 then
      ""
    else
      String.sub result.Parser.source ~offset:span.Ceibo.Span.start ~len:width
  in
  let trivia_kind_to_json = function
    | Token.CommentTrivia { terminated; _ } -> Json.Object [
      ("kind", Json.String "comment");
      ("terminated", Json.Bool terminated)
    ]
    | Token.DocstringTrivia { terminated; _ } -> Json.Object [
      ("kind", Json.String "docstring");
      ("terminated", Json.Bool terminated)
    ]
    | Token.WhitespaceTrivia -> Json.Object [ ("kind", Json.String "whitespace") ]
  in
  let trivia_to_json (trivia: Token.trivia) =
    match trivia_kind_to_json trivia.kind with
    | Json.Object fields -> Json.Object ([
      ("span", span_to_json trivia.span);
      ("text", Json.String (span_text trivia.span))
    ]
    @ fields)
    | json -> json
  in
  let token_to_json (token: Token.t) = Json.Object [
    ("kind", Json.String (Token.show_kind token.kind));
    ("span", span_to_json token.span);
    ("text", Json.String (span_text token.span));
    ("leading_trivia", Json.Array (List.map token.leading_trivia ~fn:trivia_to_json))
  ] in
  let kind_to_json kind = Json.String (SyntaxKind.to_string kind) in
  let text_to_json text = Json.String text in
  let tree_json = Ceibo.Green.to_json
    ~kind_to_json
    ~text_to_json
    (Ceibo.Green.Node result.Parser.tree) in
  Json.Object [
    ("tokens", Json.Array (List.map result.Parser.tokens ~fn:token_to_json));
    ("tree", tree_json);
    ("diagnostics", Json.Array (List.map result.Parser.diagnostics ~fn:Diagnostic.to_json))
  ]

let append_path_suffix = fun path suffix ->
  Path.to_string path ^ suffix |> Path.from_string |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let fixture_root = Path.v "packages/syn/tests/fixtures"

let lossless_snapshot_path = fun path -> append_path_suffix path ".expected_lossless.json"

let load_modified_fixture_paths = fun () ->
  let cwd = Env.current_dir () |> Result.expect ~msg:"failed to get cwd for fixture filter" in
  let args = [ "diff"; "--name-only"; "--"; Path.to_string fixture_root ] in
  match Command.make "git" ~args |> Command.output with
  | Error _ ->
      HashSet.create ()
  | Ok { status; stdout; _ } when status = 0 ->
      let modified = HashSet.create () in
      let lines = stdout |> String.split_on_char '\n' |> List.map ~fn:String.trim in
      let rec loop = function
        | [] ->
            modified
        | "" :: rest ->
            loop rest
        | relpath :: rest ->
            let () =
              match Path.from_string relpath with
              | Ok relpath ->
                  let _ = HashSet.insert modified (Path.join cwd relpath) in
                  ()
              | Error _ -> ()
            in
            loop rest
      in
      loop lines
  | Ok _ ->
      HashSet.create ()

let is_locally_modified_fixture = fun modified_fixture_paths path ->
  HashSet.contains modified_fixture_paths path

let has_lossless_snapshot = fun modified_fixture_paths path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" ->
      if is_locally_modified_fixture modified_fixture_paths path then
        `skip
      else
        let snapshot_path = lossless_snapshot_path path in
        let exists = Fs.exists snapshot_path |> Result.unwrap_or ~default:false in
        if exists then
          `keep
        else
          `skip
  | _ -> `skip

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"Failed to read fixture" in
  let parse_result = Syn.parse ~filename:ctx.fixture_path source in
  let actual_json = parse_result_to_json parse_result in
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun json -> Json.to_string_pretty json ^ "\n")
    ~actual:actual_json

let test_tagged_quoted_string_cst = fun _ctx ->
  let source = "let explanation = {explain|hello|explain}\n" in
  let parse_result = Syn.parse ~filename:(Path.v "tagged_quoted_string.ml") source in
  if List.length parse_result.Parser.diagnostics > 0 then
    let diagnostics = parse_result.Parser.diagnostics
    |> List.map ~fn:Diagnostic.to_string
    |> String.concat "\n" in
    Error ("unexpected parse diagnostics:\n" ^ diagnostics)
  else
    match Syn.build_cst parse_result with
    | Ok cst -> (
        match cst with
        | Syn.Cst.Implementation {
          items=Syn.Cst.StructureItem.LetBinding {
            value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.String {
              delimiter=Syn.Cst.Quoted { marker };
              contents;
              terminated;
              _
            });
            _
          } :: _;
          _
        } ->
            if String.equal marker "explain" && String.equal contents "hello" && terminated then
              Ok ()
            else
              Error ("unexpected tagged string CST payload: marker="
              ^ marker
              ^ ", contents="
              ^ contents
              ^ ", terminated="
              ^ Bool.to_string terminated)
        | _ -> Error "unexpected CST shape for tagged quoted string literal"
      )
    | Error (Syn.Cst_builder_error err) ->
        Error ("expected CST builder to succeed, got "
        ^ err.Syn.CstBuilder.message
        ^ " @ "
        ^ Syn.SyntaxKind.to_string err.Syn.CstBuilder.syntax_kind
        ^ " in "
        ^ String.concat " > " err.Syn.CstBuilder.context)
    | Error (Syn.Parse_diagnostics diagnostics) ->
        let diagnostics = diagnostics |> List.map ~fn:Diagnostic.to_string |> String.concat "\n" in
        Error ("unexpected build_cst parse diagnostics:\n" ^ diagnostics)

let () =
  Actors.run
    ~main:(fun ~args ->
      let modified_fixture_paths = load_modified_fixture_paths () in
      let fixture_tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixture_root
          ~filter:(has_lossless_snapshot modified_fixture_paths)
          ~snapshot_path:(fun path -> Some (lossless_snapshot_path path))
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      let tests = Test.case "tagged_quoted_string_cst" test_tagged_quoted_string_cst :: fixture_tests in
      Test.Cli.main ~name:"syn-fixtures" ~tests ~args)
    ~args:Env.args
    ()
