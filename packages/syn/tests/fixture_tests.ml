open Std
open Std.Data
open Std.Collections
open Syn
module Iterator = Iter.Iterator

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
                  let _ = HashSet.insert modified ~value:(Path.join cwd relpath) in
                  ()
              | Error _ -> ()
            in
            loop rest
      in
      loop lines
  | Ok _ ->
      HashSet.create ()

let is_locally_modified_fixture = fun modified_fixture_paths path ->
  HashSet.contains modified_fixture_paths ~value:path

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

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create source slice: " ^ Kernel.IO.Error.message error)

let diagnostics_to_string = fun diagnostics ->
  let items = ref [] in
  Vector.iter diagnostics
  |> Iterator.for_each ~fn:(fun diagnostic -> items := Diagnostic.to_string diagnostic :: !items);
  List.reverse !items |> String.concat "\n"

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source = Fs.read ctx.fixture_path |> Result.expect ~msg:"Failed to read fixture" in
  let source = source_slice source in
  let parse_result = Syn.parse2 ~filename:ctx.fixture_path source in
  if Vector.length parse_result.Parser2.diagnostics > 0 then
    Error ("unexpected parse2 diagnostics:\n" ^ diagnostics_to_string parse_result.Parser2.diagnostics)
  else
    let root = SyntaxTree.root parse_result.Parser2.tree in
    let source_length = IO.IoVec.IoSlice.length source in
    if root.SyntaxTree.full_width = source_length then
      Ok ()
    else
      Error ("parse2 root width mismatch: expected "
      ^ Int.to_string source_length
      ^ " bytes, got "
      ^ Int.to_string root.SyntaxTree.full_width)

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

let main ~args =
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
  Test.Cli.main ~name:"syn-fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
