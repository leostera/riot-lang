open Std
open Std.Collections
open Std.Iter
open Std.Result.Syntax

let name = "typ:infer-diagnostics"

let fixtures_dir = Path.v "packages/typ/tests/fixtures/diagnostics"

let infer_snapshot_path = fun path -> Path.add_extension path ~ext:"infer.expected"

let pending_infer_snapshot_path = fun path -> Path.add_extension path ~ext:"infer.expected.new"

let has_infer_snapshot = fun path ->
  Fs.exists (infer_snapshot_path path)
  |> Result.unwrap_or ~default:false

let has_pending_infer_snapshot = fun path ->
  Fs.exists (pending_infer_snapshot_path path)
  |> Result.unwrap_or ~default:false

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" when has_infer_snapshot path && not (has_pending_infer_snapshot path) ->
      Test.FixtureRunner.Keep
  | _ -> Test.FixtureRunner.Skip

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create typ diagnostic test source slice"

let render_diagnostics diagnostics =
  diagnostics
  |> List.map ~fn:Typ.Diagnostics.Diagnostic.to_string
  |> String.concat "\n"

let render_infer_diagnostics = fun (diagnostics: Typ.Diagnostics.t) ->
  diagnostics.items
  |> Vector.iter
  |> Iterator.to_list
  |> render_diagnostics

let render_interface = fun (intf: Typ.Infer.ModuleInterface.t) ->
  Typ.SignatureGenerator.from_exports
    ~types:(Typ.Infer.ModuleInterface.types intf)
    ~values:(
      intf
      |> Typ.Infer.ModuleInterface.values
      |> Iterator.map
        ~fn:(fun (name, (type_scheme: Typ.Infer.TypeScheme.t)) -> (name, type_scheme.body))
    )

type rendered_result =
  | Interface of string
  | Diagnostics of string

let render_result = fun (result: Typ.Infer.infer_result) ->
  if Vector.is_empty result.diagnostics.items then (
    let intf = render_interface result.intf in
    let suffix =
      if String.equal intf "" then
        ""
      else
        "\n\nInferred interface:\n" ^ intf
    in
    Diagnostics ("No Typ diagnostics emitted." ^ suffix)
  ) else
    let diagnostics = render_infer_diagnostics result.diagnostics in
    Diagnostics diagnostics

let validate_interface_source = fun source ->
  let parse_result =
    source
    |> source_slice
    |> Syn.parse_interface
  in
  if Vector.is_empty parse_result.diagnostics then
    Ok source
  else
    Error "generated interface did not parse cleanly"

let ensure_trailing_newline = fun text ->
  if String.ends_with ~suffix:"\n" text then
    text
  else
    text ^ "\n"

let parse_typ_ast ~path source =
  let parse_result = Syn.parse ~filename:path (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  Typ.Ast.from_parse_result ~source:model_source parse_result
  |> Result.map_err ~fn:render_diagnostics

let infer_test = fun (ctx: Test.FixtureRunner.ctx) ->
  let* source =
    Fs.read ctx.fixture_path
    |> Result.map_err ~fn:IO.error_message
  in
  let* actual =
    match parse_typ_ast ~path:ctx.fixture_path source with
    | Error diagnostics -> Ok diagnostics
    | Ok ast ->
        Typ.Infer.check ast
        |> render_result
        |> function
          | Interface source -> validate_interface_source source
          | Diagnostics diagnostics -> Ok diagnostics
  in
  Test.Snapshot.assert_text ~ctx:ctx.test ~actual:(ensure_trailing_newline actual)

let tests =
  Test.FixtureRunner.cases
    ()
    ~dir:fixtures_dir
    ~filter:fixture_filter
    ~snapshot_path:(fun path -> Some (infer_snapshot_path path))
    ~run:infer_test

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
