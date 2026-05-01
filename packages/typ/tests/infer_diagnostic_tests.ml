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

type source_location = {
  line: int;
  line_start: int;
  line_end: int;
  column_start: int;
  column_end: int;
  line_text: string;
}

let source_location = fun ~source (span: Syn.Span.t) ->
  let source_length = String.length source in
  let target = max 0 (min span.start source_length) in
  let rec find_line index line line_start =
    if index >= target then
      (line, line_start)
    else if Char.equal (String.get_unchecked source ~at:index) '\n' then
      find_line (index + 1) (line + 1) (index + 1)
    else
      find_line (index + 1) line line_start
  in
  let rec find_line_end index =
    if index >= source_length then
      index
    else if Char.equal (String.get_unchecked source ~at:index) '\n' then
      index
    else
      find_line_end (index + 1)
  in
  let (line, line_start) = find_line 0 1 0 in
  let line_end = find_line_end line_start in
  let line_text = String.sub source ~offset:line_start ~len:(line_end - line_start) in
  let column_start = target - line_start in
  let end_offset = max span.end_ (span.start + 1) in
  let column_end = max (column_start + 1) (min end_offset line_end - line_start) in
  { line; line_start; line_end; column_start; column_end; line_text }

let severity_to_string = fun diagnostic ->
  match Typ.Diagnostics.Diagnostic.severity diagnostic with
  | Typ.Diagnostics.Diagnostic.Error -> "error"
  | Typ.Diagnostics.Diagnostic.Warning -> "warning"

let render_diagnostic_report = fun ~path ~source diagnostic ->
  let span = Typ.Diagnostics.Diagnostic.span diagnostic in
  let location = source_location ~source span in
  let id =
    diagnostic
    |> Typ.Diagnostics.Diagnostic.id
    |> Typ.Diagnostics.Error.id_to_string
  in
  let marker_width = max 1 (location.column_end - location.column_start) in
  let line_number = Int.to_string location.line in
  let gutter = String.make ~len:(String.length line_number) ~char:' ' in
  let pointer =
    String.make ~len:location.column_start ~char:' '
    ^ String.make ~len:marker_width ~char:'^'
  in
  let fix =
    match Typ.Diagnostics.Diagnostic.fix diagnostic with
    | Some fix -> [ "  = fix: " ^ fix ]
    | None -> []
  in
  String.concat
    "\n"
    ([
      severity_to_string diagnostic
      ^ "["
      ^ id
      ^ "]: "
      ^ Typ.Diagnostics.Diagnostic.to_string diagnostic;
      " --> "
      ^ Path.to_string path
      ^ ":"
      ^ line_number
      ^ ":"
      ^ Int.to_string location.column_start
      ^ "-"
      ^ Int.to_string location.column_end;
      gutter ^ " |";
      line_number ^ " | " ^ location.line_text;
      gutter ^ " | " ^ pointer;
      "  = hint: " ^ Typ.Diagnostics.Diagnostic.hint diagnostic;
    ]
    @ fix)

let render_diagnostics ~path ~source diagnostics =
  diagnostics
  |> List.map ~fn:(render_diagnostic_report ~path ~source)
  |> String.concat "\n\n"

let render_infer_diagnostics = fun ~path ~source (diagnostics: Typ.Diagnostics.t) ->
  diagnostics.items
  |> Vector.iter
  |> Iterator.to_list
  |> render_diagnostics ~path ~source

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

let render_result = fun ~path ~source (result: Typ.Infer.infer_result) ->
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
    let diagnostics = render_infer_diagnostics ~path ~source result.diagnostics in
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

let parse_typ_ast ~filename ~report_path source =
  let parse_result = Syn.parse ~filename (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  Typ.Ast.from_parse_result ~source:model_source parse_result
  |> Result.map_err ~fn:(render_diagnostics ~path:report_path ~source)

let infer_test = fun (ctx: Test.FixtureRunner.ctx) ->
  let* source =
    Fs.read ctx.fixture_path
    |> Result.map_err ~fn:IO.error_message
  in
  let report_path =
    match ctx.test.workspace_root with
    | Some root ->
        Path.strip_prefix ctx.fixture_path ~prefix:root
        |> Result.unwrap_or ~default:ctx.fixture_path
    | None -> ctx.fixture_path
  in
  let* actual =
    match parse_typ_ast ~filename:ctx.fixture_path ~report_path source with
    | Error diagnostics -> Ok diagnostics
    | Ok ast ->
        Typ.Infer.check ast
        |> render_result ~path:report_path ~source
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
