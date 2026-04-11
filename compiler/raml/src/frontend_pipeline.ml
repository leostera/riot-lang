module Compiler_config = Config
open Std
open Std.Data

type typing_state = {
  json: Json.t;
  semantic_tree: Typ.Model.SemanticTree.file option;
  errors: Json.t list;
  is_complete: bool;
}

type t = {
  targeting: Json.t;
  source_unit: Source_unit.t;
  source: Json.t;
  typing: typing_state;
  core_ir: Core_ir.Compilation_unit.t Pipeline_stage.t;
}

let core_ir = fun pipeline -> pipeline.core_ir

let wrap_issue = fun stage diagnostic ->
  Json.obj [ ("stage", Json.string stage); ("diagnostic", diagnostic); ]

let completeness_to_json = fun completeness ->
  match completeness with
  | Typ.Model.FileSummary.Complete -> Json.string "complete"
  | Typ.Model.FileSummary.Partial -> Json.string "partial"

let env_to_json = fun env ->
  Json.array
    (env
    |> List.map
      (fun (name, scheme) ->
        Json.obj
          [
            ("name", Json.string name);
            ("scheme", Json.string (Typ.Model.TypePrinter.scheme_to_string scheme));
          ]))

let backend_to_json = Target.backend_to_json

let targeting_to_json = fun ~host ~target ->
  let backend = Target.select_backend ~host ~target in
  Json.obj
    [
      ("host", Target.to_json host);
      ("target", Target.to_json target);
      ("backend", backend_to_json backend);
    ]

let typing_state_of_parse_failure = fun parse_result error ->
  let parse_diagnostics = Syn.Parser.(parse_result.diagnostics) in
  let lowering_diagnostics =
    match error with
    | Syn.Parse_diagnostics _ -> []
    | Syn.Cst_builder_error builder_error -> [
      Typ.Model.Diagnostic.CstBuilderError { builder_error }
    ]
  in
  let errors = (parse_diagnostics |> List.map Syn.Diagnostic.to_json |> List.map (wrap_issue "parse"))
  @ (lowering_diagnostics
  |> List.map Typ.Model.Diagnostic.to_json
  |> List.map (wrap_issue "lowering")) in
  {
    json = Json.obj
      [
        ("status", Json.string "error");
        ("completeness", Json.string "partial");
        ("file_summary", Json.null);
        ("parse_diagnostics", Json.array (List.map Syn.Diagnostic.to_json parse_diagnostics));
        (
          "lowering_diagnostics",
          Json.array (List.map Typ.Model.Diagnostic.to_json lowering_diagnostics)
        );
        ("typing_diagnostics", Json.array []);
        ("exports", Json.array []);
      ];
    semantic_tree = None;
    errors;
    is_complete = false
  }

let typing_state_of_report = fun (report: Typ.Analysis.Check_result.t) ->
  let completeness = Typ.Model.FileSummary.completeness report.file_summary in
  let parse_issues = report.parse_diagnostics
  |> List.map Syn.Diagnostic.to_json
  |> List.map (wrap_issue "parse") in
  let lowering_issues = report.lowering_diagnostics
  |> List.map Typ.Model.Diagnostic.to_json
  |> List.map (wrap_issue "lowering") in
  let typing_issues = report.typing_diagnostics
  |> List.map Typ.Model.Diagnostic.to_json
  |> List.map (wrap_issue "typing") in
  let has_errors =
    report.parse_diagnostics <> []
    || report.lowering_diagnostics <> []
    || report.typing_diagnostics <> [] in
  let is_complete =
    if has_errors then
      false
    else if completeness = Typ.Model.FileSummary.Complete then
      Option.is_some report.semantic_tree
    else
      false
  in
  {
    json =
      Json.obj
        [ (
            "status",
            Json.string
              (
                if is_complete then
                  "ok"
                else
                  "error"
              )
          ); ("completeness", completeness_to_json completeness); (
            "file_summary",
            Typ.Model.FileSummary.to_json report.file_summary
          ); (
            "parse_diagnostics",
            Json.array (List.map Syn.Diagnostic.to_json report.parse_diagnostics)
          ); (
            "lowering_diagnostics",
            Json.array (List.map Typ.Model.Diagnostic.to_json report.lowering_diagnostics)
          ); (
            "typing_diagnostics",
            Json.array (List.map Typ.Model.Diagnostic.to_json report.typing_diagnostics)
          ); ("exports", env_to_json report.exports); ];
    semantic_tree =
      if is_complete then
        report.semantic_tree
      else
        None;
    errors = parse_issues @ lowering_issues @ typing_issues;
    is_complete;
  }

let typing_state = fun ~config ~filename ~source ->
  let parse_result = Syn.parse ~filename source in
  match Syn.build_cst parse_result with
  | Ok cst -> Typ.Check.check_source_with_config
    ~config:(Compiler_config.typing_config config)
    ~filename
    ~parse_result
    ~cst
  |> typing_state_of_report
  | Error error -> typing_state_of_parse_failure parse_result error

let compile_source = fun ~config ~relpath ~source ->
  Result.map
    (fun source_unit ->
      let typing = typing_state ~config ~filename:relpath ~source in
      let core_ir =
        match typing.semantic_tree with
        | None -> Pipeline_stage.blocked ~blocked_on:"typing" typing.errors
        | Some semantic_tree -> (
            match Typ_lowering.lower_file ~source_unit semantic_tree with
            | Ok compilation_unit -> Pipeline_stage.ok
              ~key:"compilation_unit"
              ~render:Core_ir.Compilation_unit.to_json
              compilation_unit
            | Error errors -> Pipeline_stage.error
              ~stage:"core_ir"
              (List.map Typ_lowering.error_to_json errors)
          )
      in
      {
        targeting = targeting_to_json
          ~host:(Compiler_config.host config)
          ~target:(Compiler_config.target config);
        source_unit;
        source = Source_unit.to_json source_unit;
        typing;
        core_ir;
      })
    (Source_unit.of_source ~relpath ~source)

let json_field_int = fun name json ->
  match Json.get_field name json with
  | Some value -> Json.get_int value
  | None -> None

let json_field_string = fun name json ->
  match Json.get_field name json with
  | Some value -> Json.get_string value
  | None -> None

let json_field_array_length = fun name json ->
  match Json.get_field name json with
  | Some value -> (
      match Json.get_array value with
      | Some items -> List.length items
      | None -> 0
    )
  | None -> 0

let emit_events = fun config ~path pipeline ->
  let unit_name = json_field_string "unit_name" pipeline.source |> Option.unwrap_or ~default:"Unknown" in
  let source_bytes = json_field_int "source_bytes" pipeline.source |> Option.unwrap_or ~default:0 in
  let completeness = json_field_string "completeness" pipeline.typing.json
  |> Option.unwrap_or ~default:"partial" in
  Compiler_config.emit_event config (fun () -> Event.SourceLoaded { path; unit_name; source_bytes });
  Compiler_config.emit_event config
    (fun () ->
      Event.TypingFinished {
        path;
        unit_name;
        completeness;
        parse_diagnostic_count = json_field_array_length "parse_diagnostics" pipeline.typing.json;
        lowering_diagnostic_count = json_field_array_length "lowering_diagnostics" pipeline.typing.json;
        typing_diagnostic_count = json_field_array_length "typing_diagnostics" pipeline.typing.json;
      });
  Compiler_config.emit_event
    config
    (fun () ->
      Event.LoweringFinished {
        path;
        backend = Event.CoreIr;
        status = Pipeline_stage.status pipeline.core_ir;
        error_count = List.length pipeline.core_ir.errors
      })
