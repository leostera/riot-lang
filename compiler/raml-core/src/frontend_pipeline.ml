module Compiler_config = Config
open Std
open Std.Data

type typing_state = {
  json: Json.t;
  semantic_tree: unit option;
  parse_diagnostics: Syn.Diagnostic.t list;
  lowering_diagnostics: Typ.Diagnostics.Diagnostic.t list;
  typing_diagnostics: Typ.Diagnostics.Diagnostic.t list;
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
  Json.obj [ ("stage", Json.string stage); ("diagnostic", diagnostic) ]

let span_to_json = fun (span: Syn.Span.t) ->
  Json.obj [ ("start", Json.int span.start); ("end", Json.int span.end_) ]

let diagnostic_to_json = fun diagnostic ->
  match diagnostic with
  | Typ.Diagnostics.Diagnostic.UnsupportedSyntax value -> Json.obj
    [
      ("tag", Json.string "unsupported_syntax");
      ("span", span_to_json value.span);
      ("kind", Json.string (Syn.SyntaxKind.to_string value.kind));
      ("summary", Json.string value.summary);
    ]
  | Typ.Diagnostics.Diagnostic.UnsupportedType value -> Json.obj
    [
      ("tag", Json.string "unsupported_type");
      ("span", span_to_json value.span);
      ("summary", Json.string value.summary);
    ]

let cst_builder_error_to_diagnostic = fun (error: Syn.CstBuilder.error) ->
  Typ.Diagnostics.Diagnostic.UnsupportedSyntax {
    span = error.span;
    kind = error.syntax_kind;
    summary = "Unsupported syntax (" ^ Syn.SyntaxKind.to_string error.syntax_kind ^ ") " ^ error.message
  }

let completeness_to_json = fun is_complete ->
  Json.string
    (
      if is_complete then
        "complete"
      else
        "partial"
    )

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
    | Syn.Cst_builder_error builder_error -> [ cst_builder_error_to_diagnostic builder_error ]
  in
  let errors = (parse_diagnostics
  |> List.map ~fn:Syn.Diagnostic.to_json
  |> List.map ~fn:(wrap_issue "parse"))
  @ (lowering_diagnostics |> List.map ~fn:diagnostic_to_json |> List.map ~fn:(wrap_issue "lowering")) in
  {
    json = Json.obj
      [
        ("status", Json.string "error");
        ("completeness", completeness_to_json false);
        ("file_summary", Json.string "partial");
        ("parse_diagnostics", Json.array (List.map parse_diagnostics ~fn:Syn.Diagnostic.to_json));
        ("lowering_diagnostics", Json.array (List.map lowering_diagnostics ~fn:diagnostic_to_json));
        ("typing_diagnostics", Json.array []);
        ("exports", Json.array []);
      ];
    semantic_tree = None;
    parse_diagnostics;
    lowering_diagnostics;
    typing_diagnostics = [];
    errors;
    is_complete = false;
  }

let typing_state_of_report = fun (report: Typ.Analysis.Check_result.t) ~parse_diagnostics ~source:_ ~cst:_ ->
  let parse_issues = parse_diagnostics
  |> List.map ~fn:Syn.Diagnostic.to_json
  |> List.map ~fn:(wrap_issue "parse") in
  let lowering_diagnostics = [] in
  let lowering_issues = [] in
  let typing_issues = report.diagnostics
  |> List.map ~fn:diagnostic_to_json
  |> List.map ~fn:(wrap_issue "typing") in
  let has_errors = not (List.is_empty parse_diagnostics) || not (List.is_empty report.diagnostics) in
  let is_complete = not has_errors in
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
          ); ("completeness", completeness_to_json is_complete); (
            "file_summary",
            Json.string
              (
                if is_complete then
                  "complete"
                else
                  "partial"
              )
          ); (
            "parse_diagnostics",
            Json.array (List.map parse_diagnostics ~fn:Syn.Diagnostic.to_json)
          ); ("lowering_diagnostics", Json.array []); (
            "typing_diagnostics",
            Json.array (List.map report.diagnostics ~fn:diagnostic_to_json)
          ); ("exports", Json.array []); ];
    semantic_tree =
      if is_complete then
        Some ()
      else
        None;
    parse_diagnostics;
    lowering_diagnostics;
    typing_diagnostics = report.diagnostics;
    errors = parse_issues @ lowering_issues @ typing_issues;
    is_complete;
  }

let typing_state = fun ~config:_ ~filename ~source ->
  let parse_result = Syn.parse ~filename source in
  match Syn.build_cst parse_result with
  | Ok cst ->
      let source_value = Typ.Model.Source.make ~text:source in
      let report = Typ.Check.check ~source:source_value cst in
      typing_state_of_report
        report
        ~parse_diagnostics:(Syn.Parser.(parse_result.diagnostics))
        ~source:source_value
        ~cst
  | Error error -> typing_state_of_parse_failure parse_result error

let compile_source = fun ~config ~relpath ~source ->
  Result.map (Source_unit.from_source ~relpath ~source)
    ~fn:(fun source_unit ->
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
              (List.map errors ~fn:Typ_lowering.error_to_json)
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
  let source_bytes = json_field_int "source_bytes" pipeline.typing.json
  |> Option.unwrap_or ~default:0 in
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
