open Std

module Typ_check_result = Typ.Analysis.Check_result
module Typ_diagnostic = Typ.Model.Diagnostic

type t =
  | Parse of Syn.Diagnostic.t
  | Lowering of Typ_diagnostic.t
  | Typing of Typ_diagnostic.t

let from_report = fun (report: Typ_check_result.t) ->
  List.concat
    [
      report.parse_diagnostics
      |> List.map (fun diagnostic -> Parse diagnostic);
      report.lowering_diagnostics
      |> List.map (fun diagnostic -> Lowering diagnostic);
      report.typing_diagnostics
      |> List.map (fun diagnostic -> Typing diagnostic);
    ]

let has_errors = fun diagnostics ->
  List.exists
    (
      function
      | Parse _ ->
          true
      | Lowering diagnostic
      | Typing diagnostic -> (
          match Typ_diagnostic.severity diagnostic with
          | Typ_diagnostic.Error -> true
          | Typ_diagnostic.Warning -> false
        )
    )
    diagnostics

let has_warning_diagnostic = function
  | Parse _ -> false
  | Lowering diagnostic
  | Typing diagnostic -> Typ_diagnostic.severity diagnostic = Typ_diagnostic.Warning

let has_warnings = fun diagnostics -> List.exists has_warning_diagnostic diagnostics

let severity = function
  | Parse _ -> "error"
  | Lowering diagnostic
  | Typing diagnostic -> Typ_diagnostic.severity_to_string (Typ_diagnostic.severity diagnostic)

let code = function
  | Parse diagnostic -> Syn.Diagnostic.id diagnostic
  | Lowering diagnostic
  | Typing diagnostic -> Typ_diagnostic.code diagnostic

let message = function
  | Parse diagnostic ->
      let expected = Syn.Diagnostic.expected_message diagnostic in
      let main_message = Syn.Diagnostic.main_message diagnostic in
      if String.length expected > 0 then
        main_message ^ " (expected " ^ expected ^ ")"
      else
        main_message
  | Lowering diagnostic
  | Typing diagnostic -> Typ_diagnostic.message diagnostic

let phase = function
  | Parse _ -> "parse"
  | Lowering _ -> "lowering"
  | Typing _ -> "typing"

let source = function
  | Parse _ -> "syn"
  | Lowering _
  | Typing _ -> "typ"

let fix = function
  | Parse diagnostic -> Syn.Diagnostic.fix_message diagnostic
  | _ -> None

let expected = function
  | Parse diagnostic -> (
      let expected = Syn.Diagnostic.expected_message diagnostic in
      if String.length expected = 0 then
        None
      else
        Some ("expected " ^ expected)
    )
  | _ ->
      None

let data = function
  | Parse diagnostic -> Syn.Diagnostic.to_json diagnostic
  | Lowering diagnostic
  | Typing diagnostic -> Typ_diagnostic.to_json diagnostic

let span = function
  | Parse diagnostic -> diagnostic.Syn.Diagnostic.span
  | Lowering diagnostic
  | Typing diagnostic -> Typ_diagnostic.primary_span diagnostic

let span_to_json = fun (span: Syn.Span.t) ->
  Data.Json.Object [ ("start", Data.Json.Int span.start); ("end", Data.Json.Int span.end_); ]

let to_json = fun diagnostic ->
  Data.Json.Object [
    ("phase", Data.Json.String (phase diagnostic));
    ("source", Data.Json.String (source diagnostic));
    ("severity", Data.Json.String (severity diagnostic));
    ("code", Data.Json.String (code diagnostic));
    ("message", Data.Json.String (message diagnostic));
    ("span", span_to_json (span diagnostic));
    ("data", data diagnostic);
  ]
