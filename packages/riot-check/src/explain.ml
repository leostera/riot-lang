open Std

let emit = fun ?on_event event ->
  match on_event with
  | Some callback -> callback event
  | None -> ()

let run = fun ?on_event diagnostic_id ->
  match Typ.Explanations.explain diagnostic_id with
  | None -> Error (Error.UnknownDiagnosticId { diagnostic_id })
  | Some explanation ->
      emit ?on_event (Check.Event.Explanation { explanation });
      Ok ()
