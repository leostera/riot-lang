open Std

type t =
  | Start of { mode: Runner.mode; concurrency: int }
  | FileStarted of { file: Path.t }
  | FileProgress of { file: Path.t; progress: Fixme.Source_runner.progress_event }
  | FileResult of Runner.file_result
  | Summary of { summary: Runner.summary; limit_reached: bool }

let json_object_with_type = fun type_name json ->
  let open Data.Json in
    match json with
    | Object fields -> Object (("type", String type_name) :: fields)
    | _ -> panic "expected JSON object"

let timestamp_ms = fun () ->
  Time.SystemTime.now () |> Time.SystemTime.nanos |> Int64.div 1_000_000L |> Int64.to_int

let to_json event =
  match event with
  | Start { mode; concurrency } ->
      let open Data.Json in
        Object [ ("type", String "start"); (
            "mode",
            String (
              match mode with
              | Runner.Check -> "check"
              | Runner.Apply -> "apply"
            )
          ); ("concurrency", Int concurrency); ]
  | FileStarted { file } -> let open Data.Json in Object [
    ("type", String "file_started");
    ("file", String (Path.to_string file));
    ("timestamp_ms", Int (timestamp_ms ()));
  ]
  | FileProgress { file; progress=event } ->
      let open Data.Json in
        let phase_fields =
          match event.phase with
          | Parsed { parse_diagnostics } -> [
            ("stage", String "parsed");
            ("parse_diagnostics", Int parse_diagnostics)
          ]
          | AstReady -> [ ("stage", String "ast_ready") ]
          | RuleStarted { rule_id } -> [
            ("stage", String "rule_started");
            ("rule_id", String (Rule_id.to_string rule_id))
          ]
          | RuleFinished { rule_id; diagnostics } -> [
            ("stage", String "rule_finished");
            ("rule_id", String (Rule_id.to_string rule_id));
            ("diagnostics", Int diagnostics);
          ]
        in
        Object ([
          ("type", String "progress");
          ("file", String (Path.to_string file));
          ("timestamp_ms", Int event.timestamp_ms);
        ]
        @ phase_fields)
  | FileResult result -> json_object_with_type "file" (Runner.file_result_to_json result)
  | Summary { summary; limit_reached } ->
      let open Data.Json in
        (
          match Runner.summary_to_json summary with
          | Object fields -> Object (("type", String "summary")
          :: ("limit_reached", Bool limit_reached)
          :: fields)
          | _ -> panic "expected summary JSON object"
        )
