open Std

type event =
  | Start of { total_files : int; concurrency : int }
  | File of Runner.file_result
  | Summary of Runner.summary

let relative_to_root ~root path =
  match Path.strip_prefix path ~prefix:root with
  | Ok rel -> Path.to_string rel
  | Error _ -> Path.to_string path

let file_result_to_json ~root (result : Runner.file_result) =
  let open Data.Json in
  let status =
    match result.error, result.needs_formatting with
    | Some _, _ -> "failed"
    | None, true -> "needs_formatting"
    | None, false -> "already_formatted"
  in
  Object
    [
      ("file", String (relative_to_root ~root result.file));
      ("status", String status);
      ("needs_formatting", Bool result.needs_formatting);
      ("error", match result.error with Some error -> String error | None -> Null);
    ]

let summary_to_json (summary : Runner.summary) =
  let open Data.Json in
  Object
    [
      ("total_files", Int summary.total_files);
      ("already_formatted", Int summary.already_formatted);
      ("needs_formatting", Int summary.needs_formatting);
      ("failed_files", Int summary.failed_files);
      ("duration_secs", Float (Time.Duration.to_secs_float summary.duration));
    ]

let event_to_json ~root = function
  | Start { total_files; concurrency } ->
      Data.Json.Object
        [
          ("type", Data.Json.String "start");
          ("total_files", Data.Json.Int total_files);
          ("concurrency", Data.Json.Int concurrency);
        ]
  | File result -> (
      match file_result_to_json ~root result with
      | Data.Json.Object fields ->
          Data.Json.Object (("type", Data.Json.String "file") :: fields)
      | _ -> panic "expected JSON object")
  | Summary summary -> (
      match summary_to_json summary with
      | Data.Json.Object fields ->
          Data.Json.Object (("type", Data.Json.String "summary") :: fields)
      | _ -> panic "expected JSON object")

let write_line ~writer line = IO.write_all writer ~buf:(line ^ "\n")

let write_text_file_result ~writer ~root (result : Runner.file_result) =
  let status_char, suffix =
    match result.error, result.needs_formatting with
    | Some error, _ -> ("\027[1;31m✗\027[0m", ": " ^ error)
    | None, true -> ("\027[1;33m!\027[0m", " (needs formatting)")
    | None, false -> ("\027[1;32m✓\027[0m", " (already formatted)")
  in
  let path = relative_to_root ~root result.file in
  write_line ~writer (status_char ^ " " ^ path ^ suffix)

let write_text_summary ~writer (summary : Runner.summary) =
  let status_char =
    if summary.needs_formatting = 0 && summary.failed_files = 0 then
      "\027[1;32m✓\027[0m"
    else
      "\027[1;31m✗\027[0m"
  in
  let duration = Time.Duration.to_secs_string ~precision:2 summary.duration in
  let line =
    status_char ^ " Checked " ^ Int.to_string summary.total_files ^ " files in "
    ^ duration ^ "s (" ^ Int.to_string summary.already_formatted
    ^ " already formatted, " ^ Int.to_string summary.needs_formatting
    ^ " need formatting, " ^ Int.to_string summary.failed_files ^ " failed)"
  in
  write_line ~writer line

let write_json_event ~writer ~root event =
  write_line ~writer (Data.Json.to_string (event_to_json ~root event))
