open Std

type event =
  | Start of { mode : Runner.run_mode; concurrency : int; }
  | File of Runner.file_result
  | Summary of Runner.summary

let relative_to_root = fun ~root path ->
  match Path.strip_prefix path ~prefix:root with
  | Ok rel -> Path.to_string rel
  | Error _ -> Path.to_string path

let file_result_to_json = fun ~root (result:Runner.file_result) ->
  let open Data.Json in
    let status =
      match result.status with
      | Runner.Already_formatted -> "already_formatted"
      | Runner.Needs_formatting -> "needs_formatting"
      | Runner.Would_reformat -> "would_reformat"
      | Runner.Unsafe_to_format -> "unsafe_to_format"
      | Runner.Formatted -> "formatted"
      | Runner.Failed -> "failed"
    in
    Object [
      ("file", String (relative_to_root ~root result.file));
      ("status", String status);
      ("needs_formatting", Bool result.needs_formatting);
      ("duration_ms", Int (Time.Duration.to_millis result.duration));
      (
        "error",
        match result.error with
        | Some error -> String error
        | None -> Null
      );

    ]

let summary_to_json = fun (summary:Runner.summary) ->
  let open Data.Json in Object [
    ("total_files", Int summary.total_files);
    ("already_formatted", Int summary.already_formatted);
    ("needs_formatting", Int summary.needs_formatting);
    ("would_reformat", Int summary.would_reformat);
    ("unsafe_to_format", Int summary.unsafe_to_format);
    ("formatted_files", Int summary.formatted_files);
    ("failed_files", Int summary.failed_files);
    ("duration_secs", Float (Time.Duration.to_secs_float summary.duration));

  ]

let timestamp_field = fun () ->
  ("timestamp", Data.Json.String (Datetime.now_utc () |> Datetime.to_iso8601))

let event_to_json = fun ~root ->
  function
  | Start { mode; concurrency } ->
      Data.Json.Object [ timestamp_field (); ("type", Data.Json.String "start"); (
          "mode",
          Data.Json.String (
            match mode with
            | Runner.Check -> "check"
            | Runner.Verify -> "verify"
            | Runner.Format -> "format"
          )
        ); ("concurrency", Data.Json.Int concurrency);  ]
  | File result -> (
      match file_result_to_json ~root result with
      | Data.Json.Object fields -> Data.Json.Object (timestamp_field ()
      :: ("type", Data.Json.String "file")
      :: fields)
      | _ -> panic "expected JSON object"
    )
  | Summary summary -> (
      match summary_to_json summary with
      | Data.Json.Object fields -> Data.Json.Object (timestamp_field ()
      :: ("type", Data.Json.String "summary")
      :: fields)
      | _ -> panic "expected JSON object"
    )

let write_line = fun ~writer line -> IO.write_all writer ~buf:((line ^ "\n"))

let write_text_file_result = fun ~writer ~root (result:Runner.file_result) ->
  let status_char, suffix =
    match result.status, result.error with
    | Runner.Failed, Some error -> ("\027[1;31m✗\027[0m", ": " ^ error)
    | Runner.Failed, None -> ("\027[1;31m✗\027[0m", " (failed)")
    | Runner.Already_formatted, _ -> ("\027[1;32m✓\027[0m", " (already formatted)")
    | Runner.Needs_formatting, _ -> ("\027[1;33m!\027[0m", " (needs formatting)")
    | Runner.Would_reformat, _ -> ("\027[1;32m✓\027[0m", " (would reformat safely)")
    | Runner.Formatted, _ -> ("\027[1;32m✓\027[0m", " (formatted)")
    | Runner.Unsafe_to_format, Some error -> (
      "\027[1;31m✗\027[0m",
      " (unsafe to format: " ^ error ^ ")"
    )
    | Runner.Unsafe_to_format, None -> ("\027[1;31m✗\027[0m", " (unsafe to format)")
  in
  let path = relative_to_root ~root result.file in
  write_line ~writer (status_char ^ " " ^ path ^ suffix)

let write_text_summary = fun ~writer ~mode (summary:Runner.summary) ->
  let status_char =
    if match mode with
      | Runner.Check -> summary.needs_formatting = 0 && summary.failed_files = 0
      | Runner.Verify -> summary.unsafe_to_format = 0 && summary.failed_files = 0
      | Runner.Format -> summary.failed_files = 0 then
      "\027[1;32m✓\027[0m"
    else
      "\027[1;31m✗\027[0m"
  in
  let duration = Time.Duration.to_secs_string ~precision:2 summary.duration in
  let verb =
    match mode with
    | Runner.Check -> "Checked"
    | Runner.Verify -> "Verified"
    | Runner.Format -> "Formatted"
  in
  let line =
    status_char
    ^ " "
    ^ verb
    ^ " "
    ^ Int.to_string summary.total_files
    ^ " files in "
    ^ duration
    ^ "s ("
    ^ Int.to_string summary.already_formatted
    ^ " already formatted, "
    ^ Int.to_string summary.needs_formatting
    ^ " need formatting, "
    ^ Int.to_string summary.would_reformat
    ^ " would reformat safely, "
    ^ Int.to_string summary.unsafe_to_format
    ^ " unsafe to format, "
    ^ Int.to_string summary.formatted_files
    ^ " formatted, "
    ^ Int.to_string summary.failed_files
    ^ " failed)"
  in
  write_line ~writer line

let write_json_event = fun ~writer ~root event ->
  write_line ~writer (Data.Json.to_string (event_to_json ~root event))
