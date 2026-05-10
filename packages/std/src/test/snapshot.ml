open Global
open Collections

module Bytes = IO.Bytes

type snapshot_paths = {
  approved: Path.t;
  pending: Path.t;
}

type diff_hunk = {
  start_line: int;
  expected_count: int;
  actual_count: int;
  context_before: string list;
  expected_lines: string list;
  actual_lines: string list;
  context_after: string list;
}

let emit_snapshot_progress = fun ctx progress -> Test_context.emit_progress ctx progress

let append_path_suffix = fun path suffix ->
  Path.to_string path ^ suffix
  |> Path.from_string
  |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let is_safe_snapshot_char = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | '0' .. '9'
  | '_'
  | '-' -> true
  | _ -> false

let sanitize_path_component = fun value ->
  let lower = String.lowercase_ascii value in
  let buf = Bytes.create ~size:(String.length lower) in
  let length = ref 0 in
  let push_char ch =
    Bytes.set_unchecked buf ~at:!length ~char:ch;
    length := !length + 1
  in
  let push_dash () =
    if !length > 0 && Bytes.get_unchecked buf ~at:(!length - 1) != '-' then
      push_char '-'
  in
  String.for_each
    ~fn:(fun ch ->
      if is_safe_snapshot_char ch then
        push_char ch
      else
        push_dash ())
    lower;
  let rendered =
    Bytes.sub_unchecked buf ~offset:0 ~len:!length
    |> Bytes.to_string
  in
  let rec trim_left idx =
    if idx >= String.length rendered then
      idx
    else if String.get_unchecked rendered ~at:idx = '-' then
      trim_left (idx + 1)
    else
      idx
  in
  let rec trim_right idx =
    if idx < 0 then
      idx
    else if String.get_unchecked rendered ~at:idx = '-' then
      trim_right (idx - 1)
    else
      idx
  in
  let start_idx = trim_left 0 in
  let end_idx = trim_right (String.length rendered - 1) in
  let trimmed =
    if start_idx > end_idx then
      ""
    else
      String.sub rendered ~offset:start_idx ~len:(end_idx - start_idx + 1)
  in
  if String.equal trimmed "" then
    "snapshot"
  else
    trimmed

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some parent -> Fs.create_dir_all parent
  | None -> Ok ()

let write_pending_snapshot = fun path content ->
  match ensure_parent_dir path with
  | Error err -> Error err
  | Ok () -> Fs.write content path

let canonicalize_json =
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | Data.Json.Object fields ->
        Data.Json.Object (
          fields
          |> List.map ~fn:(fun (key, value) -> (key, loop value))
          |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
        )
    | Data.Json.Array items -> Data.Json.Array (List.map items ~fn:loop)
    | other -> other
  in
  loop

let split_lines = fun text ->
  if String.equal text "" then
    []
  else
    let parts = String.split ~by:"\n" text in
    if String.ends_with ~suffix:"\n" text then
      match List.reverse parts with
      | "" :: rest -> List.reverse rest
      | _ -> parts
    else
      parts @ [ "\\ No newline at end of file" ]

let rec take = fun count xs ->
  if count <= 0 then
    []
  else
    match xs with
    | [] -> []
    | head :: tail -> head :: take (count - 1) tail

let rec drop = fun count xs ->
  if count <= 0 then
    xs
  else
    match xs with
    | [] -> []
    | _ :: tail -> drop (count - 1) tail

let rec common_prefix_len = fun left right ->
  match (left, right) with
  | (left_head :: left_tail, right_head :: right_tail) when String.equal left_head right_head ->
      1 + common_prefix_len left_tail right_tail
  | _ -> 0

let reverse = fun items -> List.reverse items

let common_suffix_len = fun left right -> common_prefix_len (reverse left) (reverse right)

let make_diff_hunk = fun ~expected ~actual ->
  let expected_lines = split_lines expected in
  let actual_lines = split_lines actual in
  let prefix_len = common_prefix_len expected_lines actual_lines in
  let max_suffix =
    Int.min (List.length expected_lines - prefix_len) (List.length actual_lines - prefix_len)
  in
  let raw_suffix_len =
    common_suffix_len (drop prefix_len expected_lines) (drop prefix_len actual_lines)
  in
  let suffix_len = Int.min max_suffix raw_suffix_len in
  let expected_changed =
    expected_lines
    |> drop prefix_len
    |> reverse
    |> drop suffix_len
    |> reverse
  in
  let actual_changed =
    actual_lines
    |> drop prefix_len
    |> reverse
    |> drop suffix_len
    |> reverse
  in
  let context_before =
    expected_lines
    |> take prefix_len
    |> reverse
    |> take 2
    |> reverse
  in
  let context_after =
    expected_lines
    |> reverse
    |> take suffix_len
    |> reverse
    |> take 2
  in
  {
    start_line = prefix_len + 1;
    expected_count = List.length expected_changed;
    actual_count = List.length actual_changed;
    context_before;
    expected_lines = expected_changed;
    actual_lines = actual_changed;
    context_after;
  }

let ansi_reset = "\027[0m"

let ansi_dim = "\027[2m"

let ansi_red = "\027[31m"

let ansi_green = "\027[32m"

let ansi_cyan = "\027[36m"

let env_value_is_truthy = fun __tmp1 ->
  match __tmp1 with
  | None -> false
  | Some value ->
      match String.lowercase_ascii value with
      | ""
      | "0"
      | "false"
      | "no"
      | "off" -> false
      | _ -> true

let env_value_disables_color = fun __tmp1 ->
  match __tmp1 with
  | Some "0" -> true
  | _ -> false

let should_color_diff = fun () ->
  if Option.is_some (Env.get Env.String ~var:"NO_COLOR") then
    false
  else if
    env_value_is_truthy (Env.get Env.String ~var:"FORCE_COLOR")
    || env_value_is_truthy (Env.get Env.String ~var:"CLICOLOR_FORCE")
  then
    true
  else if env_value_disables_color (Env.get Env.String ~var:"CLICOLOR") then
    false
  else
    match Env.get Env.String ~var:"TERM" with
    | Some term -> not (String.equal term "dumb")
    | None -> Option.is_some (Env.get Env.String ~var:"COLORTERM")

let colorize = fun ~enabled color text ->
  if enabled then
    color ^ text ^ ansi_reset
  else
    text

let format_line = fun ?(color = None) ~color_enabled prefix line ->
  let text = prefix ^ line in
  match color with
  | Some color -> colorize ~enabled:color_enabled color text
  | None -> text

let format_diff = fun ~expected_label ~actual_label ~expected ~actual ->
  let hunk = make_diff_hunk ~expected ~actual in
  let color_enabled = should_color_diff () in
  let lines =
    ((([
      colorize ~enabled:color_enabled ansi_dim ("--- " ^ expected_label);
      colorize ~enabled:color_enabled ansi_dim ("+++ " ^ actual_label);
      colorize
        ~enabled:color_enabled
        ansi_cyan
        ("@@ -"
        ^ Int.to_string hunk.start_line
        ^ ","
        ^ Int.to_string hunk.expected_count
        ^ " +"
        ^ Int.to_string hunk.start_line
        ^ ","
        ^ Int.to_string hunk.actual_count
        ^ " @@");
    ]
    @ List.map hunk.context_before ~fn:(format_line ~color_enabled " "))
    @ List.map hunk.expected_lines ~fn:(format_line ~color_enabled ~color:(Some ansi_red) "-"))
    @ List.map hunk.actual_lines ~fn:(format_line ~color_enabled ~color:(Some ansi_green) "+"))
    @ List.map hunk.context_after ~fn:(format_line ~color_enabled " ")
  in
  String.concat "\n" lines

let resolve_paths = fun ~(ctx:Test_context.t) ->
  match ctx.fixture with
  | Some fixture ->
      let approved =
        match fixture.snapshot_path with
        | Some approved -> approved
        | None -> Path.add_extension (Path.remove_extension fixture.path) ~ext:"expected"
      in
      Ok { approved; pending = append_path_suffix approved ".new" }
  | None ->
      match (ctx.workspace_root, ctx.package_name) with
      | (Some workspace_root, Some package_name) ->
          let suite_dir = sanitize_path_component ctx.suite_name in
          let test_name = sanitize_path_component ctx.test_name in
          let base =
            Path.(workspace_root
            / Path.v ".riot"
            / Path.v "snapshots"
            / Path.v package_name
            / Path.v suite_dir
            / Path.v test_name)
          in
          let approved = Path.add_extension base ~ext:"expected" in
          Ok { approved; pending = append_path_suffix approved ".new" }
      | (None, _) ->
          Error "Snapshot assertions require ctx.workspace_root to resolve external snapshot storage."
      | (_, None) ->
          Error "Snapshot assertions require ctx.package_name to resolve external snapshot storage."

let mismatch_message = fun ~kind ~approved ~pending ~expected ~actual ->
  String.concat
    "\n"
    [
      kind;
      "Approved: " ^ Path.to_string approved;
      "Pending: " ^ Path.to_string pending;
      "";
      "Review the pending candidate with `riot snapshots review`.";
      "";
      "Diff:";
      format_diff
        ~expected_label:(Path.to_string approved)
        ~actual_label:(Path.to_string pending)
        ~expected
        ~actual;
    ]

let pending_exists_message = fun ~approved ~pending ->
  String.concat
    "\n"
    [
      "Snapshot has a pending candidate awaiting review.";
      "Approved: " ^ Path.to_string approved;
      "Pending: " ^ Path.to_string pending;
      "";
      "Use `riot snapshots review` to approve, reject, or ignore it.";
    ]

let assert_text_with_format = fun ~ctx ~format ~actual ->
  match resolve_paths ~ctx with
  | Error msg -> Error msg
  | Ok paths ->
      emit_snapshot_progress
        ctx
        (
          Test_context.SnapshotAssertionStarted {
            mode = Test_context.External;
            format;
            approved_path = Some paths.approved;
            pending_path = Some paths.pending;
          }
        );
      let pending_exists =
        Fs.exists paths.pending
        |> Result.unwrap_or ~default:false
      in
      let approved_exists =
        Fs.exists paths.approved
        |> Result.unwrap_or ~default:false
      in
      if approved_exists then
        match Fs.read paths.approved with
        | Error err -> Error (IO.error_message err)
        | Ok expected ->
            if String.equal expected actual then
              if pending_exists then (
                match write_pending_snapshot paths.pending actual with
                | Error err -> Error (IO.error_message err)
                | Ok () ->
                    emit_snapshot_progress
                      ctx
                      (
                        Test_context.SnapshotAssertionMismatch {
                          mode = Test_context.External;
                          format;
                          approved_path = Some paths.approved;
                          pending_path = Some paths.pending;
                          reason = Test_context.Pending_exists;
                        }
                      );
                    Error (pending_exists_message ~approved:paths.approved ~pending:paths.pending)
              ) else (
                emit_snapshot_progress
                  ctx
                  (Test_context.SnapshotAssertionMatched {
                    mode = Test_context.External;
                    format;
                    approved_path = Some paths.approved;
                  });
                Ok ()
              )
            else
              match write_pending_snapshot paths.pending actual with
              | Error err -> Error (IO.error_message err)
              | Ok () ->
                  emit_snapshot_progress
                    ctx
                    (
                      Test_context.SnapshotAssertionMismatch {
                        mode = Test_context.External;
                        format;
                        approved_path = Some paths.approved;
                        pending_path = Some paths.pending;
                        reason = Test_context.Mismatch;
                      }
                    );
                  Error (mismatch_message
                    ~kind:"Snapshot mismatch."
                    ~approved:paths.approved
                    ~pending:paths.pending
                    ~expected
                    ~actual)
      else
        match write_pending_snapshot paths.pending actual with
        | Error err -> Error (IO.error_message err)
        | Ok () ->
            emit_snapshot_progress
              ctx
              (
                Test_context.SnapshotAssertionMismatch {
                  mode = Test_context.External;
                  format;
                  approved_path = Some paths.approved;
                  pending_path = Some paths.pending;
                  reason = Test_context.Missing_approved;
                }
              );
            Error (String.concat
              "\n"
              [
                "Missing approved snapshot.";
                "Approved: " ^ Path.to_string paths.approved;
                "Pending: " ^ Path.to_string paths.pending;
                "";
                "Commit approved snapshots and keep `.expected.new` files reviewable until they are promoted or rejected.";
              ])

let assert_text = fun ~ctx ~actual -> assert_text_with_format ~ctx ~format:Test_context.Text ~actual

let assert_with = fun ~ctx ~render ~actual -> assert_text ~ctx ~actual:(render actual)

let assert_json = fun ~ctx ~actual ->
  let rendered =
    actual
    |> canonicalize_json
    |> Data.Json.to_string_pretty
  in
  assert_text_with_format ~ctx ~format:Test_context.Json ~actual:rendered

let assert_inline_text_with_format = fun ~ctx ~format ~actual ~expected ->
  emit_snapshot_progress
    ctx
    (
      Test_context.SnapshotAssertionStarted {
        mode = Test_context.Inline;
        format;
        approved_path = None;
        pending_path = None;
      }
    );
  if String.equal actual expected then (
    emit_snapshot_progress
      ctx
      (Test_context.SnapshotAssertionMatched {
        mode = Test_context.Inline;
        format;
        approved_path = None;
      });
    Ok ()
  ) else (
    emit_snapshot_progress
      ctx
      (
        Test_context.SnapshotAssertionMismatch {
          mode = Test_context.Inline;
          format;
          approved_path = None;
          pending_path = None;
          reason = Test_context.Mismatch;
        }
      );
    Error (String.concat
      "\n"
      [
        "Inline snapshot mismatch.";
        "";
        "Diff:";
        format_diff ~expected_label:"expected" ~actual_label:"actual" ~expected ~actual;
      ])
  )

let assert_inline_text = fun ~ctx ~actual ~expected ->
  assert_inline_text_with_format
    ~ctx
    ~format:Test_context.Text
    ~actual
    ~expected

let assert_inline_json = fun ~ctx ~actual ~expected ->
  let actual =
    actual
    |> canonicalize_json
    |> Data.Json.to_string_pretty
  in
  let expected =
    expected
    |> canonicalize_json
    |> Data.Json.to_string_pretty
  in
  assert_inline_text_with_format ~ctx ~format:Test_context.Json ~actual ~expected
