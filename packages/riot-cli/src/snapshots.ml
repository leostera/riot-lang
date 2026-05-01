open Std
open Collections

type pending_snapshot = {
  approved: Path.t;
  pending: Path.t;
}

type review_decision = [`Approve | `Reject | `Ignore | `Quit]

type review_summary = { approved_count: int; rejected_count: int; ignored_count: int; quit: bool }

type 'value scan_step =
  | Continue of 'value
  | Stop of 'value

let empty_review_summary = {
  approved_count = 0;
  rejected_count = 0;
  ignored_count = 0;
  quit = false;
}

let ( let* ) value fn = Result.and_then value ~fn

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  let make_subcommand name about_text =
    command name
    |> about about_text
    |> args
      [
        positional "query"
        |> required false
        |> help "Optional path substring used to filter pending snapshots";
      ]
  in
  command "snapshots"
  |> about "Review and manage pending snapshot candidates"
  |> subcommands
    [
      make_subcommand "review" "Interactively review pending snapshot diffs";
      make_subcommand "approve" "Promote pending snapshots to approved files";
      make_subcommand "reject" "Delete pending snapshot candidates";
    ]

let ensure_trailing_new_removed = fun path ->
  let rendered = Path.to_string path in
  if String.ends_with ~suffix:".new" rendered then
    let len = String.length rendered - 4 in
    String.sub rendered ~offset:0 ~len
    |> Path.from_string
    |> Result.expect ~msg:"pending snapshot path should stay valid UTF-8"
  else
    path

let is_pending_snapshot = fun path ->
  let basename = Path.basename path in
  String.ends_with ~suffix:".new" basename && String.contains basename ".expected"

let should_skip_dir = fun path ->
  match Path.basename path with
  | "_build"
  | ".git" -> true
  | _ -> false

let display_path = fun ~workspace_root path ->
  match Path.strip_prefix path ~prefix:workspace_root with
  | Ok rel -> "./" ^ Path.to_string rel
  | Error _ -> Path.to_string path

let matches_query = fun ?query snapshot ->
  match query with
  | None -> true
  | Some query ->
      String.contains (Path.to_string snapshot.pending) query
      || String.contains (Path.to_string snapshot.approved) query

let pending_snapshot_of_path = fun path -> {
  approved = ensure_trailing_new_removed path;
  pending = path;
}

let add_scan_root = fun roots path ->
  if Path.is_directory path then
    Vector.push roots ~value:path

let pending_snapshot_roots = fun ~workspace_root ->
  let roots = Vector.with_capacity ~size:16 in
  add_scan_root roots Path.(workspace_root / Path.v ".riot" / Path.v "snapshots");
  let packages_root = Path.(workspace_root / Path.v "packages") in
  if Path.is_directory packages_root then (
    match Fs.read_dir packages_root with
    | Error _ -> ()
    | Ok packages ->
        Iter.MutIterator.for_each
          packages
          ~fn:(fun package -> add_scan_root roots Path.(packages_root / package / Path.v "tests"))
  );
  roots
  |> Vector.to_array
  |> Array.to_list

let pending_snapshot_walker = fun ~workspace_root ->
  let roots = pending_snapshot_roots ~workspace_root in
  let walker =
    match Fs.Walker.create ~roots () with
    | Ok walker ->
        Fs.Walker.filter_entry
          walker
          ~f:(fun (entry: Fs.Walker.FileItem.t) ->
            let path = Fs.Walker.FileItem.path entry in
            match Fs.Walker.FileItem.kind entry with
            | Directory -> not (should_skip_dir path)
            | File -> is_pending_snapshot path
            | Symlink
            | Other -> false)
    | Error _ -> panic "snapshots walker configuration should be valid"
  in
  walker

let fold_pending_snapshots = fun ~workspace_root ?query ~init ~fn () ->
  let walker = pending_snapshot_walker ~workspace_root in
  let iter = Fs.Walker.into_iter walker in
  let rec loop acc iter =
    match Iter.Iterator.next iter with
    | (None, _) -> Ok acc
    | (Some (Error (err: Fs.Walker.error)), _) -> Error err.cause
    | (Some (Ok (entry: Fs.Walker.FileItem.t)), iter') -> (
        let path = Fs.Walker.FileItem.path entry in
        match Fs.Walker.FileItem.kind entry with
        | File ->
            let snapshot = pending_snapshot_of_path path in
            if matches_query ?query snapshot then
              match fn acc snapshot with
              | Error err -> Error err
              | Ok (Continue acc') -> loop acc' iter'
              | Ok (Stop acc') -> Ok acc'
            else
              loop acc iter'
        | Directory
        | Symlink
        | Other -> loop acc iter'
      )
  in
  loop init iter

let discover_pending_snapshots = fun ~workspace_root ?query () ->
  let snapshots = Vector.with_capacity ~size:16 in
  match fold_pending_snapshots
    ~workspace_root
    ?query
    ~init:()
    ~fn:(fun () snapshot ->
      Vector.push snapshots ~value:snapshot;
      Ok (Continue ()))
    () with
  | Error err -> Error err
  | Ok () ->
      Ok (
        snapshots
        |> Vector.to_array
        |> Array.to_list
        |> List.sort
          ~compare:(fun left right ->
            String.compare
              (Path.to_string left.pending)
              (Path.to_string right.pending))
      )

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some parent -> Fs.create_dir_all parent
  | None -> Ok ()

let approve_pending_snapshot = fun snapshot ->
  match Fs.read snapshot.pending with
  | Error err -> Error err
  | Ok content -> (
      match ensure_parent_dir snapshot.approved with
      | Error err -> Error err
      | Ok () -> (
          match Fs.write content snapshot.approved with
          | Error err -> Error err
          | Ok () -> Fs.remove_file snapshot.pending
        )
    )

let approve_pending_snapshots = fun snapshots ->
  let rec loop = function
    | [] ->
        Ok ()
    | snapshot :: rest -> (
        match approve_pending_snapshot snapshot with
        | Ok () -> loop rest
        | Error err -> Error err
      )
  in
  loop snapshots

let reject_pending_snapshots = fun snapshots ->
  let rec loop = function
    | [] ->
        Ok ()
    | snapshot :: rest -> (
        match Fs.remove_file snapshot.pending with
        | Ok () -> loop rest
        | Error err -> Error err
      )
  in
  loop snapshots

let parse_review_decision = fun input ->
  match input
  |> String.trim
  |> String.lowercase_ascii with
  | "a"
  | "approve" -> Some `Approve
  | "r"
  | "reject" -> Some `Reject
  | ""
  | "i"
  | "ignore" -> Some `Ignore
  | "q"
  | "quit" -> Some `Quit
  | _ -> None

let print_blob = fun value ->
  print value;
  if not (String.ends_with ~suffix:"\n" value) then
    print "\n"

let review_pending_snapshot = fun ~workspace_root snapshot ->
  println "";
  println ("Approved: " ^ display_path ~workspace_root snapshot.approved);
  println ("Pending:  " ^ display_path ~workspace_root snapshot.pending);
  match Fs.exists snapshot.approved with
  | Error err -> Error err
  | Ok false -> (
      match Fs.read snapshot.pending with
      | Error err -> Error err
      | Ok content ->
          println "Missing approved snapshot.";
          println "--- pending";
          print_blob content;
          Ok ()
    )
  | Ok true -> (
      let diff_cmd =
        Command.make
          "git"
          ~cwd:(Path.to_string workspace_root)
          ~args:[
            "diff";
            "--no-index";
            "--color=always";
            "--";
            Path.to_string snapshot.approved;
            Path.to_string snapshot.pending;
          ]
      in
      match Command.output diff_cmd with
      | Error (Command.SystemError msg) -> Error (IO.Unknown_error msg)
      | Ok { stdout; stderr; status } ->
          if status = 0 || status = 1 then (
            if not (String.equal stdout "") then
              print_blob stdout;
            if not (String.equal stderr "") then
              eprintln stderr;
            Ok ()
          ) else
            Error (IO.Unknown_error ("git diff failed for "
            ^ Path.to_string snapshot.pending
            ^ " with status "
            ^ Int.to_string status))
    )

let review_pending_snapshots = fun ~workspace_root snapshots ->
  let rec loop = function
    | [] ->
        Ok ()
    | snapshot :: rest -> (
        match review_pending_snapshot ~workspace_root snapshot with
        | Ok () -> loop rest
        | Error err -> Error err
      )
  in
  loop snapshots

let print_no_pending = fun () -> println "No pending snapshots."

let print_review_summary = fun count ->
  println
    ("Found " ^ Int.to_string count ^ " pending snapshot(s).")

let print_approved = fun ~workspace_root snapshot ->
  println
    ("Approved " ^ display_path ~workspace_root snapshot.approved)

let print_rejected = fun ~workspace_root snapshot ->
  println
    ("Rejected " ^ display_path ~workspace_root snapshot.pending)

let print_ignored = fun ~workspace_root snapshot ->
  println
    ("Ignored " ^ display_path ~workspace_root snapshot.pending)

let print_review_help = fun () -> println "Actions: [a]pprove, [r]eject, [i]gnore, [q]uit"

let print_review_prompt = fun () -> eprint "Decision [a/r/i/q]: "

let rec prompt_review_decision = fun tty ->
  print_review_prompt ();
  match Tty.read_line tty with
  | Error err -> Error err
  | Ok line -> (
      match parse_review_decision line with
      | Some decision -> Ok decision
      | None ->
          eprintln "Please enter a, r, i, or q.";
          prompt_review_decision tty
    )

let print_review_outcome = fun summary ->
  let prefix =
    if summary.quit then
      "Snapshot review stopped:"
    else
      "Snapshot review finished:"
  in
  println
    (prefix
    ^ " "
    ^ Int.to_string summary.approved_count
    ^ " approved, "
    ^ Int.to_string summary.rejected_count
    ^ " rejected, "
    ^ Int.to_string summary.ignored_count
    ^ " ignored")

let review_pending_snapshots_with_decider = fun ~workspace_root snapshots ~decide ->
  let rec loop summary = function
    | [] -> Ok summary
    | snapshot :: rest ->
        let* () = review_pending_snapshot ~workspace_root snapshot in
        let* decision = decide snapshot in
        (
          match decision with
          | `Approve ->
              let* () = approve_pending_snapshot snapshot in
              print_approved ~workspace_root snapshot;
              loop { summary with approved_count = summary.approved_count + 1 } rest
          | `Reject ->
              let* () = Fs.remove_file snapshot.pending in
              print_rejected ~workspace_root snapshot;
              loop { summary with rejected_count = summary.rejected_count + 1 } rest
          | `Ignore ->
              print_ignored ~workspace_root snapshot;
              loop { summary with ignored_count = summary.ignored_count + 1 } rest
          | `Quit -> Ok { summary with quit = true }
        )
  in
  loop empty_review_summary snapshots

let review_pending_snapshots_interactively = fun ~workspace_root snapshots ->
  match Tty.make () with
  | Error Tty.NoTtyConnected ->
      review_pending_snapshots ~workspace_root snapshots
      |> Result.map ~fn:(fun () -> empty_review_summary)
  | Error (Tty.SystemError err) -> Error err
  | Ok tty ->
      let result =
        if List.is_empty snapshots then
          Ok empty_review_summary
        else (
          print_review_help ();
          review_pending_snapshots_with_decider
            ~workspace_root
            snapshots
            ~decide:(fun _snapshot -> prompt_review_decision tty)
        )
      in
      Tty.restore tty;
      result

type snapshot_action_summary = { processed_count: int }

type streaming_review_state = {
  reviewed_count: int;
  summary: review_summary;
}

let empty_snapshot_action_summary = { processed_count = 0 }

let empty_streaming_review_state = { reviewed_count = 0; summary = empty_review_summary }

let run_streaming_approve = fun ~workspace_root ?query () ->
  fold_pending_snapshots
    ~workspace_root
    ?query
    ~init:empty_snapshot_action_summary
    ~fn:(fun summary snapshot ->
      let* () = approve_pending_snapshot snapshot in
      print_approved ~workspace_root snapshot;
      Ok (Continue { processed_count = summary.processed_count + 1 }))
    ()

let run_streaming_reject = fun ~workspace_root ?query () ->
  fold_pending_snapshots
    ~workspace_root
    ?query
    ~init:empty_snapshot_action_summary
    ~fn:(fun summary snapshot ->
      let* () = Fs.remove_file snapshot.pending in
      print_rejected ~workspace_root snapshot;
      Ok (Continue { processed_count = summary.processed_count + 1 }))
    ()

let run_streaming_review_with_decider = fun
  ~workspace_root ?query ?before_first_snapshot ~decide () ->
  fold_pending_snapshots
    ~workspace_root
    ?query
    ~init:empty_streaming_review_state
    ~fn:(fun state snapshot ->
      let () =
        if Int.equal state.reviewed_count 0 then
          match before_first_snapshot with
          | Some fn -> fn ()
          | None -> ()
      in
      let* () = review_pending_snapshot ~workspace_root snapshot in
      let* decision = decide snapshot in
      let summary = state.summary in
      let reviewed_count = state.reviewed_count + 1 in
      match decision with
      | `Approve ->
          let* () = approve_pending_snapshot snapshot in
          print_approved ~workspace_root snapshot;
          Ok (Continue {
            reviewed_count;
            summary = { summary with approved_count = summary.approved_count + 1 };
          })
      | `Reject ->
          let* () = Fs.remove_file snapshot.pending in
          print_rejected ~workspace_root snapshot;
          Ok (Continue {
            reviewed_count;
            summary = { summary with rejected_count = summary.rejected_count + 1 };
          })
      | `Ignore ->
          print_ignored ~workspace_root snapshot;
          Ok (Continue {
            reviewed_count;
            summary = { summary with ignored_count = summary.ignored_count + 1 };
          })
      | `Quit -> Ok (Stop { reviewed_count; summary = { summary with quit = true } }))
    ()

let run_streaming_review = fun ~workspace_root ?query () ->
  match Tty.make () with
  | Error Tty.NoTtyConnected ->
      fold_pending_snapshots
        ~workspace_root
        ?query
        ~init:empty_streaming_review_state
        ~fn:(fun state snapshot ->
          let* () = review_pending_snapshot ~workspace_root snapshot in
          Ok (Continue {
            reviewed_count = state.reviewed_count + 1;
            summary = { state.summary with ignored_count = state.summary.ignored_count + 1 };
          }))
        ()
  | Error (Tty.SystemError err) -> Error err
  | Ok tty ->
      let result =
        run_streaming_review_with_decider
          ~workspace_root
          ?query
          ~before_first_snapshot:print_review_help
          ~decide:(fun _snapshot -> prompt_review_decision tty)
          ()
      in
      Tty.restore tty;
      result

let run_action = fun ~workspace_root ?query action ->
  let result =
    match action with
    | `Review -> (
        match run_streaming_review ~workspace_root ?query () with
        | Error err -> Error err
        | Ok state ->
            if state.reviewed_count = 0 then
              print_no_pending ()
            else
              print_review_outcome state.summary;
            Ok ()
      )
    | `Approve -> (
        match run_streaming_approve ~workspace_root ?query () with
        | Error err -> Error err
        | Ok summary ->
            if summary.processed_count = 0 then
              print_no_pending ();
            Ok ()
      )
    | `Reject -> (
        match run_streaming_reject ~workspace_root ?query () with
        | Error err -> Error err
        | Ok summary ->
            if summary.processed_count = 0 then
              print_no_pending ();
            Ok ()
      )
  in
  match result with
  | Ok () -> Ok ()
  | Error err -> Error (Failure (IO.error_message err))

let run = fun ~(workspace:Riot_model.Workspace.t) matches ->
  let open ArgParser in
  match get_subcommand matches with
  | Some ("review", sub_matches) ->
      run_action
        ~workspace_root:workspace.root
        ?query:(get_one sub_matches "query")
        `Review
  | Some ("approve", sub_matches) ->
      run_action
        ~workspace_root:workspace.root
        ?query:(get_one sub_matches "query")
        `Approve
  | Some ("reject", sub_matches) ->
      run_action
        ~workspace_root:workspace.root
        ?query:(get_one sub_matches "query")
        `Reject
  | _ -> Error (Failure "Unknown snapshots subcommand")
