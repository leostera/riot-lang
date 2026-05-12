open Std
open Std.Collections

let debounce = Time.Duration.from_millis 200

let workspace_member_packages = fun (workspace: Riot_model.Workspace.t) ->
  workspace.packages
  |> List.filter ~fn:Riot_model.Package_manifest.is_workspace_member

let package_name_equal = Riot_model.Package_name.equal

let find_workspace_package = fun packages package_name ->
  List.find
    packages
    ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> package_name_equal pkg.name package_name)

let package_dependency_names = fun (pkg: Riot_model.Package_manifest.t) ->
  Riot_model.Package_manifest.all_dependencies pkg
  |> List.map ~fn:(fun (dep: Riot_model.Package_manifest.dependency) -> dep.name)

let selected_package_cone = fun ~(workspace:Riot_model.Workspace.t) ~package_filters ->
  let packages = workspace_member_packages workspace in
  match package_filters with
  | [] -> packages
  | package_filters ->
      let rec loop = fun ~visited acc pending ->
        match pending with
        | [] -> acc
        | package_name :: rest ->
            if List.any visited ~fn:(package_name_equal package_name) then
              loop ~visited acc rest
            else
              let visited = package_name :: visited in
              match find_workspace_package packages package_name with
              | None -> loop ~visited acc rest
              | Some pkg -> loop ~visited (pkg :: acc) (package_dependency_names pkg @ rest)
      in
      loop ~visited:[] [] package_filters

let watch_roots = fun ~(workspace:Riot_model.Workspace.t) ~package_filters ->
  selected_package_cone ~workspace ~package_filters
  |> List.map ~fn:(fun pkg -> Riot_model.Workspace.package_root workspace pkg)
  |> List.map ~fn:Path.normalize
  |> List.sort ~compare:Path.compare
  |> List.unique ~compare:Path.compare

let ignored_prefixes = fun ~(workspace:Riot_model.Workspace.t) ->
  [
    workspace.target_dir_root;
    Path.(workspace.root / Path.v ".git");
    Path.(workspace.root / Path.v ".riot");
    Path.(workspace.root / Path.v "riot.lock");
  ]
  |> List.map ~fn:Path.normalize

let path_has_prefix = fun ~prefix path ->
  match Path.strip_prefix path ~prefix with
  | Ok _ -> true
  | Error _ -> false

let is_pending_snapshot_candidate = fun path ->
  let basename = Path.basename path in
  String.ends_with ~suffix:".new" basename && String.contains basename ".expected"

let should_ignore_path = fun ~workspace path ->
  let path = Path.normalize path in
  is_pending_snapshot_candidate path
  || List.any (ignored_prefixes ~workspace) ~fn:(fun prefix -> path_has_prefix ~prefix path)

let event_requires_rescan = fun (event: Fs.Event.t) ->
  event.system.root_changed
  || event.system.must_scan_subdirs
  || event.system.user_dropped
  || event.system.kernel_dropped

let is_directory_chatter = fun (event: Fs.Event.t) ->
  match (event.file_type, event.kind) with
  | (Fs.Event.Directory, Fs.Event.Modified)
  | (Fs.Event.Directory, Fs.Event.Metadata) -> true
  | _ -> false

let should_ignore_event = fun ~workspace (event: Fs.Event.t) ->
  should_ignore_path ~workspace event.path
  || (is_directory_chatter event && not (event_requires_rescan event))

let changed_paths = fun ~workspace events ->
  events
  |> List.filter ~fn:(fun event -> not (should_ignore_event ~workspace event))
  |> List.map ~fn:(fun (event: Fs.Event.t) -> Path.normalize event.path)
  |> List.sort ~compare:Path.compare
  |> List.unique ~compare:Path.compare

let display_path = fun ~(workspace:Riot_model.Workspace.t) path ->
  Ui.Common.display_path
    ~workspace_root:workspace.root
    path

let plural = fun count singular plural ->
  if count = 1 then
    singular
  else
    plural

let write_json = fun mode fields ->
  match mode with
  | Ui.Json -> println (Data.Json.to_string (Data.Json.Object fields))
  | Ui.Line
  | Ui.TUI -> ()

let write_human_status = fun mode status message ->
  match mode with
  | Ui.Json -> ()
  | Ui.Line
  | Ui.TUI -> Ui.Common.out_status status message

let paths_json = fun ~workspace paths ->
  Data.Json.Array (List.map paths ~fn:(fun path -> Data.Json.String (display_path ~workspace path)))

let write_started = fun ~command ~workspace ~mode roots ->
  let root_count = List.length roots in
  write_json
    mode
    [
      ("type", Data.Json.String "watch.started");
      ("command", Data.Json.String command);
      ("root_count", Data.Json.Int root_count);
      (
        "roots",
        Data.Json.Array (List.map
          roots
          ~fn:(fun root -> Data.Json.String (display_path ~workspace root)))
      );
    ];
  write_human_status
    mode
    Ui.Common.Terminal.Running
    ("watching "
    ^ Int.to_string root_count
    ^ " package "
    ^ plural root_count "root" "roots"
    ^ " for changes")

let change_message = fun ~command ~workspace paths ->
  match paths with
  | [] -> "change detected; rerunning " ^ command
  | path :: rest ->
      "change detected in " ^ display_path ~workspace path ^ (
        match rest with
        | [] -> ""
        | _ -> " (+" ^ Int.to_string (List.length rest) ^ " more)"
      ) ^ "; rerunning " ^ command

let write_change = fun ~command ~workspace ~mode paths ->
  write_json
    mode
    [
      ("type", Data.Json.String "watch.change");
      ("command", Data.Json.String command);
      ("paths", paths_json ~workspace paths);
    ];
  write_human_status
    mode
    Ui.Common.Terminal.Running
    (change_message ~command ~workspace paths)

let write_run_finished = fun ~command ~mode result ->
  match result with
  | Ok () ->
      write_json
        mode
        [
          ("type", Data.Json.String "watch.run.completed");
          ("command", Data.Json.String command);
          ("status", Data.Json.String "ok");
        ]
  | Error err ->
      write_json
        mode
        [
          ("type", Data.Json.String "watch.run.completed");
          ("command", Data.Json.String command);
          ("status", Data.Json.String "failed");
          ("message", Data.Json.String (Exception.to_string err));
        ]

let write_no_roots = fun ~command ~mode ->
  let message = "watch mode found no workspace package roots to monitor for " ^ command in
  write_json
    mode
    [
      ("type", Data.Json.String "watch.error");
      ("command", Data.Json.String command);
      ("message", Data.Json.String message);
    ];
  write_human_status mode Ui.Common.Terminal.Error message;
  Error (Failure message)

let event_selector = fun __tmp1 ->
  match __tmp1 with
  | Fs.FileWatcher.FileEvents events -> Select events
  | _ -> Skip

let rec drain_events = fun acc ->
  try
    let events = receive ~selector:event_selector ~timeout:debounce () in
    drain_events (events @ acc)
  with
  | Receive_timeout -> List.reverse acc

let wait_events = fun () ->
  let events = receive ~selector:event_selector () in
  drain_events events

let run = fun ~command ~workspace ~package_filters ~mode ~run_once () ->
  let initial_result = run_once () in
  write_run_finished ~command ~mode initial_result;
  let roots = watch_roots ~workspace ~package_filters in
  match roots with
  | [] -> write_no_roots ~command ~mode
  | roots ->
      let ignore_prefixes = ignored_prefixes ~workspace in
      List.for_each
        roots
        ~fn:(fun root ->
          let _watcher = Fs.FileWatcher.start_link ~latency:debounce ~ignore_prefixes ~root () in
          ());
      write_started ~command ~workspace ~mode roots;
      let rec loop () =
        let events = wait_events () in
        match changed_paths ~workspace events with
        | [] -> loop ()
        | paths -> run_after_change paths
      and run_after_change paths =
        write_change ~command ~workspace ~mode paths;
        let result = run_once () in
        write_run_finished ~command ~mode result;
        match changed_paths ~workspace (drain_events []) with
        | [] -> loop ()
        | paths -> run_after_change paths
      in
      loop ()
