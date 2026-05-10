open Std
open Std.Collections

type build_dashboard_package = {
  key: string;
  package: Riot_model.Package.t;
  build_target: Riot_model.Target.t;
  mutable action_count: int;
  mutable completed_actions: int;
  mutable planning_source_count: int;
  mutable planned_sources: int;
  planning_sources: (string, string) HashMap.t;
  planning_source_order: string IndexSet.t;
  running_actions: (string, string) HashMap.t;
  running_action_order: string IndexSet.t;
  mutable status: build_dashboard_row_status;
}

and build_dashboard_row_status =
  | Planning
  | Preparing
  | Queued
  | Blocked
  | Finalizing
  | Waiting

type build_dashboard_state = {
  active: (string, build_dashboard_package) HashMap.t;
  active_order: string IndexSet.t;
  profile_name: string option;
  target_count: int option;
  package_count: int;
  total_action_count: int;
  completed_action_count: int;
  built_count: int;
  cached_count: int;
  failed_count: int;
  skipped_count: int;
}

type build_dashboard_action_view = { action_key: string; action_label: string }

type build_dashboard_row_view = {
  key: string;
  label: string;
  action_count: int;
  completed_actions: int;
  planning_source_count: int;
  planned_sources: int;
  actions: build_dashboard_action_view list;
  status: string;
  status_kind: build_dashboard_row_status;
}

type build_dashboard_board_view = {
  completed_action_count: int;
  total_action_count: int;
  summary: string;
  rows: build_dashboard_row_view list;
}

type build_dashboard_view =
  | Empty
  | Board of build_dashboard_board_view

type t = {
  mutable state: build_dashboard_state;
  mutable last_view: build_dashboard_view option;
  mutable last_line_count: int;
  mutable last_rendered_at: Time.Instant.t option;
}

let create_state = fun ?profile () ->
  {
    active = HashMap.with_capacity ~size:16;
    active_order = IndexSet.with_capacity ~size:16;
    profile_name = profile;
    target_count = None;
    package_count = 0;
    total_action_count = 0;
    completed_action_count = 0;
    built_count = 0;
    cached_count = 0;
    failed_count = 0;
    skipped_count = 0;
  }

let create = fun ?profile () ->
  {
    state = create_state ?profile ();
    last_view = None;
    last_line_count = 0;
    last_rendered_at = None;
  }

let clear = fun dashboard ->
  if dashboard.last_line_count > 0 then (
    eprint
      (Tty.Escape_seq.cursor_up_seq dashboard.last_line_count ^ Tty.Escape_seq.erase_display_seq 0);
    dashboard.last_line_count <- 0
  );
  dashboard.last_view <- None

let action_label = fun action ->
  let path_label = fun prefix path -> prefix ^ " " ^ Path.basename path in
  match action with
  | Riot_planner.Action.CompileInterface { source; _ } -> path_label "compile" source
  | Riot_planner.Action.CompileImplementation { source; _ } -> path_label "compile" source
  | Riot_planner.Action.GenerateInterface { source; _ } -> path_label "interface" source
  | Riot_planner.Action.CompileC { source; _ } -> path_label "compile" source
  | Riot_planner.Action.CreateLibrary { outputs; _ } -> (
      match outputs with
      | output :: _ -> path_label "archive" output
      | [] -> "archive"
    )
  | Riot_planner.Action.CreateExecutable { outputs; _ } -> (
      match outputs with
      | output :: _ -> path_label "link" output
      | [] -> "link"
    )
  | Riot_planner.Action.CreateSharedLibrary { outputs; _ } -> (
      match outputs with
      | output :: _ -> path_label "link" output
      | [] -> "link shared library"
    )
  | Riot_planner.Action.CopyFile { source; _ } -> path_label "copy" source
  | Riot_planner.Action.WriteFile { destination; _ } -> path_label "write" destination
  | Riot_planner.Action.BuildForeignDependency { name; _ } -> "build " ^ name

let node_label = fun (action: Riot_planner.Action_node.t) ->
  match (Riot_planner.Action_node.value action).actions with
  | first :: _ -> action_label first
  | [] -> "build"

let min_render_interval_ms = 80

let package_key = fun state ~build_target package ->
  let profile = Option.unwrap_or ~default:"" state.profile_name in
  Riot_model.Package_name.to_string package.Riot_model.Package.name
  ^ "|"
  ^ profile
  ^ "|"
  ^ Riot_model.Target.to_string build_target

let package_label = fun state ~build_target package ->
  let show_target =
    match state.target_count with
    | Some target_count -> target_count > 1
    | None -> false
  in
  let name = Riot_model.Package_name.to_string package.Riot_model.Package.name in
  let details =
    Common.display_package_details ?profile:state.profile_name ~build_target ~show_target package
  in
  match details with
  | [] -> name
  | details ->
      name ^ Common.Terminal.muted Common.terminal (" (" ^ String.concat ", " details ^ ")")

let get_package = fun state ~build_target package ->
  let key = package_key state ~build_target package in
  match HashMap.get state.active ~key with
  | Some row -> row
  | None ->
      let row = {
        key;
        package;
        build_target;
        action_count = 0;
        completed_actions = 0;
        planning_source_count = 0;
        planned_sources = 0;
        planning_sources = HashMap.with_capacity ~size:4;
        planning_source_order = IndexSet.with_capacity ~size:4;
        running_actions = HashMap.with_capacity ~size:4;
        running_action_order = IndexSet.with_capacity ~size:4;
        status = Waiting;
      }
      in
      let _ = HashMap.insert state.active ~key ~value:row in
      let _ = IndexSet.insert state.active_order ~value:key in
      row

let find_package = fun state ~build_target package ->
  let key = package_key state ~build_target package in
  HashMap.get state.active ~key

let remove_package = fun state ~build_target package ->
  let key = package_key state ~build_target package in
  let _ = HashMap.remove state.active ~key in
  let _ = IndexSet.remove state.active_order ~value:key in
  ()

let count_summary = fun state ->
  Common.build_count_summary
    ~built_count:state.built_count
    ~cached_count:state.cached_count
    ~skipped_count:state.skipped_count
    ~failed_count:state.failed_count
    ()

let package_progress = fun state ->
  state.built_count + state.cached_count + state.skipped_count + state.failed_count

let set_action_count = fun state ~build_target package ~action_count ->
  let row = get_package state ~build_target package in
  let previous_action_count = row.action_count in
  row.action_count <- action_count;
  HashMap.clear row.planning_sources;
  IndexSet.clear row.planning_source_order;
  if HashMap.is_empty row.running_actions then
    row.status <- Preparing;
  {
    state with
    total_action_count = Int.max 0 (state.total_action_count + action_count - previous_action_count);
  }

let action_key = fun (action: Riot_planner.Action_node.t) ->
  Graph.SimpleGraph.Node_id.to_string
    (Riot_planner.Action_node.id action)

let running_action_views = fun row ->
  let actions = ref [] in
  IndexSet.for_each
    row.running_action_order
    ~fn:(fun key ->
      match HashMap.get row.running_actions ~key with
      | Some label -> actions := { action_key = key; action_label = label } :: !actions
      | None -> ());
  List.reverse !actions

let planning_source_views = fun row ->
  let max_visible_sources = Int.max 1 Thread.available_parallelism in
  let sources = ref [] in
  let visible_count = ref 0 in
  IndexSet.for_each
    row.planning_source_order
    ~fn:(fun key ->
      if !visible_count < max_visible_sources then
        match HashMap.get row.planning_sources ~key with
        | Some label ->
            visible_count := !visible_count + 1;
            sources := { action_key = key; action_label = label } :: !sources
        | None -> ());
  List.reverse !sources

let set_planning_source = fun row source ->
  let key = Path.to_string source in
  let label = "plan " ^ Path.basename source in
  let _ = IndexSet.insert row.planning_source_order ~value:key in
  let _ = HashMap.insert row.planning_sources ~key ~value:label in
  ()

let mark_action_started = fun state ~build_target ~package ~action_id ~action_label ->
  match find_package state ~build_target package with
  | Some row ->
      let _ = IndexSet.insert row.running_action_order ~value:action_id in
      HashMap.clear row.planning_sources;
      IndexSet.clear row.planning_source_order;
      let _ = HashMap.insert row.running_actions ~key:action_id ~value:action_label in
      state
  | None -> state

let mark_action_completed = fun state ~build_target package action_id ->
  match find_package state ~build_target package with
  | Some row ->
      let previous_completed_actions = row.completed_actions in
      row.completed_actions <- if row.action_count > 0 then
        Int.min row.action_count (row.completed_actions + 1)
      else
        row.completed_actions + 1;
      let _ = HashMap.remove row.running_actions ~key:action_id in
      let _ = IndexSet.remove row.running_action_order ~value:action_id in
      if HashMap.is_empty row.running_actions then
        row.status <- if row.action_count > 0 && row.completed_actions >= row.action_count then
          Finalizing
        else
          Queued;
      {
        state with
        completed_action_count = state.completed_action_count + row.completed_actions
        - previous_completed_actions;
      }
  | None -> state

let complete_package_actions = fun state ~build_target package ->
  match find_package state ~build_target package with
  | Some row when row.action_count > row.completed_actions ->
      let remaining_actions = row.action_count - row.completed_actions in
      row.completed_actions <- row.action_count;
      row.status <- Finalizing;
      { state with completed_action_count = state.completed_action_count + remaining_actions }
  | Some _
  | None -> state

let truncate = fun ~width text ->
  if String.width text <= width then
    text
  else if width <= 1 then
    String.truncate_width ~width:(Int.max 0 width) text
  else
    String.truncate_width ~width ~tail:"..." text

let terminal_width = fun () ->
  match Tty.Size.get () with
  | Ok { cols; _ } -> Int.max 40 cols
  | Error _ -> 120

let update_state = fun state event ->
  match event.Riot_model.Event.kind with
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPhase (
      Riot_model.Event.TargetsResolved { target_count }
    )
  ) ->
      { state with target_count = Some target_count }
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPhase (Riot_model.Event.PackagePlanningStarted { package_count; _ }
    | Riot_model.Event.PackagePlanningFinished { package_count; _ }
    | Riot_model.Event.PackageExecutionStarted { package_count; _ })
  ) ->
      { state with package_count }
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPhase (
      Riot_model.Event.PackagePlanStarted { package; build_target; source_count; _ }
    )
  ) ->
      let row = get_package state ~build_target package in
      row.planning_source_count <- source_count;
      row.planned_sources <- 0;
      HashMap.clear row.planning_sources;
      IndexSet.clear row.planning_source_order;
      row.status <- Planning;
      state
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPhase (
      Riot_model.Event.PackagePlanSourceStarted {
        package;
        build_target;
        source;
        source_index;
        source_count;
        _;
      }
    )
  ) ->
      let row = get_package state ~build_target package in
      row.planning_source_count <- source_count;
      row.planned_sources <- source_index;
      set_planning_source row source;
      row.status <- Planning;
      state
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPhase (
      Riot_model.Event.PackagePlanFinished { package; build_target; source_count; _ }
    )
  ) ->
      (
        match find_package state ~build_target package with
        | Some row ->
            row.planning_source_count <- source_count;
            row.planned_sources <- source_count;
            HashMap.clear row.planning_sources;
            IndexSet.clear row.planning_source_order;
            row.status <- Preparing
        | None -> ()
      );
      state
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPackageCompilationStarted { package; build_target; action_count; _ }
  ) ->
      set_action_count state ~build_target package ~action_count
  | Riot_model.Event.Build (Riot_model.Event.BuildSandboxCreated { package; build_target; _ }
  | Riot_model.Event.BuildSandboxInputsCopied { package; build_target; _ }
  | Riot_model.Event.BuildSandboxDependenciesCopied { package; build_target; _ }) ->
      (
        match find_package state ~build_target package with
        | Some row -> row.status <- Preparing
        | None -> ()
      );
      state
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPackageExecutionPrepared { package; build_target; _ }
  ) ->
      (
        match find_package state ~build_target package with
        | Some row -> row.status <- Queued
        | None -> ()
      );
      state
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPhase (
      Riot_model.Event.PackageActionGraphPlanned { package; build_target; action_count; _ }
    )
  ) ->
      set_action_count state ~build_target package ~action_count
  | Riot_model.Event.Build (
    Riot_model.Event.BuildActionStarted {
      package;
      build_target;
      action_id;
      action_label;
      _;
    }
  ) ->
      mark_action_started state ~build_target ~package ~action_id ~action_label
  | Riot_model.Event.Build (
    Riot_model.Event.BuildActionCompleted { package; build_target; action_id; _ }
  ) ->
      mark_action_completed state ~build_target package action_id
  | Riot_model.Event.Build (
    Riot_model.Event.BuildActionFailed { package; build_target; action_id; _ }
  ) ->
      mark_action_completed state ~build_target package action_id
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPackageFinished {
      package;
      build_target;
      status = Riot_model.Event.Fresh;
      _;
    }
  ) ->
      let state = complete_package_actions state ~build_target package in
      remove_package state ~build_target package;
      { state with built_count = state.built_count + 1 }
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPackageFinished {
      package;
      build_target;
      status = Riot_model.Event.Cached;
      _;
    }
  ) ->
      remove_package state ~build_target package;
      { state with cached_count = state.cached_count + 1 }
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPackageSkippedDetailed { package; build_target; _ }
  ) ->
      remove_package state ~build_target package;
      { state with skipped_count = state.skipped_count + 1 }
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPackageFailed { package; build_target; _ }
  ) ->
      remove_package state ~build_target package;
      { state with failed_count = state.failed_count + 1 }
  | _ -> state

let row_view = fun state row ->
  let running_actions = running_action_views row in
  let actions =
    match (row.status, running_actions) with
    | (Planning, []) -> planning_source_views row
    | _ -> running_actions
  in
  let status =
    match (row.status, actions) with
    | (Planning, _) -> "planning"
    | (_, _ :: _) -> "running"
    | (Preparing, []) -> "preparing"
    | (Queued, []) -> "queued"
    | (Blocked, []) -> "blocked"
    | (Finalizing, []) -> "finalizing"
    | (Waiting, []) -> "waiting"
  in
  {
    key = row.key;
    label = package_label state ~build_target:row.build_target row.package;
    action_count = row.action_count;
    completed_actions = row.completed_actions;
    planning_source_count = row.planning_source_count;
    planned_sources = row.planned_sources;
    actions;
    status;
    status_kind = row.status;
  }

let row_is_active = fun row ->
  not (HashMap.is_empty row.running_actions) || not (HashMap.is_empty row.planning_sources) || match row.status with
  | Planning -> true
  | Finalizing -> true
  | Preparing
  | Queued
  | Blocked
  | Waiting -> false

let render_state = fun state ->
  let package_done_count = package_progress state in
  if package_done_count = 0 && state.total_action_count = 0 && HashMap.length state.active = 0 then
    Empty
  else
    let rows = ref [] in
    IndexSet.for_each
      state.active_order
      ~fn:(fun key ->
        match HashMap.get state.active ~key with
        | Some row when row_is_active row -> rows := row_view state row :: !rows
        | Some _
        | None -> ());
  let rows = List.reverse !rows in
  if List.is_empty rows && state.completed_action_count = 0 && state.total_action_count = 0 then
    Empty
  else
    Board {
      completed_action_count = state.completed_action_count;
      total_action_count = state.total_action_count;
      summary = count_summary state;
      rows;
    }

let rec row_views_equal = fun left right ->
  match (left, right) with
  | ([], []) -> true
  | (left :: left_rest, right :: right_rest) ->
      String.equal left.key right.key
      && String.equal left.label right.label
      && left.action_count = right.action_count
      && left.completed_actions = right.completed_actions
      && left.planning_source_count = right.planning_source_count
      && left.planned_sources = right.planned_sources
      && left.actions = right.actions
      && String.equal left.status right.status
      && left.status_kind = right.status_kind
      && row_views_equal left_rest right_rest
  | ([], _ :: _)
  | (_ :: _, []) -> false

let board_views_equal = fun left right ->
  left.completed_action_count = right.completed_action_count
  && left.total_action_count = right.total_action_count
  && String.equal left.summary right.summary
  && row_views_equal left.rows right.rows

let views_equal = fun left right ->
  match (left, right) with
  | (Empty, Empty) -> true
  | (Board left, Board right) -> board_views_equal left right
  | (Empty, Board _)
  | (Board _, Empty) -> false

let row_line = fun ~width ~is_last row ->
  let running_action_count = List.length row.actions in
  let progress =
    match row.status_kind with
    | Planning ->
        if row.planning_source_count > 0 then
          "["
          ^ Int.to_string row.planned_sources
          ^ "/"
          ^ Int.to_string row.planning_source_count
          ^ "]"
        else
          "[0/?]"
    | Preparing
    | Queued
    | Blocked
    | Finalizing
    | Waiting ->
        let visible_action =
          if row.action_count > 0 then
            Int.min row.action_count (row.completed_actions + running_action_count)
          else
            row.completed_actions + running_action_count
        in
        if row.action_count > 0 then
          "[" ^ Int.to_string visible_action ^ "/" ^ Int.to_string row.action_count ^ "]"
        else
          "[" ^ Int.to_string row.completed_actions ^ "/?]"
  in
  let branch =
    if is_last then
      "└── "
    else
      "├── "
  in
  let suffix =
    match (row.status_kind, row.actions) with
    | (Planning, _) -> " " ^ Common.Terminal.status_label Common.terminal Common.Terminal.Plan
    | (_, []) -> " " ^ row.status
    | _ -> ""
  in
  truncate ~width (branch ^ row.label ^ " " ^ progress ^ suffix)

let action_line = fun ~width ~parent_is_last ~is_last action ->
  let prefix =
    if parent_is_last then
      "    "
    else
      "│   "
  in
  let branch =
    if is_last then
      "└── "
    else
      "├── "
  in
  truncate ~width (prefix ^ branch ^ action.action_label)

let push_action_lines = fun ~width ~parent_is_last lines actions ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | [ action ] ->
        Vector.push lines ~value:(action_line ~width ~parent_is_last ~is_last:true action)
    | action :: rest ->
        Vector.push lines ~value:(action_line ~width ~parent_is_last ~is_last:false action);
        loop rest
  in
  loop actions

let push_row_lines = fun ~width lines rows ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | [ row ] ->
        Vector.push lines ~value:(row_line ~width ~is_last:true row);
        push_action_lines ~width ~parent_is_last:true lines row.actions
    | row :: rest ->
        Vector.push lines ~value:(row_line ~width ~is_last:false row);
        push_action_lines ~width ~parent_is_last:false lines row.actions;
        loop rest
  in
  loop rows

let view_lines = fun view ->
  match view with
  | Empty -> Vector.with_capacity ~size:0
  | Board board ->
      let width = terminal_width () in
      let lines = Vector.with_capacity ~size:8 in
      Vector.push
        lines
        ~value:(truncate
          ~width
          ("["
          ^ Int.to_string board.completed_action_count
          ^ "/"
          ^ Int.to_string board.total_action_count
          ^ "] actions  "
          ^ board.summary));
      push_row_lines ~width lines board.rows;
      lines

let throttle_allows_render = fun dashboard ->
  match dashboard.last_rendered_at with
  | None -> true
  | Some rendered_at ->
      Time.Duration.to_millis (Time.Instant.elapsed rendered_at) >= min_render_interval_ms

let draw = fun ?(force = false) dashboard ->
  let view = render_state dashboard.state in
  let lines = view_lines view in
  if Vector.is_empty lines then
    if force then
      clear dashboard
    else
      ()
  else
    let view_matches =
      match dashboard.last_view with
      | Some previous -> views_equal previous view
      | None -> false
    in
    if (not force) && (view_matches || not (throttle_allows_render dashboard)) then
      ()
    else (
      clear dashboard;
      Vector.for_each lines ~fn:(fun line -> eprint (line ^ "\n"));
      dashboard.last_line_count <- Vector.length lines;
      dashboard.last_view <- Some view;
      dashboard.last_rendered_at <- Some (Time.Instant.now ())
    )

let update = fun dashboard event -> dashboard.state <- update_state dashboard.state event

let write_build_kind = fun ?render_state ?profile dashboard event ->
  match event with
  | Riot_model.Event.BuildPackageFailed { package; build_target; error } ->
      clear dashboard;
      Common.out_status
        Common.Terminal.Error
        (Common.display_build_package_name ?render_state ?profile ~build_target package
        ^ ": "
        ^ Common.build_package_error_message error)
  | Riot_model.Event.BuildPackageWarnings { package; build_target; messages; _ } ->
      clear dashboard;
      messages
      |> List.for_each
        ~fn:(fun message ->
          Common.out_prefixed_payload
            ~prefix:(Common.status_line
              Common.Terminal.Warning
              (Common.display_build_package_name ?render_state ?profile ~build_target package ^ ": "))
            message)
  | _ -> draw dashboard

let write_phase_event = fun dashboard phase ->
  match phase with
  | Riot_model.Event.BuildLockWaiting _ ->
      clear dashboard;
      Common.out_status Common.Terminal.Running "build lock is taken, waiting..."
  | Riot_model.Event.PackageExecutionFinished { built_count; failed_count; error_count; _ } ->
      if failed_count > 0 || error_count > 0 then (
        clear dashboard;
        Common.out_status
          Common.Terminal.Error
          ("execution failed: "
          ^ Common.build_count_summary
            ~built_count
            ~cached_count:0
            ~skipped_count:0
            ~failed_count
            ~error_count
            ())
      ) else
        draw dashboard
  | _ -> draw dashboard

let write_event = fun
  ?render_state ?profile ?workspace_root ~seen_registry_updates dashboard event ->
  update dashboard event;
  match event.Riot_model.Event.kind with
  | Riot_model.Event.Deps _ ->
      clear dashboard;
      Line.write_pm_event ~seen_registry_updates event
  | Riot_model.Event.Build (Riot_model.Event.BuildTargetBuilding _) -> ()
  | Riot_model.Event.Cache event ->
      clear dashboard;
      Line.write_cache_gc_event event
  | Riot_model.Event.Build (Riot_model.Event.BuildPhase phase) -> write_phase_event dashboard phase
  | Riot_model.Event.Build event -> write_build_kind ?render_state ?profile dashboard event
  | _ ->
      clear dashboard;
      Line.write_event ?render_state ?profile ?workspace_root ~seen_registry_updates event

let write_build_error = fun dashboard err ->
  clear dashboard;
  Line.write_build_error err

let write_command_error = fun dashboard message ->
  clear dashboard;
  Line.write_command_error message

let write_build_finished = fun dashboard ~duration ~progress ->
  clear dashboard;
  Line.write_build_finished ~duration ~progress
