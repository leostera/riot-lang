open Std
open Std.Collections

type render_state = {
  mutable target_count: int option;
  profile_name: string option;
}

type build_progress = {
  mutable built_count: int;
  mutable cached_count: int;
  mutable failed_count: int;
  mutable skipped_count: int;
}

let create_render_state = fun ?profile () -> { target_count = None; profile_name = profile }

let out = eprintln

module Terminal = Jollyroger.Terminal

let terminal = Terminal.make ()

let status_line = fun status message -> Terminal.status_line terminal status message

let out_status = fun status message -> out (status_line status message)

let display_path = fun ?workspace_root path ->
  match workspace_root with
  | Some workspace_root -> (
      match Path.strip_prefix path ~prefix:workspace_root with
      | Ok rel -> "./" ^ Path.to_string rel
      | Error _ -> (
          match Env.home_dir () with
          | Some home -> (
              match Path.strip_prefix path ~prefix:home with
              | Ok rel -> "~/" ^ Path.to_string rel
              | Error _ -> Path.to_string path
            )
          | None -> Path.to_string path
        )
    )
  | None -> (
      match Env.home_dir () with
      | Some home -> (
          match Path.strip_prefix path ~prefix:home with
          | Ok rel -> "~/" ^ Path.to_string rel
          | Error _ -> Path.to_string path
        )
      | None -> Path.to_string path
    )

let write_global_bin_path_hint = fun () ->
  out "";
  out "To use the installed binary from anywhere, add ~/.riot/bin to your PATH:";
  out "  export PATH='$HOME/.riot/bin:$PATH'"

let path_has_prefix = fun ~prefix path -> String.starts_with ~prefix (Path.to_string path)

let package_has_dev_artifact = fun ~(prefix:string) (package: Riot_model.Package.t) sources ->
  not (List.is_empty sources)
  || List.any
    package.binaries
    ~fn:(fun (binary: Riot_model.Package.binary) -> path_has_prefix ~prefix binary.path)

let workspace_artifact_labels = fun (package: Riot_model.Package.t) ->
  if not (Riot_model.Package.is_workspace_member package) then
    []
  else
    [
      ("test", package_has_dev_artifact ~prefix:"tests/" package package.sources.tests);
      ("example", package_has_dev_artifact ~prefix:"examples/" package package.sources.examples);
      ("bench", package_has_dev_artifact ~prefix:"bench/" package package.sources.bench);
    ]
    |> List.filter_map
      ~fn:(fun (label, enabled) ->
        if enabled then
          Some label
        else
          None)

let profile_details = fun __tmp1 ->
  match __tmp1 with
  | Some profile -> [ profile ]
  | None -> []

let display_package_details = fun
  ?profile ?build_target ?(show_target = false) (package: Riot_model.Package.t) ->
  let version_details =
    if Riot_model.Package.is_workspace_member package then
      []
    else
      match package.publish.version with
      | Some version -> [ Std.Version.to_string version ]
      | None -> []
  in
  let target_details =
    match build_target with
    | Some target when show_target -> [ Riot_model.Target.to_string target ]
    | _ -> []
  in
  ((profile_details profile @ version_details) @ workspace_artifact_labels package) @ target_details

let display_package_name = fun
  ?profile ?build_target ?(show_target = false) (package: Riot_model.Package.t) ->
  let name = Riot_model.Package_name.to_string package.name in
  let details = display_package_details ?profile ?build_target ~show_target package in
  match details with
  | [] -> name
  | details -> name ^ " (" ^ String.concat ", " details ^ ")"

let labeled_multiline_lines = fun ~label value ->
  match String.split value ~by:"\n" with
  | [] -> [ label ^ ":" ]
  | first :: rest ->
      (label ^ ": " ^ first) :: List.map
        rest
        ~fn:(fun line ->
          if String.equal line "" then
            ""
          else
            "  " ^ line)

let error_line = fun message -> status_line Terminal.Error message

let display_planner_file = fun path ->
  let path_text = Path.to_string path in
  if
    Path.is_absolute path
    || String.starts_with ~prefix:"./" path_text
    || String.starts_with ~prefix:"../" path_text
  then
    path_text
  else
    "./" ^ path_text

let planning_error_lines = fun __tmp1 ->
  match __tmp1 with
  | Riot_planner.Planning_error.CyclicDependency { cycle } ->
      [
        error_line "cyclic dependency detected while planning modules";
        "Riot found a cycle in the module graph, so it cannot choose a safe compile order.";
        "cycle: " ^ String.concat " -> " cycle;
        "examples:";
        "  - move shared types or helpers into a lower-level module";
        "  - replace one side of the cycle with a parameter, callback, or interface";
      ]
  | Riot_planner.Planning_error.ScanFailed { path; reason } ->
      [
        error_line "failed to scan package sources";
        "Riot could not read the source tree it needs to plan this package.";
        "path: " ^ Path.to_string path;
      ]
      @ labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.DependencyAnalysisFailed { reason } ->
      [
        error_line "dependency analysis failed";
        "Riot could not parse or analyze a source file while discovering module dependencies.";
      ]
      @ labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.GraphBuildFailed { reason } ->
      [
        error_line "failed to build the module graph";
        "Riot analyzed the package sources but could not assemble a valid build graph.";
      ]
      @ labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.SourceDependsOnUndeclaredPackageModule {
      package_name;
      source;
      requested_module;
      allowed_modules;
      suggested_modules;
    } ->
      let allowed_modules =
        match allowed_modules with
        | [] -> "<none>"
        | allowed_modules -> String.concat ", " allowed_modules
      in
      let suggestion_lines =
        match suggested_modules with
        | [] -> []
        | [ suggestion ] -> [ "did you mean: " ^ suggestion ]
        | suggestions -> [ "did you mean one of: " ^ String.concat ", " suggestions ]
      in
      [
        error_line (requested_module ^ " is not available to package " ^ package_name);
        "The source file imports "
        ^ requested_module
        ^ ", but Riot only exposes modules from this package and its direct dependencies.";
        "package: " ^ package_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "available direct modules: " ^ allowed_modules;
      ]
      @ suggestion_lines
      @ [
        "examples:";
        "  - add the package that provides " ^ requested_module ^ " to [dependencies]";
        "  - or depend through one of the exposed modules above if that is the public API you meant";
      ]
  | Riot_planner.Planning_error.TargetDependsOnInternalLibraryModule {
      target_name;
      source;
      requested_module;
      internal_module;
      public_module;
    } ->
      [
        error_line ("target " ^ target_name ^ " imports private module " ^ requested_module);
        "The target source reaches "
        ^ internal_module
        ^ ", which is internal to this package library.";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "internal module: " ^ internal_module;
        "public module: " ^ public_module;
        "examples:";
        "  - use " ^ public_module ^ "." ^ requested_module ^ " instead";
        "  - move shared target code behind " ^ public_module ^ " or a shared helper module";
      ]
  | Riot_planner.Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
      target_name;
      source;
      requested_module;
      internal_module;
      public_module;
    } ->
      let public_leaf =
        internal_module
        |> String.split ~by:"__"
        |> List.reverse
        |> List.head
        |> Option.unwrap_or ~default:requested_module
      in
      [
        error_line ("target " ^ target_name ^ " imports private module " ^ requested_module);
        "The target source reaches "
        ^ internal_module
        ^ ", which is a namespaced implementation detail of this package library.";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "internal module: " ^ internal_module;
        "public module: " ^ public_module;
        "examples:";
        "  - use " ^ public_module ^ "." ^ public_leaf ^ " instead";
        "  - move shared target code behind " ^ public_module ^ " or a shared helper module";
      ]
  | Riot_planner.Planning_error.TargetDependsOnOtherTargetRoot {
      target_name;
      source;
      requested_module;
      other_target_name;
      other_target_module;
      public_module;
    } ->
      [
        error_line ("target " ^ target_name ^ " imports target entrypoint " ^ other_target_module);
        "The target source reaches another target root. Target entrypoints are private and are not reusable modules.";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "other target: " ^ other_target_name;
        "other target module: " ^ other_target_module;
        "public module: " ^ public_module;
        "examples:";
        "  - move shared code behind " ^ public_module;
        "  - move shared code into a helper module that both targets can import";
      ]
  | Riot_planner.Planning_error.InvalidExecutableMain {
      package_name;
      target_name;
      file;
      error;
      _;
    } ->
      let file = display_planner_file file in
      let (headline, reason_lines) =
        match error with
        | Riot_planner.Planning_error.MissingMain -> (
          "`" ^ target_name ^ "` has no executable entry point",
          [ "But we could not find one." ]
        )
        | Riot_planner.Planning_error.MultipleMainDefinitions { count } -> (
          "`" ^ target_name ^ "` has more than one executable entry point",
          [
            "But we found " ^ Int.to_string count ^ " top-level `main` definitions.";
            "Executable targets must define exactly one.";
          ]
        )
        | Riot_planner.Planning_error.InvalidMainParameters { parameters } ->
            let parameters =
              match parameters with
              | [] -> "<none>"
              | parameters -> String.concat ", " parameters
            in
            (
              "`" ^ target_name ^ "` has an invalid executable entry point",
              [
                "But the `main` function we found does not have that shape.";
                "found parameters: " ^ parameters;
              ]
            )
      in
      [
        error_line headline;
        "";
        "Riot is building this target as an executable:";
        "";
        "    package: " ^ package_name;
        "    target:  " ^ target_name;
        "    file:    " ^ file;
        "";
        "To start the program, Riot needs this file to define a top-level";
        "`main` function with this shape:";
        "";
        "    let main ~args =";
        "      ...";
        "      Ok ()";
        "";
      ]
      @ reason_lines
  | Riot_planner.Planning_error.Exception { exn } ->
      [
        error_line "unexpected planner exception";
        "Riot hit an unexpected exception while planning this package.";
      ]
      @ labeled_multiline_lines ~label:"reason" (Exception.to_string exn)

let build_unit_planning_error_lines = fun __tmp1 ->
  match __tmp1 with
  | Riot_build.Internal.Build_unit_plan.MissingPackages { missing } ->
      let missing_lines =
        missing
        |> List.map
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Riot_planner.Build_unit_graph.Root package ->
                "missing: root -> " ^ Riot_model.Package_name.to_string package
            | Dependency { package; dependency } ->
                "missing: "
                ^ Riot_model.Package_name.to_string package
                ^ " -> "
                ^ Riot_model.Package_name.to_string dependency)
      in
      [
        error_line "missing package dependencies";
        "Riot found package dependency edges that do not point at a loaded workspace or resolved package.";
      ]
      @ missing_lines
      @ [
        "examples:";
        "  - add the missing package to the workspace";
        "  - add a registry, path, or workspace dependency entry for the missing package";
      ]
  | CycleDetected { cycle } ->
      [
        error_line "cyclic build-unit dependency detected";
        "Riot found a cycle in the package artifact graph, so it cannot choose a safe build order.";
        "cycle: " ^ String.concat " -> " (List.map cycle ~fn:Riot_planner.Build_unit.key_to_string);
        "examples:";
        "  - move shared code into a lower-level package";
        "  - remove a build/dev dependency edge that points back to its consumer";
      ]

let out_prefixed_payload = fun ~prefix payload ->
  match String.split payload ~by:"\n" with
  | [] -> ()
  | first_line :: rest ->
      out (prefix ^ first_line);
      rest
      |> List.for_each ~fn:out

let build_package_error_message = fun __tmp1 ->
  match __tmp1 with
  | Riot_model.Event.BuildPlanningFailed { message }
  | Riot_model.Event.BuildExecutionFailed { message }
  | Riot_model.Event.BuildActionExecutionFailed { message } -> message
  | Riot_model.Event.BuildActionOutputsNotCreated { missing } ->
      "missing outputs: " ^ String.concat ", " (List.map missing ~fn:Path.to_string)
  | Riot_model.Event.BuildActionDependenciesFailed { failed } ->
      "failed dependencies: " ^ String.concat ", " failed

let show_target_in_package_labels = fun __tmp1 ->
  match __tmp1 with
  | Some { target_count = Some target_count; _ } -> target_count > 1
  | Some { target_count = None; _ }
  | None -> false

let render_profile = fun ?render_state ?profile () ->
  match profile with
  | Some _ -> profile
  | None -> (
      match render_state with
      | Some state -> state.profile_name
      | None -> None
    )

let display_build_package_name = fun ?render_state ?profile ~build_target package ->
  display_package_name
    ?profile:(render_profile ?render_state ?profile ())
    ~build_target
    ~show_target:(show_target_in_package_labels render_state)
    package

let count_part = fun ?plural count singular ->
  let label =
    if count = 1 then
      singular
    else
      match plural with
      | Some plural -> plural
      | None -> singular
  in
  Int.to_string count ^ " " ^ label

let non_zero_count_part = fun count label ->
  if count = 0 then
    None
  else
    Some (count_part count label)

let build_count_parts = fun
  ~built_count ~cached_count ~skipped_count ~failed_count ?(error_count = 0) () ->
  [
    non_zero_count_part built_count "built";
    non_zero_count_part cached_count "cached";
    non_zero_count_part skipped_count "skipped";
    non_zero_count_part failed_count "failed";
    non_zero_count_part error_count "errored";
  ]
  |> List.filter_map ~fn:(fun value -> value)

let build_count_summary = fun
  ~built_count ~cached_count ~skipped_count ~failed_count ?error_count () ->
  match build_count_parts ~built_count ~cached_count ~skipped_count ~failed_count ?error_count () with
  | [] -> "nothing to do"
  | parts -> String.concat ", " parts

let record_build_event_progress = fun progress event ->
  match event.Riot_model.Event.kind with
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPackageFinished { status = Riot_model.Event.Fresh; _ }
  ) ->
      progress.built_count <- progress.built_count + 1
  | Riot_model.Event.Build (
    Riot_model.Event.BuildPackageFinished { status = Riot_model.Event.Cached; _ }
  ) ->
      progress.cached_count <- progress.cached_count + 1
  | Riot_model.Event.Build (Riot_model.Event.BuildPackageSkippedDetailed _) ->
      progress.skipped_count <- progress.skipped_count + 1
  | Riot_model.Event.Build (Riot_model.Event.BuildPackageFailed _) ->
      progress.failed_count <- progress.failed_count + 1
  | _ -> ()

let command_error_event_to_json = fun kind details ->
  Data.Json.Object (("type", Data.Json.String kind) :: details)

let build_failure_detail_lines = fun (failure: Riot_build.Build_result.failure) ->
  let package_name = Riot_model.Package_name.to_string failure.package_name in
  match failure.reason with
  | Riot_build.Build_result.PackagePlanningFailed planning_error ->
      planning_error_lines planning_error
  | _ -> [ error_line (package_name ^ " failed"); failure.message ]

let write_failure_blocks = fun failures ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | [ failure ] ->
        out "";
        build_failure_detail_lines failure
        |> List.for_each ~fn:out;
        out ""
    | failure :: rest ->
        out "";
        build_failure_detail_lines failure
        |> List.for_each ~fn:out;
        loop rest
  in
  loop failures
