open Std
open Riot_model
open Std.Result.Syntax

let generator_signature = "riot-doc:v30"

type request = {
  workspace: Riot_model.Workspace.t;
  package_name: string option;
  all: bool;
  release: bool;
  output_root: Path.t option;
  force: bool;
  no_cache: bool;
}

type generation = {
  package: Package_name.t;
  version: string;
  output_dir: Path.t;
  cache_hit: bool;
  cache_key: string;
}

type event =
  | PackageGenerationStarted of {
      package: Package_name.t;
      version: string;
      output_dir: Path.t;
    }
  | PackageGenerationFailed of {
      package: Package_name.t;
      version: string;
      output_dir: Path.t;
      error: string;
    }
  | PackageGenerationCompleted of generation

let generation_to_json = fun (summary: generation) ->
  Data.Json.Object [
    ("package", Data.Json.String (Package_name.to_string summary.package));
    ("version", Data.Json.String summary.version);
    ("output_dir", Data.Json.String (Path.to_string summary.output_dir));
    ("cache_hit", Data.Json.Bool summary.cache_hit);
    ("cache_key", Data.Json.String summary.cache_key);
  ]

let event_to_json = fun __tmp1 ->
  match __tmp1 with
  | PackageGenerationStarted { package; version; output_dir } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "doc.package_generation_started");
        ("package", Data.Json.String (Package_name.to_string package));
        ("version", Data.Json.String version);
        ("output_dir", Data.Json.String (Path.to_string output_dir));
      ])
  | PackageGenerationFailed {
      package;
      version;
      output_dir;
      error;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "doc.package_generation_failed");
        ("package", Data.Json.String (Package_name.to_string package));
        ("version", Data.Json.String version);
        ("output_dir", Data.Json.String (Path.to_string output_dir));
        ("error", Data.Json.String error);
      ])
  | PackageGenerationCompleted summary ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "doc.package_generation_completed");
        ("summary", generation_to_json summary);
      ])

let resolve_profile = fun release ->
  if release then
    "release"
  else
    "debug"

let output_version = fun ~release (package: Riot_model.Package.t) ->
  if release then
    match package.publish.version with
    | Some version -> Ok (Version.to_string version)
    | None ->
        Error ("--release requires [package.publish.version] in "
        ^ Package_name.to_string package.name)
  else
    Ok "dev"

let output_root = fun ~(workspace:Riot_model.Workspace.t) ~release ->
  let _ = release in
  Path.(workspace.target_dir_root / Path.v "doc")

let output_root_for_request = fun (request: request) ->
  match request.output_root with
  | Some override_root -> override_root
  | None -> output_root ~workspace:request.workspace ~release:request.release

let package_output_dir = fun
  ~(workspace:Riot_model.Workspace.t) ~package_name ~version ~release ~output_root_opt ->
  let root =
    match output_root_opt with
    | Some override_root -> override_root
    | None -> output_root ~workspace ~release
  in
  Path.(root / Path.v package_name / Path.v version)

let read_lockfile = fun workspace_root ->
  match Riot_deps.Lockfile_store.read ~workspace_root with
  | Ok None -> Ok None
  | Ok (Some lockfile) -> Ok (Some lockfile)
  | Error _ -> Ok None

let workspace_release_versions = fun (workspace: Riot_model.Workspace.t) ->
  workspace.packages
  |> List.filter_map
    ~fn:(fun (pkg: Riot_model.Package_manifest.t) ->
      match pkg.publish.version with
      | Some version -> Some (pkg.name, Version.to_string version)
      | None -> None)

let find_lock_package = fun ~(package:Riot_model.Package.t) (lockfile: Riot_model.Lockfile.t) ->
  let release_version =
    package.publish.version
    |> Option.map ~fn:Version.to_string
  in
  let matching_name =
    lockfile.packages
    |> List.filter
      ~fn:(fun (lock_package: Riot_model.Lockfile.package) ->
        Package_name.equal
          lock_package.id.name
          package.name)
  in
  match release_version with
  | Some version ->
      let exact_match =
        List.find
          matching_name
          ~fn:(fun (lock_package: Riot_model.Lockfile.package) ->
            match lock_package.id.version with
            | Some lock_version -> String.equal lock_version version
            | None -> false)
      in
      (
        match (exact_match, matching_name) with
        | (Some lock_package, _) -> Some lock_package
        | (None, [ lock_package ]) -> Some lock_package
        | (None, _) -> None
      )
  | None ->
      match matching_name with
      | [] -> None
      | lock_package :: _ -> Some lock_package

let locked_dependency_versions = fun
  ~(workspace:Riot_model.Workspace.t) ~(package:Riot_model.Package.t) lockfile_opt ->
  if not (Package.is_workspace_member package) then
    Ok []
  else
    let workspace_versions = workspace_release_versions workspace in
    match lockfile_opt with
    | None ->
        Error ("--release requires a lockfile to resolve documentation dependency versions for "
        ^ Package_name.to_string package.name)
    | Some lockfile -> (
        match find_lock_package ~package lockfile with
        | None ->
            Error ("--release could not find a lockfile entry for package "
            ^ Package_name.to_string package.name)
        | Some lock_package ->
            let resolved =
              lock_package.dependencies
              |> List.filter_map
                ~fn:(fun (dependency: Riot_model.Lockfile.dependency) ->
                  match dependency.package.version with
                  | Some version -> Some (dependency.name, version)
                  | None ->
                      List.find
                        workspace_versions
                        ~fn:(fun (name, _) -> Package_name.equal name dependency.name)
                      |> Option.map ~fn:(fun (_, version) -> (dependency.name, version)))
            in
            let resolved =
              resolved
              |> List.unique
                ~compare:(fun (left_name, _) (right_name, _) ->
                  Package_name.compare
                    left_name
                    right_name)
            in
            let missing =
              package.dependencies
              |> List.filter
                ~fn:(fun (dependency: Riot_model.Package.dependency) ->
                  not
                    (Package.is_builtin_dependency dependency))
              |> List.filter_map
                ~fn:(fun (dependency: Riot_model.Package.dependency) ->
                  match List.find
                    resolved
                    ~fn:(fun (name, _) -> Package_name.equal name dependency.name) with
                  | Some _ -> None
                  | None -> Some dependency.name)
            in
            match missing with
            | [] -> Ok resolved
            | names ->
                Error ("--release could not resolve locked versions for dependencies of "
                ^ Package_name.to_string package.name
                ^ ": "
                ^ String.concat ", " (List.map names ~fn:Package_name.to_string))
      )

let dependency_link_for = fun ~release dependency_map dependency ->
  if release then
    match List.find dependency_map ~fn:(fun (name, _) -> Package_name.equal name dependency) with
    | Some (_, version) ->
        Ok {
          Doctree.name = Package_name.to_string dependency;
          version = Some version;
          url = "../../" ^ Package_name.to_string dependency ^ "/" ^ version ^ "/index.html";
        }
    | None ->
        Error ("--release could not resolve a versioned documentation link for dependency "
        ^ Package_name.to_string dependency)
  else
    Ok {
      Doctree.name = Package_name.to_string dependency;
      version = None;
      url = "../../" ^ Package_name.to_string dependency ^ "/dev/index.html";
    }

let documentation_dependencies = fun (package: Riot_model.Package.t) ->
  package.dependencies
  |> List.filter
    ~fn:(fun (dependency: Riot_model.Package.dependency) ->
      not
        (Package.is_builtin_dependency dependency))

let map_dependencies = fun
  ~release ~(dependency_map:(Package_name.t * string) list) (package: Riot_model.Package.t) ->
  documentation_dependencies package
  |> List.unique
    ~compare:(fun (left: Riot_model.Package.dependency) (right: Riot_model.Package.dependency) ->
      Package_name.compare
        left.name
        right.name)
  |> List.fold_left
    ~init:(Ok [])
    ~fn:(fun acc (dependency: Riot_model.Package.dependency) ->
      let* links = acc in
      let* link = dependency_link_for ~release dependency_map dependency.name in
      Ok (link :: links))
  |> Result.map ~fn:List.reverse

let dependency_signature = fun dependency_map ->
  let state = Crypto.Sha256.create () in
  List.sort
    dependency_map
    ~compare:(fun (left_name, _) (right_name, _) -> Package_name.compare left_name right_name)
  |> List.for_each
    ~fn:(fun (name, version) ->
      Crypto.Sha256.write state (Package_name.to_string name);
      Crypto.Sha256.write state version);
  Crypto.Digest.hex (Crypto.Sha256.finish state)

let cache_key = fun
  ~request
  ~(package:Riot_model.Package.t)
  ~package_version
  ~source_signature
  ~dependency_signature ->
  let state = Crypto.Sha256.create () in
  Crypto.Sha256.write state generator_signature;
  Crypto.Sha256.write state (resolve_profile request.release);
  Crypto.Sha256.write state (Path.to_string request.workspace.root);
  Crypto.Sha256.write state (Package_name.to_string package.name);
  Crypto.Sha256.write state package_version;
  Crypto.Sha256.write state source_signature;
  Crypto.Sha256.write state dependency_signature;
  Package.hash state package;
  Crypto.Digest.hex (Crypto.Sha256.finish state)

let write_output = fun ~path content ->
  let* () =
    match Path.parent path with
    | Some parent ->
        Fs.create_dir_all parent
        |> Result.map_err ~fn:IO.error_message
    | None -> Ok ()
  in
  Fs.write content path
  |> Result.map_err ~fn:IO.error_message

let sanitize_output_path = fun path ->
  match Fs.exists path with
  | Ok true -> (
      match Fs.is_dir path with
      | Ok true ->
          Fs.remove_dir_all path
          |> Result.map_err
            ~fn:(fun err ->
              "failed to clear output dir " ^ Path.to_string path ^ ": " ^ IO.error_message err)
      | Ok false ->
          Fs.remove_file path
          |> Result.map_err
            ~fn:(fun err ->
              "failed to remove output path " ^ Path.to_string path ^ ": " ^ IO.error_message err)
      | Error err ->
          Error ("failed to inspect output path "
          ^ Path.to_string path
          ^ ": "
          ^ IO.error_message err)
    )
  | Ok false -> Ok ()
  | Error err ->
      Error ("failed to check output path " ^ Path.to_string path ^ ": " ^ IO.error_message err)

let emit = fun ~on_event event -> on_event event

let selected_packages = fun (request: request) ->
  let workspace_members =
    Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Doc request.workspace
    |> List.filter ~fn:(fun (pkg: Riot_model.Package.t) -> Package.is_workspace_member pkg)
  in
  match (request.package_name, request.all) with
  | (Some name, _) -> (
      match Package_name.from_string name with
      | Error _ ->
          Error (
            "package not found: "
            ^ name
            ^ ". available packages: "
            ^ (
              workspace_members
              |> List.map ~fn:(fun (pkg: Riot_model.Package.t) -> Package_name.to_string pkg.name)
              |> String.concat ", "
            )
          )
      | Ok package_name -> (
          match List.find
            workspace_members
            ~fn:(fun (pkg: Riot_model.Package.t) ->
              Package_name.equal pkg.name package_name && Package.is_workspace_member pkg) with
          | Some pkg -> Ok [ pkg ]
          | None ->
              Error (
                "package not found: "
                ^ name
                ^ ". available packages: "
                ^ (
                  workspace_members
                  |> List.map
                    ~fn:(fun (pkg: Riot_model.Package.t) -> Package_name.to_string pkg.name)
                  |> String.concat ", "
                )
              )
        )
    )
  | (None, true) -> Ok workspace_members
  | (None, false) ->
      if workspace_members = [] then
        Error "no workspace packages found"
      else
        Ok workspace_members

let workspace_plan_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Riot_planner.Workspace_planner.PackageNotFound { name; available } ->
      "package not found: "
      ^ Package_name.to_string name
      ^ ". available packages: "
      ^ String.concat ", " (List.map available ~fn:Package_name.to_string)
  | Riot_planner.Workspace_planner.PackagesNotFound { names; available } ->
      "packages not found: "
      ^ String.concat ", " (List.map names ~fn:Package_name.to_string)
      ^ ". available packages: "
      ^ String.concat ", " (List.map available ~fn:Package_name.to_string)
  | Riot_planner.Workspace_planner.CycleDetected { cycle } ->
      "package cycle detected: " ^ String.concat " -> " cycle
  | Riot_planner.Workspace_planner.MissingDependencies { missing } ->
      "missing dependencies: "
      ^ String.concat
        "; "
        (List.map
          missing
          ~fn:(fun (dep: Riot_planner.Package_graph.missing_dependency) ->
            dep.package ^ " -> " ^ dep.dependency))
  | Riot_planner.Workspace_planner.PackageLoadFailed { errors } ->
      "package load failed: " ^ String.concat
        "; "
        (
          List.map
            errors
            ~fn:(fun err ->
              match err with
              | Workspace_manager.PackageNotFound { package; path; dependant } -> (
                  match dependant with
                  | None -> "missing package: " ^ package ^ " (" ^ path ^ ")"
                  | Some parent ->
                      "missing package: " ^ package ^ " required by " ^ parent ^ " (" ^ path ^ ")"
                )
              | Workspace_manager.PackageTomlReadFailed { package; path } ->
                  "failed to read package toml: " ^ package ^ " (" ^ path ^ ")"
              | Workspace_manager.PackageTomlParseFailed { package; path } ->
                  "failed to parse package toml: " ^ package ^ " (" ^ path ^ ")"
              | Workspace_manager.PackageFromTomlFailed { package; path; error } ->
                  "failed to parse package toml for "
                  ^ package
                  ^ " ("
                  ^ path
                  ^ "): "
                  ^ Package_manifest.error_message error)
        )

let planner_target_for_selected_packages = fun request packages ->
  match (request.package_name, packages) with
  | (Some _, [ package ]) -> Ok (Riot_planner.Workspace_planner.Package package.Package.name)
  | (Some name, _) -> Error ("package not found: " ^ name)
  | (None, _) -> Ok Riot_planner.Workspace_planner.All

let plan_workspace_for_docs = fun request packages ->
  let* target = planner_target_for_selected_packages request packages in
  let dev_artifacts: Riot_planner.Package_graph.dev_artifacts = {
    tests = false;
    examples = false;
    benches = false;
  }
  in
  Riot_planner.plan_workspace
    ~workspace:request.workspace
    ~target
    ~scope:Riot_planner.Package_graph.Runtime
    ~load_errors:[]
    ~dev_artifacts
  |> Result.map_err ~fn:workspace_plan_error_to_string

let dependency_packages_for = fun (plan: Riot_planner.workspace_plan_result) (package: Package.t) ->
  Riot_planner.Package_graph.direct_runtime_dependencies
    plan.package_graph
    package.name

let shared_assets_dir = fun request -> Path.(output_root_for_request request / Path.v "_shared")

let write_shared_assets = fun output_dir ->
  Html.assets
  |> List.fold_left
    ~init:(Ok ())
    ~fn:(fun acc (relative_path, content) ->
      match acc with
      | Error _ as err -> err
      | Ok () ->
          let path = Path.(output_dir / Path.v relative_path) in
          write_output ~path content)

let write_pages = fun ~output_dir package_doc ->
  Doctree.flatten_modules package_doc.Doctree.modules
  |> List.fold_left
    ~init:(Ok [])
    ~fn:(fun acc module_doc ->
      match acc with
      | Error _ as err -> err
      | Ok paths ->
          let path = Doctree.module_output_path ~output_dir module_doc in
          let source_path = Doctree.module_source_output_path ~output_dir module_doc in
          let body = Html.render_module package_doc module_doc in
          let source_body = Html.render_module_source package_doc module_doc in
          let* () = write_output ~path body in
          let* () = write_output ~path:source_path source_body in
          Ok (source_path :: path :: paths))

let write_index = fun ~output_dir package_doc ->
  let path = Path.(output_dir / Path.v "index.html") in
  let body = Html.render_index package_doc in
  let* () = write_output ~path body in
  Ok path

let relative_output_path = fun ~output_dir path ->
  match Path.strip_prefix path ~prefix:output_dir with
  | Ok relative -> Path.to_string relative
  | Error _ -> Path.to_string path

type existing_manifest = {
  manifest_cache_key: string;
  manifest_outputs: string list;
}

let manifest_path = fun output_dir -> Path.(output_dir / Path.v "manifest.json")

let json_string_field = fun name json ->
  match Data.Json.get_field name json with
  | Some (Data.Json.String value) -> Some value
  | _ -> None

let json_string_array_field = fun name json ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Some (List.reverse acc)
    | (Data.Json.String value) :: rest -> loop (value :: acc) rest
    | _ -> None
  in
  match Data.Json.get_field name json with
  | Some (Data.Json.Array values) -> loop [] values
  | _ -> None

let existing_manifest_of_json = fun json ->
  match (
    json_string_field "schema" json,
    json_string_field "generator" json,
    json_string_field "cache_key" json,
    json_string_array_field "outputs" json
  ) with
  | (Some "riot-doc.manifest.v1", Some generator, Some manifest_cache_key, Some manifest_outputs) ->
      if String.equal generator generator_signature then
        Some { manifest_cache_key; manifest_outputs }
      else
        None
  | _ -> None

let read_existing_manifest = fun output_dir ->
  let path = manifest_path output_dir in
  match Fs.exists path with
  | Ok true -> (
      match Fs.read path with
      | Ok content -> (
          match Data.Json.of_string content with
          | Ok json -> existing_manifest_of_json json
          | Error _ -> None
        )
      | Error _ -> None
    )
  | Ok false
  | Error _ -> None

let existing_manifest_outputs_exist = fun ~output_dir outputs ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> true
    | relative_path :: rest -> (
        match Fs.exists Path.(output_dir / Path.v relative_path) with
        | Ok true -> loop rest
        | Ok false
        | Error _ -> false
      )
  in
  loop outputs

let existing_output_matches = fun ~output_dir ~cache_key ->
  match read_existing_manifest output_dir with
  | Some manifest ->
      String.equal manifest.manifest_cache_key cache_key
      && existing_manifest_outputs_exist ~output_dir manifest.manifest_outputs
  | None -> false

let manifest_json = fun
  ~profile ~package ~version ~cache_key ~source_signature ~dependency_signature ~outputs ->
  Data.Json.Object [
    ("schema", Data.Json.String "riot-doc.manifest.v1");
    ("generator", Data.Json.String generator_signature);
    ("package", Data.Json.String (Package_name.to_string package));
    ("version", Data.Json.String version);
    ("profile", Data.Json.String profile);
    ("cache_key", Data.Json.String cache_key);
    ("source_signature", Data.Json.String source_signature);
    ("dependency_signature", Data.Json.String dependency_signature);
    ("outputs", Data.Json.Array (
      outputs
      |> List.map ~fn:(fun path -> Data.Json.String path)
    ));
  ]

let write_manifest = fun
  ~output_dir
  ~profile
  ~package
  ~version
  ~cache_key
  ~source_signature
  ~dependency_signature
  ~outputs ->
  let path = manifest_path output_dir in
  let relative_outputs =
    (path :: outputs)
    |> List.map ~fn:(relative_output_path ~output_dir)
    |> List.sort ~compare:String.compare
  in
  let content =
    manifest_json
      ~profile
      ~package
      ~version
      ~cache_key
      ~source_signature
      ~dependency_signature
      ~outputs:relative_outputs
    |> Data.Json.to_string_pretty
    |> fun json -> json ^ "\n"
  in
  let* () = write_output ~path content in
  Ok path

let path_entry_name = fun path ->
  Path.basename path
  |> Path.v
  |> Path.remove_extension
  |> Path.to_string

let executable_entry = fun (binary: Package.binary) ->
  Doctree.{
    name = binary.name;
    summary = None;
    meta = Some (Path.to_string binary.path);
    href = None;
  }

let is_runtime_binary = fun (binary: Package.binary) ->
  let path = Path.to_string binary.path in
  not
    (String.starts_with ~prefix:"tests/" path
    || String.starts_with ~prefix:"examples/" path
    || String.starts_with ~prefix:"bench/" path)

let command_entry = fun (command: Package_command.t) ->
  Doctree.{
    name = command.name;
    summary = Some command.description;
    meta = Some ("module " ^ command.command_module);
    href = None;
  }

let lint_rule_entries = fun (providers: Fix_provider.t list) ->
  providers
  |> List.fold_left
    ~init:[]
    ~fn:(fun acc (provider: Fix_provider.t) ->
      provider.rules
      |> List.fold_left
        ~init:acc
        ~fn:(fun acc rule ->
          Doctree.{
            name = rule;
            summary = None;
            meta = Some ("provider " ^ provider.name);
            href = None;
          }
          :: acc))
  |> List.sort ~compare:(fun left right -> String.compare left.Doctree.name right.Doctree.name)

let example_entry = fun path ->
  Doctree.{
    name = path_entry_name path;
    summary = None;
    meta = Some (Path.to_string path);
    href = None;
  }

let package_doc_metadata = fun (package: Riot_model.Package.t) -> (
  List.map package.commands ~fn:command_entry,
  package.binaries
  |> List.filter ~fn:is_runtime_binary
  |> List.map ~fn:executable_entry,
  lint_rule_entries package.fix_providers,
  List.map package.sources.examples ~fn:example_entry
)

let package_doc_of_sources = fun ~package ~version ~dependencies sources ->
  let package_name = Package_name.to_string package.Package.name in
  let (commands, executables, lint_rules, examples) = package_doc_metadata package in
  let lookup = Source.build_lookup sources in
  match Source.find_root_interface ~package_name sources with
  | Some root_source ->
      let* module_doc = Transform.of_interface_source ~lookup root_source in
      Ok {
        Doctree.package = package_name;
        version;
        modules = [ module_doc ];
        commands;
        executables;
        lint_rules;
        examples;
        dependencies;
      }
  | None ->
      let rec loop acc = fun __tmp1 ->
        match __tmp1 with
        | [] ->
            Ok {
              Doctree.package = package_name;
              version;
              modules = List.reverse acc;
              commands;
              executables;
              lint_rules;
              examples;
              dependencies;
            }
        | source :: rest ->
            let* module_doc = Transform.of_interface_source ~lookup source in
            loop (module_doc :: acc) rest
      in
      loop [] sources

let run_for_package = fun
  ~on_event
  ~store
  ~cache_allowed
  ~request
  ~workspace_plan
  ~(package:Riot_model.Package.t)
  ~lockfile_opt ->
  let* version = output_version ~release:request.release package in
  let dependency_packages = dependency_packages_for workspace_plan package in
  let output_dir =
    package_output_dir
      ~workspace:request.workspace
      ~package_name:(Package_name.to_string package.name)
      ~version
      ~release:request.release
      ~output_root_opt:request.output_root
  in
  let () =
    emit ~on_event (PackageGenerationStarted { package = package.name; version; output_dir })
  in
  let result =
    let* sources =
      Source.collect_interfaces
        ~workspace:request.workspace
        ~store
        ~dependency_packages
        ~release:request.release
        package
    in
    if sources = [] then
      Error ("no interface files found for package " ^ Package_name.to_string package.name)
    else
      let* dependency_map =
        if request.release then
          locked_dependency_versions ~workspace:request.workspace ~package lockfile_opt
        else
          Ok []
      in
      let source_signature = Source.source_signature sources in
      let dependency_signature = dependency_signature dependency_map in
      let cache_key =
        cache_key ~request ~package ~package_version:version ~source_signature ~dependency_signature
      in
      let cache_hit_ref = ref false in
      let* () =
        if not request.force && existing_output_matches ~output_dir ~cache_key then (
          cache_hit_ref := true;
          Ok ()
        ) else if cache_allowed && not request.force then
          match Riot_store.Store.get store (Crypto.hash_string cache_key) with
          | Some _ ->
              let* () =
                Riot_store.Store.promote store (Crypto.hash_string cache_key) ~target_dir:output_dir
                |> Result.map_err
                  ~fn:(fun err ->
                    "failed to promote cache hit: " ^ Riot_store.Store.error_message err)
              in
              cache_hit_ref := true;
              Ok ()
          | None -> Ok ()
        else
          Ok ()
      in
      if !cache_hit_ref then
        let summary = {
          package = package.name;
          version;
          output_dir;
          cache_hit = true;
          cache_key;
        }
        in
        let () = emit ~on_event (PackageGenerationCompleted summary) in
        Ok summary
      else
        let* dependencies = map_dependencies ~release:request.release ~dependency_map package in
        let* package_doc = package_doc_of_sources ~package ~version ~dependencies sources in
        let* () = sanitize_output_path output_dir in
        let* index_path = write_index ~output_dir package_doc in
        let* page_paths = write_pages ~output_dir package_doc in
        let output_paths = [ index_path ] @ page_paths in
        let* manifest_path =
          write_manifest
            ~output_dir
            ~profile:(resolve_profile request.release)
            ~package:package.name
            ~version
            ~cache_key
            ~source_signature
            ~dependency_signature
            ~outputs:output_paths
        in
        let* () =
          if cache_allowed then
            Riot_store.Store.save
              ~package:(Package_name.to_string package.name)
              ~input_hash:(Crypto.hash_string cache_key)
              store
              ~sandbox_dir:output_dir
              ~outs:(manifest_path :: output_paths)
            |> Result.map_err
              ~fn:(fun err -> "failed to save cached docs: " ^ Riot_store.Store.error_message err)
            |> Result.map ~fn:(fun _ -> ())
          else
            Ok ()
        in
        let summary = {
          package = package.name;
          version;
          output_dir;
          cache_hit = false;
          cache_key;
        }
        in
        let () = emit ~on_event (PackageGenerationCompleted summary) in
        Ok summary
  in
  match result with
  | Ok _ as ok -> ok
  | Error error ->
      let () =
        emit
          ~on_event
          (
            PackageGenerationFailed {
              package = package.name;
              version;
              output_dir;
              error;
            }
          )
      in
      Error error

let run = fun ?on_event (request: request) ->
  let on_event = Option.unwrap_or ~default:(fun _ -> ()) on_event in
  let* lockfile_opt = read_lockfile request.workspace.root in
  let* packages = selected_packages request in
  let* workspace_plan = plan_workspace_for_docs request packages in
  let store =
    Riot_store.Store.create_for_lane
      ~workspace:request.workspace
      ~profile:(resolve_profile request.release)
      ~target:(Riot_dirs.host_target ())
  in
  let cache_allowed = not request.no_cache in
  let* () = write_shared_assets (shared_assets_dir request) in
  packages
  |> List.fold_left
    ~init:(Ok [])
    ~fn:(fun acc package ->
      let* summaries = acc in
      let* summary =
        run_for_package
          ~on_event
          ~store
          ~cache_allowed
          ~request
          ~workspace_plan
          ~package
          ~lockfile_opt
      in
      Ok (summary :: summaries))
  |> Result.map ~fn:List.reverse
