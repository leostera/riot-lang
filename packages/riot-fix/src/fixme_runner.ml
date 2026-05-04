open Std

type generated_provider = {
  provider: Riot_model.Fix_provider.t;
  module_name: string;
  copied_source_path: Path.t;
  support_module_sources: (string * Path.t) list;
}

type plan = {
  provider_hash: string;
  generated_dir: Path.t;
  package_dir: Path.t;
  src_dir: Path.t;
  providers_dir: Path.t;
  library_path: Path.t;
  main_path: Path.t;
  binary_path: Path.t;
  package_name: Riot_model.Package_name.t;
  binary_name: string;
  package: Riot_model.Package.t;
  providers: generated_provider list;
}

let generator_version = "v6"

let sanitize_component = fun text ->
  String.map
    text
    ~fn:(fun ch ->
      if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') then
        ch
      else
        '_')

let generated_module_name = fun (provider: Riot_model.Fix_provider.t) ->
  "Provider_"
  ^ sanitize_component (Riot_model.Package_name.to_string provider.package_name)
  ^ "_"
  ^ sanitize_component provider.name

let ocaml_module_name_of_path = fun path ->
  let base =
    Path.basename path
    |> Path.v
    |> Path.remove_extension
    |> Path.to_string
  in
  if String.length base = 0 then
    "Generated"
  else
    String.uppercase_ascii (String.sub base ~offset:0 ~len:1)
    ^ String.sub base ~offset:1 ~len:(String.length base - 1)

let support_module_sources = fun (provider: Riot_model.Fix_provider.t) ->
  let provider_dir = Path.dirname provider.source_path in
  let provider_basename = Path.basename provider.source_path in
  match Fs.read_dir provider_dir with
  | Error _ -> []
  | Ok iter ->
      Std.Iter.MutIterator.to_list iter
      |> List.filter_map
        ~fn:(fun entry ->
          let source_path = Path.(provider_dir / entry) in
          let entry_name = Path.basename source_path in
          if
            String.equal entry_name provider_basename
            || not (String.ends_with ~suffix:".ml" entry_name)
          then
            None
          else
            Some (ocaml_module_name_of_path source_path, source_path))
      |> List.sort
        ~compare:(fun (left_name, left_path) (right_name, right_path) ->
          match String.compare left_name right_name with
          | Order.EQ -> String.compare (Path.to_string left_path) (Path.to_string right_path)
          | cmp -> cmp)

let file_content_hash = fun path ->
  match Fs.read path with
  | Ok source ->
      Crypto.hash_string source
      |> Crypto.Digest.hex
  | Error _ -> "missing"

let provider_fingerprint = fun (provider: Riot_model.Fix_provider.t) ->
  let support_hashes =
    support_module_sources provider
    |> List.map
      ~fn:(fun (module_name, source_path) ->
        module_name ^ ":" ^ Path.to_string source_path ^ ":" ^ file_content_hash source_path)
    |> String.concat ","
  in
  String.concat
    ":"
    [
      Riot_model.Package_name.to_string provider.package_name;
      provider.name;
      Path.to_string provider.source_path;
      file_content_hash provider.source_path;
      support_hashes;
      String.concat "," provider.rules;
    ]

let provider_hash = fun providers ->
  providers
  |> List.sort
    ~compare:(fun (left: Riot_model.Fix_provider.t) right ->
      String.compare
        (provider_fingerprint left)
        (provider_fingerprint right))
  |> List.map ~fn:provider_fingerprint
  |> fun fingerprints ->
    String.concat "\n" (generator_version :: fingerprints)
    |> Crypto.hash_string
    |> Crypto.Digest.hex

let generated_provider = fun plan provider ->
  let module_name = generated_module_name provider in
  {
    provider;
    module_name;
    copied_source_path = Path.(plan.providers_dir / Path.v (module_name ^ ".ml"));
    support_module_sources = support_module_sources provider;
  }

let relative_path_for_package = fun ~workspace_root package_dir ->
  match Path.strip_prefix package_dir ~prefix:workspace_root with
  | Ok relative -> relative
  | Error _ -> package_dir

let provider_module_line = fun (provider: generated_provider) ->
  "    (module " ^ provider.module_name ^ " : Riot_fix.Provider.S);"

let registry_source = fun providers ->
  let provider_lines =
    providers
    |> List.map
      ~fn:(fun (provider: Riot_model.Fix_provider.t) ->
        "    (module " ^ generated_module_name provider ^ " : Riot_fix.Provider.S);")
  in
  String.concat
    "\n"
    [
      "open Std";
      "";
      "let register () =";
      "  Riot_fix.Provider_registry.register_providers";
      "    [";
      String.concat "\n" provider_lines;
      "    ]";
      "";
    ]

let embedded_provider_module_source = fun (provider: generated_provider) ->
  let source =
    Fs.read provider.provider.source_path
    |> Result.expect
      ~msg:("failed to read provider source " ^ Path.to_string provider.provider.source_path)
  in
  String.concat
    "\n"
    [
      "module " ^ provider.module_name ^ " = struct";
      String.concat
        "\n"
        (
          List.map
            provider.support_module_sources
            ~fn:(fun (module_name, source_path) ->
              let source =
                Fs.read source_path
                |> Result.expect
                  ~msg:("failed to read provider support source " ^ Path.to_string source_path)
              in
              String.concat "\n" [ "module " ^ module_name ^ " = struct"; source; "end"; ""; ])
        );
      source;
      "end";
      "";
    ]

let dependency_entries = fun workspace_root providers ->
  let workspace_packages =
    let workspace_manager = Riot_model.Workspace_manager.create () in
    match Riot_model.Workspace_manager.scan workspace_manager workspace_root with
    | Ok (workspace, _errors) -> Riot_model.Workspace_manifest.(workspace.packages)
    | Error _ -> []
  in
  let resolve_dependency_path ~package_path path =
    if Path.is_absolute path then
      Path.normalize path
    else
      Path.normalize Path.(package_path / path)
  in
  let workspace_package_path name =
    workspace_packages
    |> List.find
      ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> Riot_model.Package_name.equal pkg.name name)
    |> Option.map ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.path)
  in
  let provider_build_deps =
    providers
    |> List.map
      ~fn:(fun ({ provider; _ }: generated_provider) ->
        workspace_packages
        |> List.find
          ~fn:(fun (pkg: Riot_model.Package_manifest.t) ->
            Riot_model.Package_name.equal
              pkg.name
              provider.package_name)
        |> Option.map
          ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> (pkg.path, pkg.build_dependencies))
        |> Option.unwrap_or ~default:(provider.package_path, [])
        |> fun (package_path, build_dependencies) ->
          build_dependencies
          |> List.filter_map
            ~fn:(fun (dep: Riot_model.Package.dependency) ->
              match dep.source with
              | { workspace = true; _ } ->
                  workspace_package_path dep.name
                  |> Option.map ~fn:(fun path -> (dep.name, path))
              | { builtin = true; _ } -> None
              | { path = Some path; _ } ->
                  let path =
                    match workspace_package_path dep.name with
                    | Some workspace_path -> workspace_path
                    | None -> resolve_dependency_path ~package_path path
                  in
                  Some (dep.name, path)
              | { path = None; _ } -> None))
    |> List.concat
  in
  let package_name value =
    Riot_model.Package_name.from_string value
    |> Result.expect ~msg:("expected valid package name: " ^ value)
  in
  let entries =
    ([
      (package_name "std", Path.(workspace_root / Path.v "packages" / Path.v "std"));
      (package_name "syn", Path.(workspace_root / Path.v "packages" / Path.v "syn"));
      (package_name "riot-fix", Path.(workspace_root / Path.v "packages" / Path.v "riot-fix"));
      (package_name "fixme", Path.(workspace_root / Path.v "packages" / Path.v "fixme"));
    ]
    @ provider_build_deps)
    @ List.map
      providers
      ~fn:(fun ({ provider; _ }: generated_provider) -> (
        provider.package_name,
        provider.package_path
      ))
  in
  let rec dedupe_by_name seen acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | (name, path) :: rest ->
        if List.any seen ~fn:(Riot_model.Package_name.equal name) then
          dedupe_by_name seen acc rest
        else
          dedupe_by_name (name :: seen) ((name, path) :: acc) rest
  in
  dedupe_by_name [] [] entries

let dependency_of_entry = fun (name, path) ->
  Riot_model.Package.{
    name;
    source =
      {
        workspace = false;
        builtin = false;
        path = Some path;
        source_locator = None;
        ref_ = None;
        version = Some Std.Version.any;
      };
  }

let package_dependencies = fun ~workspace_root providers ->
  dependency_entries workspace_root providers
  |> List.map ~fn:dependency_of_entry

let plan = fun ~workspace_root ~target_dir_root providers ->
  let package_name =
    Riot_model.Package_name.from_string "fixme-runner"
    |> Result.expect ~msg:"expected generated fixme runner package name to be valid"
  in
  let hash = provider_hash providers in
  let generated_dir =
    Path.(target_dir_root / Path.v "riot-fix" / Path.v "fixme-runner" / Path.v hash)
  in
  let package_dir = Path.(generated_dir / Path.v "package") in
  let src_dir = Path.(package_dir / Path.v "src") in
  let providers_dir = Path.(generated_dir / Path.v "providers") in
  let binary_name = "fixme-runner" in
  let binary_path =
    Path.(target_dir_root
    / Path.v "release"
    / Path.v (Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()))
    / Path.v "out"
    / Path.v (Riot_model.Package_name.to_string package_name)
    / Path.v binary_name)
  in
  let placeholder_package =
    Riot_model.Package.synthetic
      ~name:package_name
      ~path:package_dir
      ~relative_path:(relative_path_for_package ~workspace_root package_dir)
  in
  let plan = {
    provider_hash = hash;
    generated_dir;
    package_dir;
    src_dir;
    providers_dir;
    library_path = Path.(src_dir / Path.v "fixme_runner.ml");
    main_path = Path.(src_dir / Path.v "main.ml");
    binary_path;
    package_name;
    binary_name;
    package = placeholder_package;
    providers = [];
  }
  in
  let providers = List.map providers ~fn:(generated_provider plan) in
  let package =
    Riot_model.Package.make
      ~name:package_name
      ~path:package_dir
      ~relative_path:(relative_path_for_package ~workspace_root package_dir)
      ~dependencies:(package_dependencies ~workspace_root providers)
      ~binaries:[ Riot_model.Package.{ name = binary_name; path = Path.v "src/main.ml" } ]
      ~library:Riot_model.Package.{ path = Path.v "src/fixme_runner.ml" }
      ~sources:Riot_model.Package.{
        src = [ Path.v "src/fixme_runner.ml"; Path.v "src/main.ml" ];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }
      ()
  in
  { plan with package; providers }

let library_source = fun plan ->
  String.concat
    "\n"
    [
      "open Std";
      "";
      String.concat "\n" (List.map plan.providers ~fn:embedded_provider_module_source);
      "let print_response_output response =";
      "  match Riot_fix.response_output response with";
      "  | Some output ->";
      "      print output;";
      "      (";
      "        match response with";
      "        | Riot_fix.ListedRules { format=Riot_fix.Reporter.Text; _ }";
      "        | Riot_fix.ListedDiagnostics { format=Riot_fix.Reporter.Text; _ }";
      "        | Riot_fix.ExplainedRule _ -> print \"\\n\"";
      "        | Riot_fix.ListedRules { format=Riot_fix.Reporter.Json; _ }";
      "        | Riot_fix.ListedDiagnostics { format=Riot_fix.Reporter.Json; _ }";
      "        | Riot_fix.Completed -> ()";
      "      );";
      "      Ok ()";
      "  | None -> Ok ()";
      "";
      "let run_generated_request ~args =";
      "  match ArgParser.get_matches Riot_fix.Cli.command args with";
      "  | Error err ->";
      "      ArgParser.print_error err;";
      "      ArgParser.print_help Riot_fix.Cli.command;";
      "      Error (Failure \"Argument parsing failed\")";
      "  | Ok matches -> (";
      "      match Riot_fix.fix_request_of_matches matches with";
      "      | Error _ as err -> err";
      "      | Ok request ->";
      "          let output_mode = Riot_fix.output_mode_of_request request in";
      "          match request.Riot_fix.action with";
      "          | Riot_fix.Run { mode; limit; target; _ } ->";
      "              Riot_fix.Cli.Execution.run_with_coordinator";
      "                ~output_mode";
      "                ~mode";
      "                ~scope:request.scope";
      "                ~limit";
      "                ~roots:[ target ]";
      "                ()";
      "          | _ ->";
      "              match Riot_fix.fix ~output_mode request with";
      "              | Error _ as err -> err";
      "              | Ok response -> print_response_output response";
      "    )";
      "";
      "let main ~args =";
      "  Riot_fix.Provider_registry.register_providers";
      "    [";
      String.concat "\n" (List.map plan.providers ~fn:provider_module_line);
      "    ];";
      "  run_generated_request ~args";
      "";
    ]

let main_source =
  String.concat
    "\n"
    [
      "open Std";
      "";
      "let main ~args =";
      "  Fixme_runner.main ~args";
      "";
      "let () =";
      "  Runtime.run ~main ~args:Env.args ()";
      "";
    ]

let ensure_directories = fun plan ->
  List.for_each
    [ plan.generated_dir; plan.package_dir; plan.src_dir; plan.providers_dir; ]
    ~fn:(fun path ->
      Fs.create_dir_all path
      |> Result.expect ~msg:("failed to create generated fixme runner dir " ^ Path.to_string path))

let remove_if_exists = fun path remove ->
  match Fs.exists path with
  | Ok true ->
      remove path
      |> Result.expect ~msg:("failed to clean " ^ Path.to_string path)
  | _ -> ()

let cleanup_stale_sources = fun plan -> remove_if_exists plan.providers_dir Fs.remove_dir_all

let write_file = fun path content ->
  Fs.write content path
  |> Result.expect ~msg:("failed to write " ^ Path.to_string path)

let copy_provider_source = fun (provider: generated_provider) ->
  Fs.copy ~src:provider.provider.source_path ~dst:provider.copied_source_path
  |> Result.expect
    ~msg:("failed to copy provider source " ^ Path.to_string provider.provider.source_path)

let attach_to_workspace = fun workspace plan ->
  let other_packages =
    workspace.Riot_model.Workspace.packages
    |> List.filter
      ~fn:(fun (pkg: Riot_model.Package_manifest.t) ->
        not
          (Riot_model.Package_name.equal pkg.name plan.package_name))
  in
  {
    workspace with
    packages = other_packages @ [ Riot_model.Package_manifest.from_package plan.package ];
  }

let materialize = fun ~workspace_root ~target_dir_root providers ->
  let plan = plan ~workspace_root ~target_dir_root providers in
  cleanup_stale_sources plan;
  ensure_directories plan;
  write_file plan.library_path (library_source plan);
  write_file plan.main_path main_source;
  List.for_each plan.providers ~fn:copy_provider_source;
  plan
