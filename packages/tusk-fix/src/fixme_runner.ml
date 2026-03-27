open Std

type generated_provider = {
  provider : Tusk_model.Fix_provider.t;
  module_name : string;
  copied_source_path : Path.t;
  support_module_sources : (string * Path.t) list;
}

type plan = {
  provider_hash : string;
  generated_dir : Path.t;
  workspace_root : Path.t;
  workspace_toml_path : Path.t;
  toolchain_toml_path : Path.t;
  package_dir : Path.t;
  package_toml_path : Path.t;
  src_dir : Path.t;
  providers_dir : Path.t;
  library_path : Path.t;
  main_path : Path.t;
  registry_path : Path.t;
  binary_path : Path.t;
  package_name : string;
  binary_name : string;
  providers : generated_provider list;
}

let sanitize_component text =
  String.map
    (fun ch ->
      if
        (ch >= 'a' && ch <= 'z')
        || (ch >= 'A' && ch <= 'Z')
        || (ch >= '0' && ch <= '9')
      then ch
      else '_')
    text

let generated_module_name (provider : Tusk_model.Fix_provider.t) =
  "Provider_" ^ sanitize_component provider.package_name ^ "_"
  ^ sanitize_component provider.name

let ocaml_module_name_of_path path =
  let base =
    Path.basename path
    |> Path.v
    |> Path.remove_extension
    |> Path.to_string
  in
  if String.length base = 0 then
    "Generated"
  else
    String.uppercase_ascii (String.sub base 0 1)
    ^ String.sub base 1 (String.length base - 1)

let support_module_sources (provider : Tusk_model.Fix_provider.t) =
  let provider_dir = Path.dirname provider.source_path in
  let provider_basename = Path.basename provider.source_path in
  match Fs.read_dir provider_dir with
  | Error _ -> []
  | Ok iter ->
      Std.Iter.MutIterator.to_list iter
      |> List.filter_map (fun entry ->
             let source_path = Path.(provider_dir / entry) in
             let entry_name = Path.basename source_path in
             if
               String.equal entry_name provider_basename
               || not (String.ends_with ~suffix:".ml" entry_name)
             then None
             else Some (ocaml_module_name_of_path source_path, source_path))
      |> List.sort (fun (left_name, left_path) (right_name, right_path) ->
             match String.compare left_name right_name with
             | 0 -> String.compare (Path.to_string left_path) (Path.to_string right_path)
             | cmp -> cmp)

let provider_fingerprint (provider : Tusk_model.Fix_provider.t) =
  String.concat ":"
    [
      provider.package_name;
      provider.name;
      Path.to_string provider.source_path;
      String.concat "," provider.rules;
    ]

let provider_hash providers =
  providers
  |> List.sort (fun (left : Tusk_model.Fix_provider.t) right ->
         String.compare (provider_fingerprint left) (provider_fingerprint right))
  |> List.map provider_fingerprint
  |> String.concat "\n"
  |> Crypto.hash_string
  |> Crypto.Digest.hex

let generated_provider plan provider =
  let module_name = generated_module_name provider in
  {
    provider;
    module_name;
    copied_source_path = Path.(plan.providers_dir / Path.v (module_name ^ ".ml"));
    support_module_sources = support_module_sources provider;
  }

let plan ~workspace_root ~target_dir_root providers =
  let hash = provider_hash providers in
  let generated_dir =
    Path.(target_dir_root / Path.v "tusk-fix" / Path.v "fixme-runner" / Path.v hash)
  in
  let workspace_root = Path.(generated_dir / Path.v "workspace") in
  let build_dir_root = Path.(generated_dir / Path.v "build") in
  let package_dir = Path.(workspace_root / Path.v "packages" / Path.v "fixme-runner") in
  let src_dir = Path.(package_dir / Path.v "src") in
  let providers_dir = Path.(generated_dir / Path.v "providers") in
  let binary_name = "fixme-runner" in
  let package_name = "fixme-runner" in
  let plan =
    {
      provider_hash = hash;
      generated_dir;
      workspace_root;
      workspace_toml_path = Path.(workspace_root / Path.v "tusk.toml");
      toolchain_toml_path = Path.(workspace_root / Path.v "ocaml-toolchain.toml");
      package_dir;
      package_toml_path = Path.(package_dir / Path.v "tusk.toml");
      src_dir;
      providers_dir;
      library_path = Path.(src_dir / Path.v "fixme_runner.ml");
      main_path = Path.(src_dir / Path.v "main.ml");
      registry_path = Path.(generated_dir / Path.v "fixme_registry.ml");
      binary_path =
        Path.(
          build_dir_root
          / Path.v "debug"
          / Path.v (Tusk_model.Tusk_dirs.host_target ())
          / Path.v "out"
          / Path.v (package_name ^ "/" ^ binary_name));
      package_name;
      binary_name;
      providers = [];
    }
  in
  {
    plan with
    providers = List.map (generated_provider plan) providers;
  }

let provider_module_line (provider : generated_provider) =
  "    (module " ^ provider.module_name ^ " : Tusk_fix.Provider.S);"

let registry_source providers =
  let plan = plan ~workspace_root:(Path.v ".") ~target_dir_root:(Path.v "_build") providers in
  String.concat "\n"
    [
      "open Std";
      "";
      "let register () =";
      "  Tusk_fix.Provider_registry.register_providers";
      "    [";
      String.concat "\n" (List.map provider_module_line plan.providers);
      "    ]";
      "";
    ]

let embedded_provider_module_source (provider : generated_provider) =
  let source =
    Fs.read provider.provider.source_path
    |> Result.expect
         ~msg:
           ("failed to read provider source "
          ^ Path.to_string provider.provider.source_path)
  in
  String.concat "\n"
    [
      "module " ^ provider.module_name ^ " = struct";
      String.concat "\n"
        (List.map
           (fun (module_name, source_path) ->
             let source =
               Fs.read source_path
               |> Result.expect
                    ~msg:
                      ("failed to read provider support source "
                     ^ Path.to_string source_path)
             in
             String.concat "\n"
               [ "module " ^ module_name ^ " = struct"; source; "end"; "" ])
           provider.support_module_sources);
      source;
      "end";
      "";
    ]

let workspace_toml_source plan =
  String.concat "\n"
    [
      "[workspace]";
      "members = [\"packages/fixme-runner\"]";
      "";
      "[tusk]";
      "target_dir = \""
      ^ Path.to_string Path.(plan.generated_dir / Path.v "build")
      ^ "\"";
      "";
    ]

let dependency_entries workspace_root providers =
  let workspace_packages =
    match Tusk_model.Workspace_manager.scan workspace_root with
    | Ok (workspace, _errors) -> workspace.Tusk_model.Workspace.packages
    | Error _ -> []
  in
  let workspace_package_path name =
    workspace_packages
    |> List.find_opt (fun (pkg : Tusk_model.Package.t) -> String.equal pkg.name name)
    |> Option.map (fun (pkg: Tusk_model.Package.t) -> pkg.path)
  in
  let provider_build_deps =
    providers
    |> List.concat_map (fun ({ provider; _ } : generated_provider) ->
           workspace_packages
           |> List.find_opt (fun (pkg : Tusk_model.Package.t) ->
                  String.equal pkg.name provider.package_name)
           |> Option.map (fun pkg -> pkg.Tusk_model.Package.build_dependencies)
           |> Option.unwrap_or ~default:[]
           |> List.filter_map (fun (dep : Tusk_model.Package.dependency) ->
                  match dep.source with
                  | Tusk_model.Package.Workspace ->
                      workspace_package_path dep.name
                      |> Option.map (fun path -> (dep.name, path))
                  | Tusk_model.Package.Path path -> Some (dep.name, path)))
  in
  let entries =
    [
      ("std", Path.(workspace_root / Path.v "packages" / Path.v "std"));
      ("syn", Path.(workspace_root / Path.v "packages" / Path.v "syn"));
      ("tusk-fix", Path.(workspace_root / Path.v "packages" / Path.v "tusk-fix"));
      ("fixme", Path.(workspace_root / Path.v "packages" / Path.v "fixme"));
    ]
    @ provider_build_deps
    @ List.map (fun ({ provider; _ } : generated_provider) ->
          (provider.package_name, provider.package_path))
        providers
  in
  List.sort_uniq
    (fun (left_name, left_path) (right_name, right_path) ->
      match String.compare left_name right_name with
      | 0 -> String.compare (Path.to_string left_path) (Path.to_string right_path)
      | cmp -> cmp)
    entries

let package_toml_source workspace_root plan =
  let dependency_lines =
    dependency_entries workspace_root plan.providers
    |> List.map (fun (name, path) ->
           name ^ " = { path = " ^ "\"" ^ Path.to_string path ^ "\" }")
  in
  String.concat "\n"
    [
      "[package]";
      "name = \"" ^ plan.package_name ^ "\"";
      "version = \"0.1.0\"";
      "";
      "[lib]";
      "path = \"src/fixme_runner.ml\"";
      "";
      "[[bin]]";
      "name = \"" ^ plan.binary_name ^ "\"";
      "path = \"src/main.ml\"";
      "";
      "[dependencies]";
      String.concat "\n" dependency_lines;
      "";
    ]

let library_source plan =
  String.concat "\n"
    [
      "open Std";
      "";
      String.concat "\n" (List.map embedded_provider_module_source plan.providers);
      "let main ~args =";
      "  Tusk_fix.Provider_registry.register_providers";
      "    [";
      String.concat "\n" (List.map provider_module_line plan.providers);
      "    ];";
      "  Tusk_fix.Cli.main ~args";
      "";
    ]

let main_source =
  String.concat "\n"
    [
      "open Std";
      "";
      "let () =";
      "  Miniriot.run ~main:Fixme_runner.main ~args:Env.args ()";
      "";
    ]

let local_toolchain_source workspace_root =
  let direct_config = Path.(workspace_root / Path.v "ocaml-toolchain.toml") in
  let local_compiler =
    Path.(workspace_root / Path.v "vendor" / Path.v "ocaml" / Path.v "compiler")
  in
  match Fs.exists direct_config with
  | Ok true -> Some (`Copy direct_config)
  | _ -> (
      match Fs.is_dir local_compiler with
      | Ok true -> Some (`Generate local_compiler)
      | _ -> None)

let toolchain_toml_source compiler_path =
  String.concat "\n"
    [
      "[toolchain]";
      "version = { path = \"" ^ Path.to_string compiler_path ^ "\" }";
      "";
    ]

let ensure_directories plan =
  List.iter
    (fun path ->
      Fs.create_dir_all path
      |> Result.expect
           ~msg:("failed to create generated fixme runner dir " ^ Path.to_string path))
    [ plan.workspace_root; plan.package_dir; plan.src_dir; plan.providers_dir ]

let remove_if_exists path remove =
  match Fs.exists path with
  | Ok true -> remove path |> Result.expect ~msg:("failed to clean " ^ Path.to_string path)
  | _ -> ()

let cleanup_stale_sources plan =
  remove_if_exists plan.registry_path Fs.remove_file;
  remove_if_exists plan.providers_dir Fs.remove_dir_all

let write_file path content =
  Fs.write content path
  |> Result.expect ~msg:("failed to write " ^ Path.to_string path)

let copy_provider_source (provider : generated_provider) =
  Fs.copy ~src:provider.provider.source_path ~dst:provider.copied_source_path
  |> Result.expect
       ~msg:
         ("failed to copy provider source "
        ^ Path.to_string provider.provider.source_path)

let materialize_toolchain workspace_root plan =
  match local_toolchain_source workspace_root with
  | Some (`Copy source_path) ->
      Fs.copy ~src:source_path ~dst:plan.toolchain_toml_path
      |> Result.expect
           ~msg:
             ("failed to copy " ^ Path.to_string source_path ^ " into fixme runner")
  | Some (`Generate compiler_path) ->
      write_file plan.toolchain_toml_path (toolchain_toml_source compiler_path)
  | None -> ()

let materialize ~workspace_root ~target_dir_root providers =
  let plan = plan ~workspace_root ~target_dir_root providers in
  cleanup_stale_sources plan;
  ensure_directories plan;
  write_file plan.workspace_toml_path (workspace_toml_source plan);
  materialize_toolchain workspace_root plan;
  write_file plan.package_toml_path (package_toml_source workspace_root plan);
  write_file plan.library_path (library_source plan);
  write_file plan.main_path main_source;
  write_file plan.registry_path (registry_source providers);
  List.iter copy_provider_source plan.providers;
  plan
