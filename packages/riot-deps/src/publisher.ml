open Std

type error =
  | MissingPublishVersion of { package: string }
  | MissingPublishDescription of { package: string }
  | MissingPublishLicense of { package: string }
  | PackageNotPublic of { package: string }
  | MissingManifest of {
      package_root: Path.t;
    }
  | RuntimeDependencyNotPublishable of {
      package: string;
      dependency: string;
      reason: [ | `PathOnly of Path.t | `WorkspaceOnly | `MissingVersionOrPath];
    }
  | RuntimeDependencyRegistryLookupFailed of {
      package: string;
      dependency: string;
      registry: string;
      error: string;
    }
  | RuntimeDependencyNotFoundInRegistry of { package: string; dependency: string; registry: string }
  | SymlinkNotAllowed of {
      path: Path.t;
    }
  | UnsupportedEntry of {
      path: Path.t;
      kind: string;
    }
  | DirectoryReadFailed of {
      path: Path.t;
      error: IO.error;
    }
  | MetadataReadFailed of {
      path: Path.t;
      error: metadata_error;
    }
  | ArtifactReadFailed of {
      path: Path.t;
      error: IO.error;
    }
  | TarCommandFailed of { command: string; status: int; stdout: string; stderr: string }
  | TarCommandSpawnFailed of {
      command: string;
      error: Command.error;
    }
  | GitProvenanceFailed of Git_provenance.error
  | RegistryPublishFailed of { locator: string; error: string }
  | CyclicWorkspacePublishOrder of {
      cycle: string list;
    }

and metadata_error =
  | MetadataIoError of IO.error
  | MetadataPathError of Path.error

type prepared_publish = {
  package: Riot_model.Package.t;
  version: Std.Version.t;
  locator: string;
  selector: string;
  artifact_path: Path.t;
}

type publish_plan = {
  package: Riot_model.Package.t;
  version: Std.Version.t;
  locator: string;
  selector: string;
}

let excluded_entry_names = [
  "_build";
  ".git";
  ".hg";
  "node_modules";
  ".direnv";
  ".DS_Store";
  "dist";
  "coverage";
  ".turbo";
  ".cache";
  ".tmp";
  "tmp";
]

let is_apple_junk_entry = fun name ->
  String.starts_with ~prefix:"._" name
  || String.equal name ".DS_Store"
  || String.equal name "__MACOSX"

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid utf8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "invalid utf8 from " ^ syscall ^ ": " ^ path
  | Path.SystemError msg -> msg

let metadata_error_message = function
  | MetadataIoError error -> IO.error_message error
  | MetadataPathError error -> path_error_message error

let command_error_message = function
  | Command.SystemError error -> error

let message = function
  | MissingPublishVersion { package } -> "package '" ^ package ^ "' is missing [package].version"
  | MissingPublishDescription { package } ->
      "package '" ^ package ^ "' is missing [package].description"
  | MissingPublishLicense { package } -> "package '" ^ package ^ "' is missing [package].license"
  | PackageNotPublic { package } ->
      "package '" ^ package ^ "' must set [package].public = true to be published"
  | MissingManifest { package_root } ->
      "package root '" ^ Path.to_string package_root ^ "' is missing riot.toml at archive root"
  | RuntimeDependencyNotPublishable { package; dependency; reason = `PathOnly path } ->
      "runtime dependency '"
      ^ dependency
      ^ "' in package '"
      ^ package
      ^ "' is path-only and cannot be published (path = "
      ^ Path.to_string path
      ^ ")"
  | RuntimeDependencyNotPublishable { package; dependency; reason = `WorkspaceOnly } ->
      "runtime dependency '"
      ^ dependency
      ^ "' in package '"
      ^ package
      ^ "' is workspace-only and cannot be published"
  | RuntimeDependencyNotPublishable { package; dependency; reason = `MissingVersionOrPath } ->
      "runtime dependency '"
      ^ dependency
      ^ "' in package '"
      ^ package
      ^ "' must declare a version or publishable source"
  | RuntimeDependencyRegistryLookupFailed {
    package;
    dependency;
    registry;
    error
  } ->
      "failed to verify runtime dependency '"
      ^ dependency
      ^ "' for package '"
      ^ package
      ^ "' in registry '"
      ^ registry
      ^ "': "
      ^ error
  | RuntimeDependencyNotFoundInRegistry { package; dependency; registry } ->
      "runtime dependency '"
      ^ dependency
      ^ "' for package '"
      ^ package
      ^ "' was not found in registry '"
      ^ registry
      ^ "'"
  | SymlinkNotAllowed { path } ->
      "publish artifacts do not support symlinks: " ^ Path.to_string path
  | UnsupportedEntry { path; kind } ->
      "publish artifacts only support regular files and directories; found "
      ^ kind
      ^ " at "
      ^ Path.to_string path
  | DirectoryReadFailed { path; error } ->
      "failed to read directory '" ^ Path.to_string path ^ "': " ^ IO.error_message error
  | MetadataReadFailed { path; error } ->
      "failed to read metadata for '" ^ Path.to_string path ^ "': " ^ metadata_error_message error
  | ArtifactReadFailed { path; error } ->
      "failed to read publish artifact '" ^ Path.to_string path ^ "': " ^ IO.error_message error
  | TarCommandFailed {
    command;
    status;
    stdout;
    stderr
  } ->
      let detail =
        if String.equal stderr "" then
          stdout
        else
          stderr
      in
      "failed to create publish artifact with '"
      ^ command
      ^ "' (exit "
      ^ Int.to_string status
      ^ "): "
      ^ detail
  | TarCommandSpawnFailed { command; error } ->
      "failed to spawn publish artifact command '" ^ command ^ "': " ^ command_error_message error
  | GitProvenanceFailed error -> Git_provenance.message error
  | RegistryPublishFailed { locator; error } -> "failed to publish '" ^ locator ^ "': " ^ error
  | CyclicWorkspacePublishOrder { cycle } ->
      "workspace publish order contains a cycle: " ^ String.concat " -> " cycle

let should_skip_entry = fun path ->
  let name = Path.basename path in
  is_apple_junk_entry name || List.any excluded_entry_names ~fn:(String.equal name)

let file_kind_to_string = function
  | `Regular -> "regular file"
  | `Directory -> "directory"
  | `Symlink -> "symlink"
  | `Block -> "block device"
  | `Character -> "character device"
  | `Fifo -> "fifo"
  | `Socket -> "socket"

let walker_kind_to_string = function
  | Fs.Walker.File -> "regular file"
  | Fs.Walker.Directory -> "directory"
  | Fs.Walker.Symlink -> "symlink"
  | Fs.Walker.Other -> "unsupported entry"

let publisher_error_of_walker_error = fun ~package_root (err: Fs.Walker.error) ->
  match err.path with
  | Some path -> (
      if Path.is_directory path then
        DirectoryReadFailed { path; error = err.cause }
      else
        MetadataReadFailed { path; error = MetadataIoError err.cause }
    )
  | None -> DirectoryReadFailed { path = package_root; error = err.cause }

let validate_publish_metadata = fun ~(package:Riot_model.Package.t) ->
  let package_name = Riot_model.Package_name.to_string package.name in
  match package.publish.version with
  | None -> Error (MissingPublishVersion { package = package_name })
  | Some version -> (
      match package.publish.description with
      | None -> Error (MissingPublishDescription { package = package_name })
      | Some _ -> (
          match package.publish.license with
          | None -> Error (MissingPublishLicense { package = package_name })
          | Some _ -> (
              match package.publish.is_public with
              | Some true -> Ok version
              | Some false
              | None -> Error (PackageNotPublic { package = package_name })
            )
        )
    )

let validate_runtime_dependency = fun
  ~(package:Riot_model.Package.t)
  (dep: Riot_model.Package.dependency) ->
  let package_name = Riot_model.Package_name.to_string package.name in
  let dependency_name = Riot_model.Package_name.to_string dep.name in
  match dep.source with
  | { builtin = true; _ } -> Ok ()
  | { workspace = true; _ } ->
      Error (
        RuntimeDependencyNotPublishable {
          package = package_name;
          dependency = dependency_name;
          reason = `WorkspaceOnly;
        }
      )
  | { path = Some path; source_locator = None; version = None; _ } ->
      Error (
        RuntimeDependencyNotPublishable {
          package = package_name;
          dependency = dependency_name;
          reason = `PathOnly path;
        }
      )
  | { path = None; source_locator = None; version = None; _ } ->
      Error (
        RuntimeDependencyNotPublishable {
          package = package_name;
          dependency = dependency_name;
          reason = `MissingVersionOrPath;
        }
      )
  | _ -> Ok ()

let validate_runtime_dependencies = fun ~(package:Riot_model.Package.t) ->
  let rec loop = function
    | [] ->
        Ok ()
    | dep :: rest -> (
        match validate_runtime_dependency ~package dep with
        | Ok () -> loop rest
        | Error _ as err -> err
      )
  in
  loop package.dependencies

let validate_registry_dependencies = fun
  ~registry
  ~publishing_workspace_packages
  ~(package:Riot_model.Package.t) ->
  let package_name = Riot_model.Package_name.to_string package.name in
  let rec loop = function
    | [] ->
        Ok ()
    | dep :: rest -> (
        if Riot_model.Package.is_builtin_dependency dep then
          loop rest
        else if
          List.any
            publishing_workspace_packages
            ~fn:(fun package_name -> Riot_model.Package_name.equal dep.name package_name)
        then
          loop rest
        else if Option.is_some dep.source.source_locator then
          loop rest
        else
          let dependency_name = Riot_model.Package_name.to_string dep.name in
          match Pkgs_ml.Registry.read_package_document registry ~package_name:dependency_name with
          | Error error ->
              Error (
                RuntimeDependencyRegistryLookupFailed {
                  package = package_name;
                  dependency = dependency_name;
                  registry = Pkgs_ml.Registry.name registry;
                  error;
                }
              )
          | Ok None ->
              Error (RuntimeDependencyNotFoundInRegistry {
                package = package_name;
                dependency = dependency_name;
                registry = Pkgs_ml.Registry.name registry;
              })
          | Ok (Some _) -> loop rest
      )
  in
  loop package.dependencies

let published_version_exists = fun ~registry ~package_name ~version ->
  let package_name_string = Riot_model.Package_name.to_string package_name in
  match Pkgs_ml.Registry.read_package_document registry ~package_name:package_name_string with
  | Error error ->
      Error (
        RuntimeDependencyRegistryLookupFailed {
          package = package_name_string;
          dependency = package_name_string;
          registry = Pkgs_ml.Registry.name registry;
          error;
        }
      )
  | Ok None -> Ok false
  | Ok (Some document) ->
      let version = Std.Version.to_string version in
      Ok (List.any
        document.releases
        ~fn:(fun (release: Pkgs_ml.Sparse_index.release) -> String.equal release.version version))

let collect_relative_files = fun ~package_root ->
  let walker =
    match Fs.Walker.create ~roots:[ package_root ] ~sort:true () with
    | Ok walker -> walker
    | Error _ -> panic "publisher walker configuration should be valid"
  in
  let iter =
    walker
    |> Fs.Walker.filter_entry
      ~f:(fun (entry: Fs.Walker.FileItem.t) ->
        not
          (should_skip_entry (Fs.Walker.FileItem.path entry)))
    |> Fs.Walker.into_iter
  in
  let rec loop acc iter =
    match Iter.Iterator.next iter with
    | (None, _) -> Ok (List.reverse acc)
    | (Some (Error err), _) -> Error (publisher_error_of_walker_error ~package_root err)
    | (Some (Ok (entry: Fs.Walker.FileItem.t)), iter') -> (
        let path = Fs.Walker.FileItem.path entry in
        match Fs.Walker.FileItem.kind entry with
        | Directory -> loop acc iter'
        | File -> (
            match Path.strip_prefix path ~prefix:package_root with
            | Ok relative -> loop (relative :: acc) iter'
            | Error err -> Error (MetadataReadFailed { path; error = MetadataPathError err })
          )
        | Symlink -> Error (SymlinkNotAllowed { path })
        | Other ->
            Error (UnsupportedEntry {
              path;
              kind = walker_kind_to_string (Fs.Walker.FileItem.kind entry);
            })
      )
  in
  loop [] iter

let publish_artifact_path = fun ~target_dir_root ~(package:Riot_model.Package.t) ~version ->
  let package_name = Riot_model.Package_name.to_string package.name in
  Path.(target_dir_root
  / Path.v "release"
  / Path.v "publish"
  / Path.v package_name
  / Path.v (Std.Version.to_string version)
  / Path.v "package.tar.gz")

let create_archive = fun ~package_root ~artifact_path ~relative_files ->
  let parent =
    match Path.parent artifact_path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  match Fs.create_dir_all parent with
  | Error err -> Error (ArtifactReadFailed { path = artifact_path; error = err })
  | Ok () ->
      let args =
        [ "-czf"; Path.to_string artifact_path; "-C"; Path.to_string package_root; ]
        @ List.map relative_files ~fn:Path.to_string
      in
      let command = Command.make "tar" ~args in
      match Command.output command with
      | Error error ->
          Error (TarCommandSpawnFailed { command = Command.to_string command; error })
      | Ok output when not (Int.equal output.status 0) ->
          Error (
            TarCommandFailed {
              command = Command.to_string command;
              status = output.status;
              stdout = output.stdout;
              stderr = output.stderr;
            }
          )
      | Ok _ -> Ok artifact_path

let create_artifact = fun ~target_dir_root ~(package:Riot_model.Package.t) ~version ->
  match collect_relative_files ~package_root:package.path with
  | Error _ as err -> err
  | Ok relative_files ->
      let relative_files =
        List.sort
          relative_files
          ~compare:(fun left right -> String.compare (Path.to_string left) (Path.to_string right))
      in
      if not (List.any relative_files ~fn:(Path.equal (Path.v "riot.toml"))) then
        Error (MissingManifest { package_root = package.path })
      else
        let artifact_path = publish_artifact_path ~target_dir_root ~package ~version in
        create_archive ~package_root:package.path ~artifact_path ~relative_files

let plan_publish = fun ~registry ~publishing_workspace_packages ~(package:Riot_model.Package.t) ->
  match validate_publish_metadata ~package with
  | Error _ as err -> err
  | Ok version -> (
      match validate_runtime_dependencies ~package with
      | Error _ as err -> err
      | Ok () -> (
          match validate_registry_dependencies ~registry ~publishing_workspace_packages ~package with
          | Error _ as err -> err
          | Ok () -> (
              match Git_provenance.discover ~package_root:package.path with
              | Error error -> Error (GitProvenanceFailed error)
              | Ok provenance ->
                  Ok {
                    package;
                    version;
                    locator = provenance.locator;
                    selector = provenance.selector;
                  }
            )
        )
    )

let prepare_publish_artifact = fun ~target_dir_root (plan: publish_plan) ->
  match create_artifact ~target_dir_root ~package:plan.package ~version:plan.version with
  | Error _ as err -> err
  | Ok artifact_path ->
      Ok {
        package = plan.package;
        version = plan.version;
        locator = plan.locator;
        selector = plan.selector;
        artifact_path;
      }

let prepare_publish = fun
  ~registry
  ~target_dir_root
  ~publishing_workspace_packages
  ~(package:Riot_model.Package.t) ->
  match plan_publish ~registry ~publishing_workspace_packages ~package with
  | Error _ as err -> err
  | Ok plan -> prepare_publish_artifact ~target_dir_root plan

let publish_prepared = fun ~registry ~api_token (prepared: prepared_publish) ->
  match Fs.read prepared.artifact_path with
  | Error err -> Error (ArtifactReadFailed { path = prepared.artifact_path; error = err })
  | Ok artifact -> (
      match Pkgs_ml.Registry.publish_artifact registry ~api_token ~artifact with
      | Ok published -> Ok published
      | Error error -> Error (RegistryPublishFailed { locator = prepared.locator; error })
    )

let publish = fun
  ~registry
  ~target_dir_root
  ~publishing_workspace_packages
  ~(package:Riot_model.Package.t)
  ~api_token ->
  match prepare_publish ~registry ~target_dir_root ~publishing_workspace_packages ~package with
  | Error _ as err -> err
  | Ok prepared -> publish_prepared ~registry ~api_token prepared

let assoc_package = fun packages name ->
  List.find packages ~fn:(fun (pkg_name, _pkg) -> Riot_model.Package_name.equal pkg_name name)
  |> Option.map ~fn:(fun (_, package) -> package)

let workspace_runtime_dependency_names = fun ~workspace_packages (pkg: Riot_model.Package.t) ->
  let is_workspace_dependency (dep: Riot_model.Package.dependency) =
    if dep.source.workspace then
      Option.is_some (assoc_package workspace_packages dep.name)
    else
      match dep.source.path with
      | Some _ -> Option.is_some (assoc_package workspace_packages dep.name)
      | None -> false
  in
  pkg.dependencies
  |> List.filter ~fn:is_workspace_dependency
  |> List.map ~fn:(fun (dep: Riot_model.Package.dependency) -> dep.name)

let workspace_publish_order = fun ~packages ->
  let workspace_packages =
    packages
    |> List.filter ~fn:Riot_model.Package.is_workspace_member
    |> List.map ~fn:(fun (pkg: Riot_model.Package.t) -> (pkg.name, pkg))
  in
  let rec visit ~visiting ~visited ordered name =
    if List.any visited ~fn:(Riot_model.Package_name.equal name) then
      Ok (visited, ordered)
    else if List.any visiting ~fn:(Riot_model.Package_name.equal name) then
      Error (
        CyclicWorkspacePublishOrder {
          cycle =
            List.reverse (name :: visiting)
            |> List.map ~fn:Riot_model.Package_name.to_string;
        }
      )
    else
      match assoc_package workspace_packages name with
      | None -> Ok (visited, ordered)
      | Some pkg ->
          let visiting = name :: visiting in
          let dependency_names = workspace_runtime_dependency_names ~workspace_packages pkg in
          let rec visit_dependencies visited ordered = function
            | [] ->
                let visited = name :: visited in
                Ok (visited, pkg :: ordered)
            | dep_name :: rest -> (
                match visit ~visiting ~visited ordered dep_name with
                | Error _ as err -> err
                | Ok (visited, ordered) -> visit_dependencies visited ordered rest
              )
          in
          visit_dependencies visited ordered dependency_names
  in
  let rec walk_names visited ordered = function
    | [] ->
        Ok (List.reverse ordered)
    | name :: rest -> (
        match visit ~visiting:[] ~visited ordered name with
        | Error _ as err -> err
        | Ok (visited, ordered) -> walk_names visited ordered rest
      )
  in
  walk_names
    []
    []
    (List.map workspace_packages ~fn:(fun (package_name, _) -> package_name))
