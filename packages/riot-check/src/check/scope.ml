open Std
open Std.Collections

module Check_error = Error

open Riot_model

type package_scope = {
  package_root: Path.t;
  config: Fmt_config.t;
}

type t = {
  workspace_root: Path.t;
  workspace_config: Fmt_config.t;
  packages: package_scope list;
}

let compare_paths = fun left right -> String.compare (Path.to_string left) (Path.to_string right)

let dedupe_paths = fun paths ->
  let seen = HashSet.create () in
  let rec loop acc remaining =
    match remaining with
    | [] -> List.rev acc
    | head :: tail ->
        let key = Path.to_string head in
        if HashSet.contains seen key then
          loop acc tail
        else
          (
            let _ = HashSet.insert seen key in
            loop (head :: acc) tail
          )
  in
  loop [] (List.sort compare_paths paths)

let is_supported_source_file = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> true
  | _ -> false

let relative_or_absolute = fun ~workspace_root path ->
  let root = Path.normalize workspace_root in
  let path = Path.normalize path in
  match Path.strip_prefix path ~prefix:root with
  | Ok rel -> Path.to_string rel
  | Error _ -> Path.to_string path

let workspace_roots = fun (workspace: Workspace.t) ->
  workspace.packages
  |> List.filter Package.is_workspace_member
  |> List.map (Workspace.package_root workspace)
  |> dedupe_paths

let workspace_roots_for_package = fun (workspace: Workspace.t) package_name ->
  workspace.packages
  |> List.filter
    (fun (pkg: Package.t) -> Package.is_workspace_member pkg && String.equal pkg.name package_name)
  |> List.map (Workspace.package_root workspace)
  |> dedupe_paths

let target_files_for_package = fun
  ~(workspace:Workspace.t) ~(include_dev:bool) (pkg: Package.t) ->
  let package_root = Workspace.package_root workspace pkg in
  let sources =
    if
      List.is_empty pkg.sources.src
      && List.is_empty pkg.sources.tests
      && List.is_empty pkg.sources.examples
      && List.is_empty pkg.sources.bench
    then
      Package.scan_sources ~package_path:package_root ()
    else
      pkg.sources
  in
  let scoped_sources =
    if include_dev then
      ((sources.src @ sources.tests) @ sources.examples) @ sources.bench
    else
      sources.src
  in
  scoped_sources
  |> List.filter is_supported_source_file
  |> List.map (fun relative -> Path.(package_root / relative))
  |> dedupe_paths

let workspace_target_files = fun ~(include_dev:bool) (workspace: Workspace.t) ->
  workspace.packages
  |> List.filter Package.is_workspace_member
  |> List.concat_map (target_files_for_package ~workspace ~include_dev)
  |> dedupe_paths

let workspace_target_files_for_package = fun
  ~(include_dev:bool) (workspace: Workspace.t) package_name ->
  workspace.packages
  |> List.filter
    (fun (pkg: Package.t) -> Package.is_workspace_member pkg && String.equal pkg.name package_name)
  |> List.concat_map (target_files_for_package ~workspace ~include_dev)
  |> dedupe_paths

let from_workspace = fun (workspace: Workspace.t) ->
  let scope_of_package (pkg: Package.t) =
    let path = Workspace.package_root workspace pkg in
    let package_toml = Path.(path / Path.v "riot.toml") in
    { package_root = path; config = Fmt_config.load package_toml }
  in
  let workspace_toml = Path.(workspace.root / Path.v "riot.toml") in
  {
    workspace_root = workspace.root;
    workspace_config = Fmt_config.load workspace_toml;
    packages =
      workspace.packages
      |> List.map scope_of_package;
  }

let matches_ignore_pattern = fun ~root pattern path ->
  let rel =
    match Path.strip_prefix path ~prefix:root with
    | Ok rel -> Path.to_string rel
    | Error _ -> Path.to_string path
  in
  String.contains rel pattern

let find_package_scope = fun (scope: t) file ->
  scope.packages
  |> List.filter_map
    (fun package_scope ->
      match Path.strip_prefix file ~prefix:package_scope.package_root with
      | Ok _ -> Some (String.length (Path.to_string package_scope.package_root), package_scope)
      | Error _ -> None)
  |> List.sort (fun (left_len, _) (right_len, _) -> Int.compare right_len left_len)
  |> List.map snd
  |> function
    | package_scope :: _ -> Some package_scope
    | [] -> None

let should_ignore_file = fun (scope: t) file ->
  if
    List.exists
      (fun pattern ->
        matches_ignore_pattern ~root:scope.workspace_root pattern file)
      scope.workspace_config.ignore_patterns
  then
    true
  else
    match find_package_scope scope file with
    | Some package_scope ->
        List.exists
          (fun pattern ->
            matches_ignore_pattern ~root:package_scope.package_root pattern file)
          package_scope.config.ignore_patterns
    | None -> false

let resolve_search_roots = fun ~workspace ?package_filter () ->
  match package_filter with
  | Some package_name -> (
      match workspace_roots_for_package workspace package_name with
      | [] -> Error (Check_error.UnknownPackage { package_name })
      | roots -> Ok roots
    )
  | None -> Ok (workspace_roots workspace)

let workspace_contains_path = fun (workspace: Workspace.t) path ->
  let root = Path.normalize workspace.root in
  let path = Path.normalize path in
  Path.equal path root || match Path.strip_prefix path ~prefix:root with
  | Ok _ -> true
  | Error _ -> false

let resolve_explicit_target_path = fun ~workspace path ->
  if Path.is_absolute path then
    Path.normalize path
  else
    let cwd_candidate =
      match Env.current_dir () with
      | Ok cwd -> Path.normalize Path.(cwd / path)
      | Error _ -> Path.normalize path
    in
    if workspace_contains_path workspace cwd_candidate then
      cwd_candidate
    else
      let workspace_candidate = Path.normalize Path.(workspace.root / path) in
      if Path.exists workspace_candidate then
        workspace_candidate
      else
        cwd_candidate

let validate_explicit_target = fun ~workspace path ->
  let resolved = resolve_explicit_target_path ~workspace path in
  if not (Path.exists resolved) then
    Error (Check_error.InvalidPath { path; reason = "path does not exist" })
  else if Path.is_file resolved && not (is_supported_source_file resolved) then
    Error (Check_error.InvalidPath {
      path;
      reason = "path is not an OCaml source file (.ml/.mli) or directory";
    })
  else
    Ok resolved

let validate_explicit_targets = fun ~workspace roots ->
  let rec loop remaining acc =
    match remaining with
    | [] -> Ok acc
    | head :: tail -> (
        match validate_explicit_target ~workspace head with
        | Error _ as err -> err
        | Ok root -> loop tail (root :: acc)
      )
  in
  loop roots []

let resolve_targets = fun ~workspace ?package_filter ?(include_dev = false) paths ->
  let scope = from_workspace workspace in
  let collect_ordered_files roots =
    let (explicit_files, directory_roots) =
      roots
      |> List.fold_left
        (fun (files, directories) root ->
          if Path.is_file root then
            (root :: files, directories)
          else
            (files, root :: directories))
        ([], [])
    in
    let walked_files =
      directory_roots
      |> List.concat_map
        (fun root ->
          Krasny.Runner.collect_ocaml_files
            ~should_ignore:(should_ignore_file scope)
            ~roots:[ root ]
            ()
          |> List.sort compare_paths)
    in
    dedupe_paths (explicit_files @ walked_files)
  in
  let roots =
    if List.is_empty paths then
      match package_filter with
      | Some package_name -> (
          match workspace_target_files_for_package ~include_dev workspace package_name with
          | [] -> (
              match workspace_roots_for_package workspace package_name with
              | [] -> Error (Check_error.UnknownPackage { package_name })
              | _ -> Error Check_error.NoTargets
            )
          | files -> Ok files
        )
      | None -> (
          match workspace_target_files ~include_dev workspace with
          | [] -> Error Check_error.NoTargets
          | files -> Ok files
        )
    else
      validate_explicit_targets ~workspace paths
      |> Result.map (List.sort_uniq compare_paths)
  in
  match roots with
  | Error _ as err -> err
  | Ok validated_roots ->
      let target_files = collect_ordered_files validated_roots in
      if List.is_empty target_files then
        Error Check_error.NoTargets
      else
        Ok target_files
