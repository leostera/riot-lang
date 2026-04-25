open Std
open Riot_model
open ArgParser

type target =
  | Workspace_target
  | Package_target of string

type workspace_kind =
  | Workspace
  | Standalone_package

type workspace_scan =
  | NoWorkspace
  | ScanFailed of workspace_scan_error
  | Loaded of Riot_model.Workspace_manifest.t * Riot_model.Workspace_manager.load_error list

and workspace_scan_error =
  | CurrentDirReadFailed of Path.error
  | WorkspaceScanFailed of Riot_model.Workspace_manager.scan_error

let command =
  let open ArgParser in
    let open ArgParser.Arg in command "info"
    |> about "Show resolved workspace or package information"
    |> args
      [
        positional "target" |> required false |> help "workspace or <pkg>[@<version>]";
        flag "json" |> long "json" |> help "Emit machine-readable JSON output";
      ]

let target_of_matches: ArgParser.matches -> target = fun matches ->
  match ArgParser.get_one matches "target" with
  | None
  | Some "workspace" -> Workspace_target
  | Some target -> Package_target target

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "system call '"
  ^ syscall
  ^ "' returned invalid UTF-8 path: "
  ^ path
  | Path.SystemError error -> error

let workspace_scan_error_message = function
  | CurrentDirReadFailed error -> "failed to read current directory: " ^ path_error_message error
  | WorkspaceScanFailed error -> Workspace_manager.scan_error_message error

let rec toml_json = function
  | Data.Toml.String value -> Data.Json.String value
  | Data.Toml.Int value -> Data.Json.Int value
  | Data.Toml.Array values -> Data.Json.Array (List.map values ~fn:toml_json)
  | Data.Toml.Table fields -> Data.Json.Object (List.map
    fields
    ~fn:(fun (key, value) -> (key, toml_json value)))
  | Data.Toml.Bool value -> Data.Json.Bool value

let workspace_kind = fun ~(workspace_manager:Workspace_manager.t) (workspace: Workspace_manifest.t) ->
  let manifest_path = Path.(workspace.root / Path.v "riot.toml") in
  match Workspace_manager.load_riot_toml workspace_manager manifest_path with
  | Ok toml -> (
      match Data.Toml.get_table toml with
      | Some fields when List.any fields
        ~fn:(fun (name, _) ->
          String.equal name "workspace") -> Workspace
      | _ -> Standalone_package
    )
  | Error _ -> Workspace

let workspace_kind_string = function
  | Workspace -> "workspace"
  | Standalone_package -> "package"

let json_string_or_null = function
  | Some value -> Data.Json.String value
  | None -> Data.Json.Null

let relative_or_absolute_path = fun ~root path ->
  let root = Path.normalize root in
  let path = Path.normalize path in
  match Path.strip_prefix path ~prefix:root with
  | Ok relative_path -> Path.to_string relative_path
  | Error _ -> Path.to_string path

let workspace_packages = fun (workspace: Workspace_manifest.t) ->
  workspace.packages |> List.filter ~fn:Package_manifest.is_workspace_member |> List.sort
    ~compare:(fun (left: Package_manifest.t) (right: Package_manifest.t) ->
      Package_name.compare left.name right.name)

let manifest_path = fun path -> Path.normalize Path.(path / Path.v "riot.toml")

let manifest_json_fields = fun ~(workspace_manager:Workspace_manager.t) path ->
  let manifest_path = manifest_path path in
  match Workspace_manager.load_riot_toml workspace_manager manifest_path with
  | Ok manifest -> [
    ("manifest_path", Data.Json.String (Path.to_string manifest_path));
    ("manifest", toml_json manifest);
  ]
  | Error err -> [
    ("manifest_path", Data.Json.String (Path.to_string manifest_path));
    ("manifest", Data.Json.Null);
    ("manifest_error", Data.Json.String (Workspace_manager.manifest_load_error_message err));
  ]

let package_json = fun ~(workspace_manager:Workspace_manager.t) ~(workspace:Workspace_manifest.t) (
  pkg: Package_manifest.t
) ->
  let package_root = Path.normalize pkg.path in
  let fields = [
    ("name", Data.Json.String (Package_name.to_string pkg.name));
    ("root", Data.Json.String (Path.to_string package_root));
    ("relative_path", Data.Json.String (relative_or_absolute_path ~root:workspace.root package_root));
  ]
  @ manifest_json_fields ~workspace_manager package_root in
  Data.Json.Object fields

let workspace_json = fun ~(workspace_manager:Workspace_manager.t) ~(load_errors:Workspace_manager.load_error list) (
  workspace: Workspace_manifest.t
) ->
  let kind = workspace_kind ~workspace_manager workspace in
  let workspace_root = Path.normalize workspace.root in
  let fields = [
    ("type", Data.Json.String "workspace_info");
    ("kind", Data.Json.String (workspace_kind_string kind));
    ("name", json_string_or_null workspace.name);
    ("root", Data.Json.String (Path.to_string workspace_root));
    ("target_dir_root", Data.Json.String (Path.to_string workspace.target_dir_root));
    (
      "packages",
      Data.Json.Array (workspace_packages workspace
      |> List.map ~fn:(package_json ~workspace_manager ~workspace))
    );
    (
      "load_errors",
      Data.Json.Array (load_errors
      |> List.map ~fn:Workspace_manager.load_error_to_string
      |> List.map ~fn:Data.Json.string)
    );
  ]
  @ manifest_json_fields ~workspace_manager workspace_root in
  Data.Json.Object fields

let error_json = fun ~kind ~message ->
  Data.Json.Object [
    ("type", Data.Json.String "workspace_info_error");
    ("kind", Data.Json.String kind);
    ("error", Data.Json.String message);
  ]

let print_workspace = fun ~(load_errors:Workspace_manager.load_error list) (
  workspace: Workspace_manifest.t
) ->
  let workspace_manager = Workspace_manager.create () in
  let kind = workspace_kind ~workspace_manager workspace in
  let workspace_manifest_path = manifest_path workspace.root in
  let display_name =
    match workspace.name with
    | Some name -> name
    | None -> Path.basename workspace.root
  in
  println ("Kind: " ^ workspace_kind_string kind);
  println ("Name: " ^ display_name);
  println ("Root: " ^ Path.to_string workspace.root);
  println ("Manifest: " ^ Path.to_string workspace_manifest_path);
  println ("Target dir: " ^ Path.to_string workspace.target_dir_root);
  println "";
  println "Packages:";
  workspace_packages workspace |> List.for_each
    ~fn:(fun (pkg: Package_manifest.t) ->
      let package_manifest_path = manifest_path pkg.path in
      println
        ("  - "
        ^ Package_name.to_string pkg.name
        ^ " ("
        ^ relative_or_absolute_path ~root:workspace.root pkg.path
        ^ ")");
      println
        ("      manifest: " ^ relative_or_absolute_path ~root:workspace.root package_manifest_path));
  if not (List.is_empty load_errors) then
    (
      println "";
      println "Load errors:";
      load_errors
      |> List.for_each ~fn:(fun err -> println ("  - " ^ Workspace_manager.load_error_to_string err))
    )

let print_json = fun json -> println (Data.Json.to_string json)

let print_package = fun (info: Info_package.t) ->
  println ("Package: " ^ Riot_model.Package_name.to_string info.name);
  println
    (
      "Source: " ^ (
        match info.source_kind with
        | Info_package.Workspace -> "workspace"
        | Info_package.Registry -> "registry"
      )
    );
  (
    match info.resolved_version with
    | Some version -> println ("Version: " ^ version)
    | None -> ()
  );
  (
    match info.source_kind with
    | Info_package.Workspace ->
        println ("Public: " ^ Bool.to_string (Option.unwrap_or ~default:false info.is_public));
        (
          match info.workspace_root with
          | Some workspace_root -> println ("Workspace root: " ^ Path.to_string workspace_root)
          | None -> ()
        );
        (
          match info.package_path with
          | Some package_path -> println ("Package path: " ^ package_path)
          | None -> println ("Package path: " ^ Path.to_string info.manifest_path)
        )
    | Info_package.Registry ->
        println ("Root: " ^ Path.to_string info.root);
        println ("Manifest: " ^ Path.to_string info.manifest_path);
        (
          match info.registry_root with
          | Some registry_root -> println ("Registry root: " ^ Path.to_string registry_root)
          | None -> ()
        );
        (
          match info.registry_package_path with
          | Some path -> println ("Registry package path: " ^ Path.to_string path)
          | None -> ()
        )
  );
  (
    match info.description with
    | Some description -> println ("Description: " ^ description)
    | None -> ()
  );
  (
    match info.license with
    | Some license -> println ("License: " ^ license)
    | None -> ()
  );
  let link_lines =
    [
      ("docs", info.links.docs_url);
      ("package", info.links.package_url);
      ("homepage", info.links.homepage_url);
      ("repository", info.links.repository_url);
      ("source", info.links.source_url);
    ]
    |> List.filter_map
      ~fn:(fun (label, value) ->
        match value with
        | Some value -> Some ("  " ^ label ^ ": " ^ value)
        | None -> None)
  in
  if not (List.is_empty link_lines) then
    (
      println "";
      println "Links:";
      List.for_each link_lines ~fn:println
    );
  (
    match info.manifest_error with
    | Some err ->
        println "";
        println ("Manifest error: " ^ err)
    | None -> ()
  );
  if not (List.is_empty info.load_errors) then
    (
      println "";
      println "Load errors:";
      List.for_each info.load_errors ~fn:(fun err -> println ("  - " ^ err))
    )

let run = fun ~(workspace_scan:workspace_scan) matches ->
  let json = ArgParser.get_flag matches "json" in
  match target_of_matches matches with
  | Workspace_target -> (
      match workspace_scan with
      | Loaded (workspace, load_errors) ->
          let workspace_manager = Workspace_manager.create () in
          if json then
            print_json (workspace_json ~workspace_manager ~load_errors workspace)
          else
            print_workspace ~load_errors workspace;
          Ok ()
      | NoWorkspace ->
          let message = Workspace_hint.not_in_workspace_message in
          if json then
            print_json (error_json ~kind:"no_workspace" ~message)
          else
            Workspace_hint.print_not_in_workspace ();
          Error (Failure message)
      | ScanFailed err ->
          let message = workspace_scan_error_message err in
          if json then
            print_json (error_json ~kind:"scan_failed" ~message)
          else
            eprintln ("\027[1;31mError\027[0m: " ^ message);
          Error (Failure message)
    )
  | Package_target target ->
      let local_workspace =
        match workspace_scan with
        | Loaded (workspace, load_errors) -> Some (workspace, load_errors)
        | NoWorkspace
        | ScanFailed _ -> None
      in
      (
        match Info_package.resolve ~local_workspace ~target () with
        | Ok info ->
            if json then
              print_json (Info_package.to_json info)
            else
              print_package info;
            Ok ()
        | Error err ->
            if json then
              print_json (Info_package.error_to_json ~error:err)
            else
              eprintln ("\027[1;31mError\027[0m: " ^ err.message);
            Error (Failure err.message)
      )
