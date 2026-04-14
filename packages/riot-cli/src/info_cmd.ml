open Std
open Riot_model
open ArgParser

type workspace_kind =
  | Workspace
  | Package

type workspace_scan =
  | NoWorkspace
  | ScanFailed of string
  | Loaded of Riot_model.Workspace.t * Riot_model.Workspace_manager.load_error list

let command =
  let open ArgParser in
    let open Arg in command "info"
    |> about "Show resolved workspace information"
    |> args [ flag "json" |> long "json" |> help "Emit machine-readable JSON output"; ]

let rec toml_json = function
  | Data.Toml.String value -> Data.Json.String value
  | Data.Toml.Int value -> Data.Json.Int value
  | Data.Toml.Array values -> Data.Json.Array (List.map values ~fn:toml_json)
  | Data.Toml.Table fields -> Data.Json.Object (List.map fields ~fn:(fun (key, value) -> (key, toml_json value)))
  | Data.Toml.Bool value -> Data.Json.Bool value

let workspace_kind = fun ~(workspace_manager:Workspace_manager.t) (workspace: Workspace.t) ->
  let manifest_path = Path.(workspace.root / Path.v "riot.toml") in
  match Workspace_manager.load_riot_toml workspace_manager manifest_path with
  | Ok toml -> (
      match Data.Toml.get_table toml with
      | Some fields when List.any fields ~fn:(fun (name, _) -> String.equal name "workspace") -> Workspace
      | _ -> Package
    )
  | Error _ -> Workspace

let workspace_kind_string = function
  | Workspace -> "workspace"
  | Package -> "package"

let json_string_or_null = function
  | Some value -> Data.Json.String value
  | None -> Data.Json.Null

let relative_or_absolute_path = fun ~root path ->
  let root = Path.normalize root in
  let path = Path.normalize path in
  match Path.strip_prefix path ~prefix:root with
  | Ok relative_path -> Path.to_string relative_path
  | Error _ -> Path.to_string path

let workspace_packages = fun (workspace: Workspace.t) ->
  workspace.packages
  |> List.filter ~fn:Package_manifest.is_workspace_member
  |> List.sort ~compare:(fun (left: Package_manifest.t) (right: Package_manifest.t) ->
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
    ("manifest_error", Data.Json.String err);
  ]

let package_json = fun ~(workspace_manager:Workspace_manager.t) ~(workspace:Workspace.t) (
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
  workspace: Workspace.t
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

let print_workspace = fun ~(load_errors:Workspace_manager.load_error list) (workspace: Workspace.t) ->
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
  workspace_packages workspace |> List.for_each ~fn:
    (fun (pkg: Package_manifest.t) ->
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

let print_json = fun json ->
  print (Data.Json.to_string json);
  print "\n"

let run = fun ~(workspace_scan:workspace_scan) matches ->
  let json = ArgParser.get_flag matches "json" in
  match workspace_scan with
  | Loaded (workspace, load_errors) ->
      let workspace_manager = Workspace_manager.create () in
      if json then
        print_json (workspace_json ~workspace_manager ~load_errors workspace)
      else
        print_workspace ~load_errors workspace;
      Ok ()
  | NoWorkspace ->
      let message = "Not in a riot workspace" in
      if json then
        print_json (error_json ~kind:"no_workspace" ~message)
      else
        eprintln "❌ Not in a riot workspace";
      Error (Failure message)
  | ScanFailed err ->
      if json then
        print_json (error_json ~kind:"scan_failed" ~message:err)
      else
        eprintln ("\027[1;31mError\027[0m: " ^ err);
      Error (Failure err)
