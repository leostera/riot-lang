(** Workspace - TOML parsing for workspace manifests *)
open Std
open Std.Collections
open Std.Data
open Std.Result.Syntax

(** Types *)
type t = {
  name: string option;
  root: Path.t;
  target_dir_root: Path.t;
  packages: Package_manifest.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
}

(** Workspace TOML parsing *)
type manifest = {
  name: string option;
  members: Path.t list;
  dependencies: Package.dependency list;
  dev_dependencies: Package.dependency list;
  build_dependencies: Package.dependency list;
  profile_overrides: (string * Package.profile_override) list;
  target_dir: string option;
}

type dependency_field =
  | Path
  | Source
  | Github
  | Ref
  | Version

type dependency_error =
  | InvalidDependencyName of {
      raw_name: string;
      error: Package_name.error;
    }
  | InvalidDependencyRequirement of {
      dependency_name: string;
      requirement: string;
      error: Version.parse_error;
    }
  | DependencyCannotUseWorkspaceFlag of { dependency_name: string }
  | DependencyFieldMustBeString of {
      dependency_name: string;
      field: dependency_field;
    }
  | DependencyCannotSpecifySourceAndGithub of { dependency_name: string }
  | DependencyRefRequiresSource of { dependency_name: string }
  | BuiltinDependencyDoesNotSupportOverrides of { dependency_name: string }
  | BuiltinDependencyDoesNotSupportVersionRequirement of {
      dependency_name: string;
      requirement: string;
    }
  | DependencyMustBeStringOrTable of { dependency_name: string }

type error =
  | DependencySectionMustBeTable of { section_name: string }
  | DependencyError of dependency_error

let version_parse_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Version.Invalid_format msg -> msg
  | Version.Invalid_version_segment segment -> "invalid version segment: " ^ segment
  | Version.Invalid_pre_release_segment segment -> "invalid pre-release segment: " ^ segment

let dependency_field_name = fun __tmp1 ->
  match __tmp1 with
  | Path -> "path"
  | Source -> "source"
  | Github -> "github"
  | Ref -> "ref"
  | Version -> "version"

let dependency_error_message = fun __tmp1 ->
  match __tmp1 with
  | InvalidDependencyName { raw_name; error } ->
      "dependency name '" ^ raw_name ^ "' is invalid: " ^ Package_name.error_message error
  | InvalidDependencyRequirement { dependency_name; requirement; error } ->
      "dependency '"
      ^ dependency_name
      ^ "' has invalid version requirement '"
      ^ requirement
      ^ "': "
      ^ version_parse_error_to_string error
  | DependencyCannotUseWorkspaceFlag { dependency_name } ->
      "workspace dependency '" ^ dependency_name ^ "' cannot use workspace = true"
  | DependencyFieldMustBeString { dependency_name; field } ->
      "dependency '" ^ dependency_name ^ "' has non-string " ^ dependency_field_name field
  | DependencyCannotSpecifySourceAndGithub { dependency_name } ->
      "dependency '" ^ dependency_name ^ "' cannot specify both source and github"
  | DependencyRefRequiresSource { dependency_name } ->
      "dependency '" ^ dependency_name ^ "' cannot specify ref without source"
  | BuiltinDependencyDoesNotSupportOverrides { dependency_name } ->
      "builtin dependency '" ^ dependency_name ^ "' does not support path or source overrides"
  | BuiltinDependencyDoesNotSupportVersionRequirement { dependency_name; requirement } ->
      "builtin dependency '"
      ^ dependency_name
      ^ "' does not support version requirement '"
      ^ requirement
      ^ "'"
  | DependencyMustBeStringOrTable { dependency_name } ->
      "dependency '" ^ dependency_name ^ "' must be a string or table"

let error_message = fun __tmp1 ->
  match __tmp1 with
  | DependencySectionMustBeTable { section_name } -> "[" ^ section_name ^ "] must be a table"
  | DependencyError error -> dependency_error_message error

let validate_requirement = fun ~dependency_name requirement ->
  let trimmed = String.trim requirement in
  match Version.parse_requirement trimmed with
  | Ok requirement -> Ok requirement
  | Error err ->
      Error (DependencyError (InvalidDependencyRequirement {
        dependency_name;
        requirement;
        error = err;
      }))

let requirement_is_any = fun requirement ->
  String.equal
    (Version.requirement_to_string requirement)
    "*"

let validate_dependency_source = fun ~dependency_name (source: Package.dependency_source) ->
  if source.workspace then
    Error (DependencyError (DependencyCannotUseWorkspaceFlag { dependency_name }))
  else if Option.is_some source.ref_ && Option.is_none source.source_locator then
    Error (DependencyError (DependencyRefRequiresSource { dependency_name }))
  else if
    source.builtin
    && (Option.is_some source.path
    || Option.is_some source.source_locator
    || Option.is_some source.ref_)
  then
    Error (DependencyError (BuiltinDependencyDoesNotSupportOverrides { dependency_name }))
  else if source.builtin then
    match source.version with
    | None -> Ok { source with version = Some Version.any }
    | Some version when requirement_is_any version -> Ok source
    | Some version ->
        Error (DependencyError (BuiltinDependencyDoesNotSupportVersionRequirement {
          dependency_name;
          requirement = Version.requirement_to_string version;
        }))
  else if
    Option.is_some source.path
    || Option.is_some source.source_locator
    || Option.is_some source.version
  then
    Ok source
  else
    Ok { source with version = Some Version.any }

let normalize_source_locator = fun raw ->
  let raw = String.trim raw in
  let raw =
    if String.starts_with ~prefix:"https://" raw then
      String.sub raw ~offset:8 ~len:(String.length raw - 8)
    else if String.starts_with ~prefix:"http://" raw then
      String.sub raw ~offset:7 ~len:(String.length raw - 7)
    else
      raw
  in
  if String.ends_with ~suffix:".git" raw then
    String.sub raw ~offset:0 ~len:(String.length raw - 4)
  else
    raw

let github_locator_of_value = fun value -> "github.com/" ^ String.trim value

let parse_dependency: string -> Toml.value -> (Package.dependency, error) result = fun
  raw_name value ->
  let* name =
    Package_name.from_string raw_name
    |> Result.map_err ~fn:(fun error -> DependencyError (InvalidDependencyName { raw_name; error }))
  in
  let dependency_name = Package_name.to_string name in
  let make_dependency source: Package.dependency = { name; source } in
  match value with
  | Toml.Table attrs -> (
      let path =
        match Fields.get "path" attrs with
        | Some (Toml.String path_str) -> Ok (Some (Path.v path_str))
        | Some _ ->
            Error (DependencyError (DependencyFieldMustBeString { dependency_name; field = Path }))
        | None -> Ok None
      in
      let source_locator =
        match (Fields.get "source" attrs, Fields.get "github" attrs) with
        | (Some _, Some _) ->
            Error (DependencyError (DependencyCannotSpecifySourceAndGithub { dependency_name }))
        | (Some (Toml.String locator), None) -> Ok (Some (normalize_source_locator locator))
        | (Some _, None) ->
            Error (DependencyError (DependencyFieldMustBeString { dependency_name; field = Source }))
        | (None, Some (Toml.String github)) -> Ok (Some (github_locator_of_value github))
        | (None, Some _) ->
            Error (DependencyError (DependencyFieldMustBeString { dependency_name; field = Github }))
        | (None, None) -> Ok None
      in
      let ref_ =
        match Fields.get "ref" attrs with
        | Some (Toml.String ref_) -> Ok (Some (String.trim ref_))
        | Some _ ->
            Error (DependencyError (DependencyFieldMustBeString { dependency_name; field = Ref }))
        | None -> Ok None
      in
      let version =
        match Fields.get "version" attrs with
        | Some (Toml.String requirement) ->
            validate_requirement ~dependency_name requirement
            |> Result.map ~fn:(fun version -> Some version)
        | Some _ ->
            Error (DependencyError (DependencyFieldMustBeString { dependency_name; field = Version }))
        | None -> Ok None
      in
      match (path, source_locator, ref_, version) with
      | ((Error _ as err), _, _, _) -> err
      | (_, (Error _ as err), _, _) -> err
      | (_, _, (Error _ as err), _) -> err
      | (_, _, _, (Error _ as err)) -> err
      | (Ok path, Ok source_locator, Ok ref_, Ok version) ->
          validate_dependency_source
            ~dependency_name
            {
              workspace = false;
              builtin = Package.is_builtin_dependency_name dependency_name;
              path;
              source_locator;
              ref_;
              version;
            }
          |> Result.map ~fn:make_dependency
    )
  | Toml.String requirement -> (
      match validate_requirement ~dependency_name requirement with
      | Error _ as err -> err
      | Ok version ->
          validate_dependency_source
            ~dependency_name
            {
              workspace = false;
              builtin = Package.is_builtin_dependency_name dependency_name;
              path = None;
              source_locator = None;
              ref_ = None;
              version = Some version;
            }
          |> Result.map ~fn:make_dependency
    )
  | _ -> Error (DependencyError (DependencyMustBeStringOrTable { dependency_name }))

let parse_dependencies: (string * Toml.value) list -> (Package.dependency list, error) result = fun
  items ->
  let rec loop acc entries =
    match entries with
    | [] -> Ok (List.reverse acc)
    | (name, value) :: rest -> (
        match parse_dependency name value with
        | Ok dep -> loop (dep :: acc) rest
        | Error _ as err -> err
      )
  in
  loop [] items

let parse_dependency_section section_name (toml: Toml.value) =
  match toml with
  | Toml.Table items -> (
      match Fields.get section_name items with
      | Some (Toml.Table dep_items) -> parse_dependencies dep_items
      | Some _ -> Error (DependencySectionMustBeTable { section_name })
      | None -> Ok []
    )
  | _ -> Ok []

let parse_members: Toml.value -> Path.t list = fun toml ->
  match toml with
  | Toml.Table items -> (
      match Fields.get "workspace" items with
      | Some (Toml.Table workspace_items) -> (
          match Fields.get "members" workspace_items with
          | Some (Toml.Array members) ->
              List.filter_map members ~fn:(fun m -> Option.map (Toml.get_string m) ~fn:Path.v)
          | _ -> []
        )
      | _ -> []
    )
  | _ -> []

let parse_workspace_name: Toml.value -> string option = fun toml ->
  match toml with
  | Toml.Table items -> (
      match Fields.get "workspace" items with
      | Some (Toml.Table workspace_items) -> (
          match Fields.get "name" workspace_items with
          | Some (Toml.String name) ->
              let trimmed = String.trim name in
              if String.is_empty trimmed then
                None
              else
                Some trimmed
          | _ -> None
        )
      | _ -> None
    )
  | _ -> None

let parse_workspace_dependencies: Toml.value -> Package.dependency list = fun toml ->
  Log.debug ("[WORKSPACE] parse_workspacE_dependencies has items: " ^ Toml.to_string toml);
  parse_dependency_section "dependencies" toml
  |> Result.map_err ~fn:error_message
  |> Result.expect ~msg:"workspace dependencies should be parsed through from_toml"

let parse_workspace_dev_dependencies: Toml.value -> Package.dependency list = fun toml ->
  parse_dependency_section "dev-dependencies" toml
  |> Result.map_err ~fn:error_message
  |> Result.expect ~msg:"workspace dev dependencies should be parsed through from_toml"

let parse_workspace_build_dependencies: Toml.value -> Package.dependency list = fun toml ->
  parse_dependency_section "build-dependencies" toml
  |> Result.map_err ~fn:error_message
  |> Result.expect ~msg:"workspace build dependencies should be parsed through from_toml"

let parse_profile_overrides: Toml.value -> (string * Profile.profile_override) list = fun toml ->
  Log.debug "[WORKSPACE] parse_profile_overrides called";
  match toml with
  | Toml.Table items -> (
      Log.debug
        ("[WORKSPACE] Looking for [profile] in TOML with "
        ^ Int.to_string (List.length items)
        ^ " top-level keys");
      Log.debug
        ("[WORKSPACE] Top-level keys: "
        ^ String.concat ", " (List.map items ~fn:(fun (key, _) -> key)));
      match Fields.get "profile" items with
      | Some (Toml.Table profile_items) ->
          Log.debug
            ("[WORKSPACE] Found [profile] section with "
            ^ Int.to_string (List.length profile_items)
            ^ " profiles");
          let result =
            List.filter_map
              profile_items
              ~fn:(fun (profile_name, value) ->
                Log.debug ("[WORKSPACE] Parsing profile: " ^ profile_name);
                match value with
                | Toml.Table profile_table ->
                    Log.debug
                      ("[WORKSPACE] Profile "
                      ^ profile_name
                      ^ " has "
                      ^ Int.to_string (List.length profile_table)
                      ^ " fields");
                    Some (profile_name, Profile.override_from_toml profile_table)
                | _ ->
                    Log.debug ("[WORKSPACE] Profile " ^ profile_name ^ " is not a table, skipping");
                    None)
          in
          Log.debug
            ("[WORKSPACE] Parsed " ^ Int.to_string (List.length result) ^ " profile overrides");
          result
      | _ ->
          Log.debug "[WORKSPACE] No [profile] section found in TOML";
          []
    )
  | _ ->
      Log.debug "[WORKSPACE] TOML root is not a table";
      []

let parse_target_dir: Toml.value -> string option = fun toml ->
  match toml with
  | Toml.Table items -> (
      match Fields.get "riot" items with
      | Some (Toml.Table riot_items) -> (
          match Fields.get "target_dir" riot_items with
          | Some (Toml.String target_dir) -> Some target_dir
          | _ -> None
        )
      | _ -> None
    )
  | _ -> None

let from_toml: Toml.value -> (manifest, error) result = fun toml ->
  let members = parse_members toml in
  let name = parse_workspace_name toml in
  match parse_dependency_section "dependencies" toml with
  | Error _ as err -> err
  | Ok dependencies -> (
      match parse_dependency_section "dev-dependencies" toml with
      | Error _ as err -> err
      | Ok dev_dependencies -> (
          match parse_dependency_section "build-dependencies" toml with
          | Error _ as err -> err
          | Ok build_dependencies ->
              let profile_overrides = parse_profile_overrides toml in
              let target_dir = parse_target_dir toml in
              Ok {
                name;
                members;
                dependencies;
                dev_dependencies;
                build_dependencies;
                profile_overrides;
                target_dir;
              }
        )
    )

let resolve_target_dir_root = fun ~root ?target_dir () ->
  let target_dir =
    match target_dir with
    | Some target_dir -> target_dir
    | None -> "_build"
  in
  let target_dir_path = Path.v target_dir in
  if Path.is_absolute target_dir_path then
    target_dir_path
  else
    Path.(root / target_dir_path)

let make
  ?name
  ~root
  ~packages
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  ?(profile_overrides = [])
  ?target_dir
  () = {
  name;
  root;
  target_dir_root = resolve_target_dir_root ~root ?target_dir ();
  packages;
  dependencies;
  dev_dependencies;
  build_dependencies;
  profile_overrides;
}

let make_realized
  ?name
  ~root
  ~packages
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  ?(profile_overrides = [])
  ?target_dir
  () =
  make
    ?name
    ~root
    ~packages:(List.map packages ~fn:Package_manifest.from_package)
    ~dependencies
    ~dev_dependencies
    ~build_dependencies
    ~profile_overrides
    ?target_dir
    ()

let dependencies_for_scope = fun scope (workspace: t) ->
  match scope with
  | Package.Normal -> workspace.dependencies
  | Package.Dev -> workspace.dependencies @ workspace.dev_dependencies
  | Package.Build -> workspace.build_dependencies

let package_root = fun (workspace: t) (pkg: Package_manifest.t) ->
  if Package_manifest.is_workspace_member pkg then
    Path.normalize Path.(workspace.root / pkg.relative_path)
  else
    Path.normalize pkg.path

let find_package_for_path = fun (workspace: t) ~path ->
  let path = Path.normalize path in
  let contains_path (pkg: Package_manifest.t) =
    let package_root = package_root workspace pkg in
    Path.equal path package_root || match Path.strip_prefix path ~prefix:package_root with
    | Ok _ -> true
    | Error _ -> false
  in
  workspace.packages
  |> List.filter ~fn:contains_path
  |> List.sort
    ~compare:(fun (left: Package_manifest.t) (right: Package_manifest.t) ->
      Int.compare
        (String.length (Path.to_string (package_root workspace right)))
        (String.length (Path.to_string (package_root workspace left))))
  |> fun __tmp1 ->
    match __tmp1 with
    | pkg :: _ -> Some pkg
    | [] -> None

let realize_package = fun ~intent manifest -> Package_manifest.realize ~intent manifest

let realize_packages = fun ~intent workspace ->
  List.map
    workspace.packages
    ~fn:(realize_package ~intent)

(** Utility functions *)
let project_id = fun workspace ->
  let root_str = Path.to_string workspace.root in
  String.map
    root_str
    ~fn:(fun c ->
      if c = '/' then
        '-'
      else
        c)

let server_port = fun workspace ->
  let root_str = Path.to_string workspace.root in
  let hash = Std.Crypto.hash_string root_str in
  let hash_int = Std.Crypto.Digest.to_int hash in
  let port_range = 65_535 - 49_152 in
  50_152 + (Int.abs hash_int mod port_range)

(** Command discovery functions - moved here to avoid circular dependency *)
let discover_commands: t -> Package_command.t list = fun workspace ->
  List.map workspace.packages ~fn:(fun (pkg: Package_manifest.t) -> pkg.commands)
  |> List.concat

let find_command: t -> string -> Package_command.t option = fun workspace name ->
  discover_commands workspace
  |> List.find ~fn:(fun (cmd: Package_command.t) -> cmd.name = name)

let discover_fix_providers: t -> Fix_provider.t list = fun workspace ->
  List.map workspace.packages ~fn:(fun (pkg: Package_manifest.t) -> pkg.fix_providers)
  |> List.concat

module Tests = struct
  let package_name value =
    Package_name.from_string value
    |> Result.expect ~msg:"expected valid package name"

  let test_parse_workspace_toml () = Ok () [@test]

  let test_parse_target_dir () =
    let toml =
      Std.Data.Toml.parse
        {|
[workspace]
members = ["packages/foo"]

[riot]
target_dir = "build-out"
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let manifest =
      from_toml toml
      |> Result.expect ~msg:"expected workspace manifest"
    in
    if manifest.target_dir = Some "build-out" then
      Ok ()
    else
      Error "expected [riot].target_dir to be parsed" [@test]

  let test_parse_workspace_name () =
    let toml =
      Std.Data.Toml.parse {|
[workspace]
name = "riot"
members = ["packages/foo"]
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let manifest =
      from_toml toml
      |> Result.expect ~msg:"expected workspace manifest"
    in
    if manifest.name = Some "riot" then
      Ok ()
    else
      Error "expected [workspace].name to be parsed" [@test]

  let test_make_uses_custom_target_dir () =
    let workspace = make ~root:(Path.v "/tmp/example") ~packages:[] ~target_dir:"build-out" () in
    if Path.to_string workspace.target_dir_root = "/tmp/example/build-out" then
      Ok ()
    else
      Error "expected custom target_dir_root" [@test]

  let test_workspace_dependencies_parse_registry_requirements () =
    let toml =
      Std.Data.Toml.parse {|
[workspace]
members = []

[dependencies]
std = ">= 1.2.3"
|}
      |> Result.expect ~msg:"expected workspace toml to parse"
    in
    match from_toml toml with
    | Error err -> Error (error_message err)
    | Ok manifest ->
        (
          match manifest.dependencies with
          | [
              {
                Package.source = {
                  workspace = false;
                  builtin = false;
                  path = None;
                  source_locator = None;
                  ref_ = None;
                  version = Some requirement;
                };
                _;
              };
            ] ->
              if String.equal (Version.requirement_to_string requirement) ">= 1.2.3" then
                Ok ()
              else
                Error "expected workspace registry requirement to be parsed structurally"
          | _ -> Error "expected workspace dependency to parse as a registry requirement"
        ) [@test]

  let test_workspace_dependencies_reject_non_string_version () =
    let toml =
      Std.Data.Toml.parse {|
[workspace]
members = []

[dependencies]
std = { version = 123 }
|}
      |> Result.expect ~msg:"expected workspace toml to parse"
    in
    match from_toml toml with
    | Error (
      DependencyError (
        DependencyFieldMustBeString { dependency_name = "std"; field = Version }
      )
    ) ->
        Ok ()
    | Error err -> Error ("expected non-string version error, got: " ^ error_message err)
    | Ok _ ->
        Error "expected workspace manifest parse to fail for non-string dependency version" [@test]

  let test_discover_fix_providers () =
    let package_toml =
      Std.Data.Toml.parse
        {|
[package]
name = "std"
version = "0.1.0"

[riot.fix.provider]
path = "fix/no_stdlib_provider.ml"
rules = ["no-stdlib"]
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let package =
      Package.from_toml
        package_toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:(Path.v "/tmp/example/packages/std")
        ~relative_path:(Path.v "packages/std")
      |> Result.expect ~msg:"expected package manifest"
    in
    let workspace = make_realized ~root:(Path.v "/tmp/example") ~packages:[ package ] () in
    match discover_fix_providers workspace with
    | [ provider ] ->
        if Package_name.equal
          provider.package_name
          (
            Package_name.from_string "std"
            |> Result.expect ~msg:"expected valid package name"
          )
        && String.equal
          (Path.to_string provider.source_path)
          "/tmp/example/packages/std/fix/no_stdlib_provider.ml"
        && String.equal provider.name "std"
        && provider.rules = [ "std:no-stdlib" ] then
          Ok ()
        else
          Error "expected provider metadata to round-trip"
    | _ -> Error "expected one fix provider" [@test]

  let test_parse_workspace_dependency_classes () =
    let toml =
      Std.Data.Toml.parse
        {|
[workspace]
members = ["packages/foo"]

[dependencies]
std = { path = "packages/std" }

[dev-dependencies]
propane = { path = "packages/propane" }

[build-dependencies]
fixme = { path = "packages/fixme" }
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let manifest =
      from_toml toml
      |> Result.expect ~msg:"expected workspace manifest"
    in
    if
      List.map manifest.dependencies ~fn:(fun (dep: Package.dependency) -> dep.Package.name)
      = [ package_name "std" ]
      && List.map manifest.dev_dependencies ~fn:(fun (dep: Package.dependency) -> dep.Package.name)
      = [ package_name "propane" ]
      && List.map
        manifest.build_dependencies
        ~fn:(fun (dep: Package.dependency) -> dep.Package.name)
      = [ package_name "fixme" ]
    then
      Ok ()
    else
      Error "expected workspace dependency classes to parse" [@test]
end [@test]
