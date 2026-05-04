(** Package - TOML parsing for package manifests *)
open Std
open Std.Data
open Std.Collections
open Std.Result.Syntax

(** Types *)
type dependency_source = {
  workspace: bool;
  builtin: bool;
  path: Path.t option;
  source_locator: string option;
  ref_: string option;
  version: Std.Version.requirement option;
}

type dependency_scope =
  | Normal
  | Dev
  | Build

type key =
  | Key of string

type dependency = {
  name: Package_name.t;
  source: dependency_source;
}

type resolved_dependency = {
  requirement: dependency;
  resolved_id: Lockfile.package_id;
}

type publish_metadata = {
  version: Std.Version.t option;
  description: string option;
  license: string option;
  is_public: bool option;
}

type binary = {
  name: string;
  path: Path.t;
}

type library = {
  path: Path.t;
}

type sources = {
  src: Path.t list;
  native: Path.t list;
  tests: Path.t list;
  examples: Path.t list;
  bench: Path.t list;
}

type dev_artifacts = { tests: bool; examples: bool; benches: bool }

type realization_intent =
  | Build
  | Runtime
  | Dev
  | Run
  | Test
  | Bench
  | Doc
  | Check

type target_platform = string

(* "macos", "linux", "windows", etc. *)
(** Re-export types from Profile *)

type 'value override = 'value Profile.override

type profile_override = Profile.profile_override

(** Target-specific override - can override any profile field for a specific platform *)
type target_override = {
  profile_override: Profile.profile_override option;
  (* Profile fields that can be overridden *)
}

type compiler_config = {
  profile_overrides: (string * profile_override) list;
  (* "debug" -> override, "release" -> override *)
  target_overrides: (target_platform * target_override) list;
  (* "macos" -> override, etc. *)
}

type foreign_dependency = {
  name: string;
  path: Path.t;
  inputs: Path.t list;
  build_cmd: string list;
  clean_cmd: string list option;
  test_cmd: string list option;
  outputs: Path.t list;
  env: (string * string) list;
}

type manifest_spec = {
  name: Package_name.t;
  path: Path.t;
  relative_path: Path.t;
  dependencies: dependency list;
  dev_dependencies: dependency list;
  build_dependencies: dependency list;
  foreign_dependencies: foreign_dependency list;
  declared_binaries: binary list;
  library: library option;
  compiler: compiler_config;
  commands: Package_command.t list;
  fix_providers: Fix_provider.t list;
  publish: publish_metadata;
}

type publish_field =
  | PublishVersion
  | PublishDescription
  | PublishLicense
  | PublishPublic

type dependency_field =
  | DependencyWorkspace
  | DependencyPath
  | DependencySource
  | DependencyGithub
  | DependencyRef
  | DependencyVersion

type publish_metadata_error =
  | PackageSectionMustBeTable
  | InvalidPackageVersion of {
      package_name: string;
      version: string;
      error: Std.Version.parse_error;
    }
  | NonStringPublishField of {
      package_name: string;
      field: publish_field;
    }
  | NonBooleanPublicFlag of { package_name: string }

type dependency_error =
  | InvalidDependencyName of {
      raw_name: string;
      error: Package_name.error;
    }
  | InvalidDependencyRequirement of {
      dependency_name: string;
      requirement: string;
      error: Std.Version.parse_error;
    }
  | NonBooleanWorkspaceFlag of { dependency_name: string }
  | NonStringDependencyField of {
      dependency_name: string;
      field: dependency_field;
    }
  | DependencyCannotSpecifySourceAndGithub of { dependency_name: string }
  | WorkspaceDependencyCannotSpecifyOverrides of { dependency_name: string }
  | DependencyRefRequiresSource of { dependency_name: string }
  | BuiltinDependencyCannotSpecifyOverrides of { dependency_name: string }
  | BuiltinDependencyVersionRequirementNotSupported of {
      dependency_name: string;
      requirement: string;
    }
  | DependencyMustBeStringOrTable of { dependency_name: string }

type manifest_error =
  | ManifestMustBeTable
  | InvalidPackageName of {
      raw_name: string;
      error: Package_name.error;
    }
  | InvalidPublishMetadata of publish_metadata_error
  | DependencySectionMustBeTable of { section_name: string }
  | InvalidDependency of dependency_error

type t = {
  name: Package_name.t;
  path: Path.t;
  relative_path: Path.t;
  dependencies: dependency list;
  dev_dependencies: dependency list;
  build_dependencies: dependency list;
  foreign_dependencies: foreign_dependency list;
  binaries: binary list;
  library: library option;
  sources: sources;
  compiler: compiler_config;
  commands: Package_command.t list;
  fix_providers: Fix_provider.t list;
  publish: publish_metadata;
}

type resolved = {
  package: t;
  id: Lockfile.package_id;
  manifest_path: Path.t;
  materialized_root: Path.t;
  provenance: Lockfile.provenance;
  runtime_resolved: resolved_dependency list;
  build_resolved: resolved_dependency list;
  dev_resolved: resolved_dependency list;
}

let default_publish_metadata = {
  version = None;
  description = None;
  license = None;
  is_public = None;
}

let empty_sources = {
  src = [];
  native = [];
  tests = [];
  examples = [];
  bench = [];
}

let all_dev_artifacts = { tests = true; examples = true; benches = true }

let compare_path = fun left right -> String.compare (Path.to_string left) (Path.to_string right)

let compare_option compare_value left right =
  match (left, right) with
  | (None, None) -> Order.EQ
  | (None, Some _) -> Order.LT
  | (Some _, None) -> Order.GT
  | (Some left, Some right) -> compare_value left right

let compare_package_name = Package_name.compare

let compare_dependency_source = fun left right ->
  let by_workspace = Bool.compare left.workspace right.workspace in
  if by_workspace != Order.EQ then
    by_workspace
  else
    let by_builtin = Bool.compare left.builtin right.builtin in
    if by_builtin != Order.EQ then
      by_builtin
    else
      let by_path = compare_option compare_path left.path right.path in
      if by_path != Order.EQ then
        by_path
      else
        let by_source_locator =
          compare_option String.compare left.source_locator right.source_locator
        in
        if by_source_locator != Order.EQ then
          by_source_locator
        else
          let by_ref = compare_option String.compare left.ref_ right.ref_ in
          if by_ref != Order.EQ then
            by_ref
          else
            compare_option
              (fun left right ->
                String.compare
                  (Version.requirement_to_string left)
                  (Version.requirement_to_string right))
              left.version
              right.version

let compare_dependency = fun (left: dependency) (right: dependency) ->
  let by_name = compare_package_name left.name right.name in
  if by_name != Order.EQ then
    by_name
  else
    compare_dependency_source left.source right.source

let compare_binary = fun (left: binary) (right: binary) ->
  let by_name = String.compare left.name right.name in
  if by_name != Order.EQ then
    by_name
  else
    compare_path left.path right.path

let compare_fix_provider = fun (left: Fix_provider.t) (right: Fix_provider.t) ->
  let by_name = String.compare left.name right.name in
  if by_name != Order.EQ then
    by_name
  else
    compare_path left.source_path right.source_path

let compare_profile_override = fun (left_name, _) (right_name, _) ->
  String.compare
    left_name
    right_name

let compare_target_override = fun (left_name, _) (right_name, _) ->
  String.compare
    left_name
    right_name

let compare_foreign_dependency = fun (left: foreign_dependency) (right: foreign_dependency) ->
  let by_name = String.compare left.name right.name in
  if by_name != Order.EQ then
    by_name
  else
    compare_path left.path right.path

let canonicalize_path_list = fun paths -> List.unique paths ~compare:compare_path

let canonicalize_sources = fun sources ->
  {
    src = canonicalize_path_list sources.src;
    native = canonicalize_path_list sources.native;
    tests = canonicalize_path_list sources.tests;
    examples = canonicalize_path_list sources.examples;
    bench = canonicalize_path_list sources.bench;
  }

let canonicalize_foreign_dependency = fun (foreign: foreign_dependency) -> {
  foreign with
  inputs = canonicalize_path_list foreign.inputs;
  outputs = canonicalize_path_list foreign.outputs;
}

let canonicalize_manifest_spec = fun (spec: manifest_spec) ->
  {
    spec with
    dependencies = List.sort spec.dependencies ~compare:compare_dependency;
    dev_dependencies = List.sort spec.dev_dependencies ~compare:compare_dependency;
    build_dependencies = List.sort spec.build_dependencies ~compare:compare_dependency;
    foreign_dependencies =
      spec.foreign_dependencies
      |> List.map ~fn:canonicalize_foreign_dependency
      |> List.sort ~compare:compare_foreign_dependency;
    declared_binaries = List.sort spec.declared_binaries ~compare:compare_binary;
    compiler = {
      profile_overrides = List.sort
        spec.compiler.profile_overrides
        ~compare:compare_profile_override;
      target_overrides = List.sort spec.compiler.target_overrides ~compare:compare_target_override;
    };
    fix_providers = List.sort spec.fix_providers ~compare:compare_fix_provider;
  }

let canonicalize = fun (pkg: t) ->
  {
    pkg with
    dependencies = List.sort pkg.dependencies ~compare:compare_dependency;
    dev_dependencies = List.sort pkg.dev_dependencies ~compare:compare_dependency;
    build_dependencies = List.sort pkg.build_dependencies ~compare:compare_dependency;
    foreign_dependencies =
      pkg.foreign_dependencies
      |> List.map ~fn:canonicalize_foreign_dependency
      |> List.sort ~compare:compare_foreign_dependency;
    binaries = List.sort pkg.binaries ~compare:compare_binary;
    sources = canonicalize_sources pkg.sources;
    compiler = {
      profile_overrides = List.sort pkg.compiler.profile_overrides ~compare:compare_profile_override;
      target_overrides = List.sort pkg.compiler.target_overrides ~compare:compare_target_override;
    };
    fix_providers = List.sort pkg.fix_providers ~compare:compare_fix_provider;
  }

let make = fun
  ~name
  ~path
  ~relative_path
  ?(dependencies = [])
  ?(dev_dependencies = [])
  ?(build_dependencies = [])
  ?(foreign_dependencies = [])
  ?(binaries = [])
  ?library
  ?(sources = empty_sources)
  ?(compiler = {profile_overrides = []; target_overrides = []})
  ?(commands = [])
  ?(fix_providers = [])
  ?(publish = default_publish_metadata)
  () ->
  canonicalize
    {
      name;
      path;
      relative_path;
      dependencies;
      dev_dependencies;
      build_dependencies;
      foreign_dependencies;
      binaries;
      library;
      sources;
      compiler;
      commands;
      fix_providers;
      publish;
    }

let synthetic = fun ~name ~path ~relative_path -> make ~name ~path ~relative_path ()

let equal = fun a b -> Package_name.equal a.name b.name && a.path = b.path

let root_module_name = fun (pkg: t) ->
  Module_name.(from_string (Package_name.to_string pkg.name)
  |> to_string)

let key_of_string = fun value -> Key value

let key_to_string = fun (Key value) -> value

let key_equal = fun left right -> String.equal (key_to_string left) (key_to_string right)

let key_compare = fun left right -> String.compare (key_to_string left) (key_to_string right)

let dependencies_for_scope = fun scope (pkg: t) ->
  match scope with
  | Normal -> pkg.dependencies
  | Dev -> pkg.dependencies @ pkg.dev_dependencies
  | Build -> pkg.build_dependencies

let is_builtin_dependency_name = fun name ->
  match name with
  | "unix"
  | "stdlib"
  | "threads"
  | "str"
  | "bigarray"
  | "dynlink"
  | "compiler-libs"
  | "graphics" -> true
  | _ -> false

let is_builtin_dependency = fun (dep: dependency) -> dep.source.builtin

let binary_scope: binary -> dependency_scope = fun (bin: binary) ->
  let path_str = Path.to_string bin.path in
  if
    String.starts_with ~prefix:"tests/" path_str
    || String.starts_with ~prefix:"examples/" path_str
    || String.starts_with ~prefix:"bench/" path_str
  then
    Dev
  else
    Normal

let dev_artifact_selected_for_binary = fun (dev_artifacts: dev_artifacts) (bin: binary) ->
  let path_str = Path.to_string bin.path in
  if String.starts_with ~prefix:"tests/" path_str then
    dev_artifacts.tests
  else if String.starts_with ~prefix:"examples/" path_str then
    dev_artifacts.examples
  else if String.starts_with ~prefix:"bench/" path_str then
    dev_artifacts.benches
  else
    false

let scope_of_binary_name = fun (pkg: t) ~binary_name ->
  List.find pkg.binaries ~fn:(fun (bin: binary) -> String.equal bin.name binary_name)
  |> Option.map ~fn:binary_scope

let binaries_for_scope = fun ?(dev_artifacts = all_dev_artifacts) scope (pkg: t) ->
  match scope with
  | Normal -> List.filter pkg.binaries ~fn:(fun bin -> binary_scope bin = Normal)
  | Dev ->
      List.filter
        pkg.binaries
        ~fn:(fun bin -> binary_scope bin = Dev && dev_artifact_selected_for_binary dev_artifacts bin)
  | Build -> []

let commands_for_scope = fun scope (pkg: t) ->
  match scope with
  | Normal -> pkg.commands
  | Dev
  | Build -> []

let sources_for_scope = fun ?(dev_artifacts = all_dev_artifacts) scope (pkg: t) ->
  match scope with
  | Normal -> { pkg.sources with tests = []; examples = []; bench = [] }
  | Dev ->
      let src =
        match pkg.library with
        | Some _ -> []
        | None -> pkg.sources.src
      in
      {
        src;
        native = [];
        tests =
          if dev_artifacts.tests then
            pkg.sources.tests
          else
            [];
        examples =
          if dev_artifacts.examples then
            pkg.sources.examples
          else
            [];
        bench =
          if dev_artifacts.benches then
            pkg.sources.bench
          else
            [];
      }
  | Build ->
      {
        src = [];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }

let for_scope = fun ?(dev_artifacts = all_dev_artifacts) scope (pkg: t) ->
  match scope with
  | Normal ->
      canonicalize
        {
          pkg with
          dev_dependencies = [];
          binaries = binaries_for_scope Normal pkg;
          commands = commands_for_scope Normal pkg;
          sources = sources_for_scope Normal pkg;
        }
  | Dev ->
      canonicalize
        {
          pkg with
          library = None;
          binaries = binaries_for_scope ~dev_artifacts Dev pkg;
          commands = commands_for_scope Dev pkg;
          sources = sources_for_scope ~dev_artifacts Dev pkg;
        }
  | Build ->
      canonicalize
        {
          pkg with
          dependencies = [];
          dev_dependencies = [];
          library = None;
          binaries = [];
          commands = commands_for_scope Build pkg;
          sources = sources_for_scope Build pkg;
        }

let sources_for_binary = fun (bin: binary) (pkg: t) ->
  let path = Path.to_string bin.path in
  if String.starts_with ~prefix:"tests/" path then
    { empty_sources with tests = pkg.sources.tests }
  else if String.starts_with ~prefix:"examples/" path then
    { empty_sources with examples = pkg.sources.examples }
  else if String.starts_with ~prefix:"bench/" path then
    { empty_sources with bench = pkg.sources.bench }
  else
    (
      if Option.is_some pkg.library then
        { empty_sources with src = [ bin.path ] }
      else
        { empty_sources with src = pkg.sources.src; native = pkg.sources.native }
    )

let for_binary = fun ~binary_name (pkg: t) ->
  List.find pkg.binaries ~fn:(fun (bin: binary) -> String.equal bin.name binary_name)
  |> Option.map
    ~fn:(fun bin ->
      let dependency_scope = binary_scope bin in
      let dependencies =
        match dependency_scope with
        | Normal
        | Dev -> pkg.dependencies
        | Build -> []
      in
      let dev_dependencies =
        match dependency_scope with
        | Normal -> []
        | Dev -> pkg.dev_dependencies
        | Build -> []
      in
      canonicalize
        {
          pkg with
          library = None;
          dependencies;
          dev_dependencies;
          binaries = [ bin ];
          commands = [];
          sources = sources_for_binary bin pkg;
        })

let build_graph_dependencies = fun (pkg: t) -> pkg.dependencies @ pkg.dev_dependencies

let all_dependencies = fun (pkg: t) ->
  (pkg.dependencies @ pkg.dev_dependencies) @ pkg.build_dependencies

let resolve_scope = fun ~scope_name ~manifest_dependencies ~lock_dependencies ->
  let rec loop (acc: resolved_dependency list) (requirements: dependency list) =
    match requirements with
    | [] -> Ok (List.reverse acc)
    | (requirement: dependency) :: rest ->
        if is_builtin_dependency requirement then
          loop acc rest
        else
          (
            match List.find
              lock_dependencies
              ~fn:(fun (dep: Lockfile.dependency) -> Package_name.equal dep.name requirement.name) with
            | Some resolved -> loop ({ requirement; resolved_id = resolved.package } :: acc) rest
            | None ->
                Error ("lockfile is missing resolved "
                ^ scope_name
                ^ " dependency '"
                ^ Package_name.to_string requirement.name
                ^ "'")
          )
  in
  loop [] manifest_dependencies

let resolve = fun
  ~(package:t) ~(lock_package:Lockfile.package) ~manifest_path ~materialized_root ->
  match resolve_scope
    ~scope_name:"runtime"
    ~manifest_dependencies:package.dependencies
    ~lock_dependencies:lock_package.dependencies with
  | Error _ as err -> err
  | Ok dependencies -> (
      match resolve_scope
        ~scope_name:"build"
        ~manifest_dependencies:package.build_dependencies
        ~lock_dependencies:lock_package.build_dependencies with
      | Error _ as err -> err
      | Ok build_dependencies -> (
          match resolve_scope
            ~scope_name:"dev"
            ~manifest_dependencies:package.dev_dependencies
            ~lock_dependencies:lock_package.dev_dependencies with
          | Error _ as err -> err
          | Ok dev_dependencies ->
              Ok {
                package;
                id = lock_package.id;
                manifest_path;
                materialized_root;
                provenance = lock_package.provenance;
                runtime_resolved = dependencies;
                build_resolved = build_dependencies;
                dev_resolved = dev_dependencies;
              }
        )
    )

(**
   Check if this package is a workspace member (not an external dependency).
   External dependencies have relative_path that escapes the workspace (starts with "../")
   or uses absolute paths.
*)
let is_workspace_member: t -> bool = fun pkg ->
  let rel_str = Path.to_string pkg.relative_path in
  not (String.starts_with ~prefix:"../" rel_str || Path.is_absolute pkg.relative_path)

(** Validate package name according to Riot naming conventions *)
let validate_name = Package_name.from_string

let version_parse_error_to_string = fun err ->
  match err with
  | Version.Invalid_format msg -> msg
  | Version.Invalid_version_segment segment -> "invalid version segment: " ^ segment
  | Version.Invalid_pre_release_segment segment -> "invalid pre-release segment: " ^ segment

let publish_field_name = fun __tmp1 ->
  match __tmp1 with
  | PublishVersion -> "version"
  | PublishDescription -> "description"
  | PublishLicense -> "license"
  | PublishPublic -> "public"

let dependency_field_name = fun __tmp1 ->
  match __tmp1 with
  | DependencyWorkspace -> "workspace"
  | DependencyPath -> "path"
  | DependencySource -> "source"
  | DependencyGithub -> "github"
  | DependencyRef -> "ref"
  | DependencyVersion -> "version"

let publish_metadata_error_message = fun __tmp1 ->
  match __tmp1 with
  | PackageSectionMustBeTable -> "[package] must be a table"
  | InvalidPackageVersion { package_name; version; error } ->
      "package '"
      ^ package_name
      ^ "' has invalid version '"
      ^ version
      ^ "': "
      ^ version_parse_error_to_string error
  | NonStringPublishField { package_name; field } ->
      "package '" ^ package_name ^ "' has non-string " ^ publish_field_name field
  | NonBooleanPublicFlag { package_name } ->
      "package '" ^ package_name ^ "' has non-boolean public flag"

let dependency_error_message = fun __tmp1 ->
  match __tmp1 with
  | InvalidDependencyName { raw_name; error } ->
      "dependency '" ^ raw_name ^ "' has invalid package name: " ^ Package_name.error_message error
  | InvalidDependencyRequirement { dependency_name; requirement; error } ->
      "dependency '"
      ^ dependency_name
      ^ "' has invalid version requirement '"
      ^ requirement
      ^ "': "
      ^ version_parse_error_to_string error
  | NonBooleanWorkspaceFlag { dependency_name } ->
      "dependency '" ^ dependency_name ^ "' has non-boolean workspace flag"
  | NonStringDependencyField { dependency_name; field } ->
      "dependency '" ^ dependency_name ^ "' has non-string " ^ dependency_field_name field
  | DependencyCannotSpecifySourceAndGithub { dependency_name } ->
      "dependency '" ^ dependency_name ^ "' cannot specify both source and github"
  | WorkspaceDependencyCannotSpecifyOverrides { dependency_name } ->
      "dependency '"
      ^ dependency_name
      ^ "' cannot combine workspace = true with path, source, ref, or version"
  | DependencyRefRequiresSource { dependency_name } ->
      "dependency '" ^ dependency_name ^ "' cannot specify ref without source"
  | BuiltinDependencyCannotSpecifyOverrides { dependency_name } ->
      "builtin dependency '" ^ dependency_name ^ "' does not support path or source overrides"
  | BuiltinDependencyVersionRequirementNotSupported { dependency_name; requirement } ->
      "builtin dependency '"
      ^ dependency_name
      ^ "' does not support version requirement '"
      ^ requirement
      ^ "'"
  | DependencyMustBeStringOrTable { dependency_name } ->
      "dependency '" ^ dependency_name ^ "' must be a string or table"

let manifest_error_message = fun __tmp1 ->
  match __tmp1 with
  | ManifestMustBeTable -> "package manifest must be a table"
  | InvalidPackageName { raw_name = _; error } -> Package_name.error_message error
  | InvalidPublishMetadata error -> publish_metadata_error_message error
  | DependencySectionMustBeTable { section_name } -> "[" ^ section_name ^ "] must be a table"
  | InvalidDependency error -> dependency_error_message error

(** Package TOML parsing *)
let parse_name: (string * Toml.value) list -> string -> (Package_name.t, manifest_error) result = fun
  items fallback ->
  let raw_name =
    match Fields.get "package" items with
    | Some (Toml.Table pkg_items) -> (
        match Fields.get "name" pkg_items with
        | Some (Toml.String n) -> n
        | _ -> fallback
      )
    | _ -> fallback
  in
  Package_name.from_string raw_name
  |> Result.map_err ~fn:(fun error -> InvalidPackageName { raw_name; error })

let parse_publish_metadata:
  (string * Toml.value) list ->
  (publish_metadata, publish_metadata_error) result = fun items ->
  let parse_version = fun ~package_name ->
    fun __tmp1 ->
      match __tmp1 with
      | Toml.String raw_version -> (
          match Version.parse (String.trim raw_version) with
          | Ok version -> Ok (Some version)
          | Error error ->
              Error (InvalidPackageVersion { package_name; version = raw_version; error })
        )
      | _ -> Error (NonStringPublishField { package_name; field = PublishVersion })
  in
  let parse_optional_string = fun ~package_name ~field ->
    fun __tmp1 ->
      match __tmp1 with
      | Toml.String value -> Ok (Some value)
      | _ -> Error (NonStringPublishField { package_name; field })
  in
  let parse_public = fun ~package_name ->
    fun __tmp1 ->
      match __tmp1 with
      | Toml.Bool value -> Ok (Some value)
      | _ -> Error (NonBooleanPublicFlag { package_name })
  in
  match Fields.get "package" items with
  | Some (Toml.Table pkg_items) ->
      let package_name =
        match Fields.get "name" pkg_items with
        | Some (Toml.String name) -> name
        | _ -> "<package>"
      in
      let version =
        match Fields.get "version" pkg_items with
        | Some value -> parse_version ~package_name value
        | None -> Ok None
      in
      let description =
        match Fields.get "description" pkg_items with
        | Some value -> parse_optional_string ~package_name ~field:PublishDescription value
        | None -> Ok None
      in
      let license =
        match Fields.get "license" pkg_items with
        | Some value -> parse_optional_string ~package_name ~field:PublishLicense value
        | None -> Ok None
      in
      let is_public =
        match Fields.get "public" pkg_items with
        | Some value -> parse_public ~package_name value
        | None -> Ok None
      in
      (
        match (version, description, license, is_public) with
        | (Ok version, Ok description, Ok license, Ok is_public) ->
            Ok {
              version;
              description;
              license;
              is_public;
            }
        | (Error err, _, _, _)
        | (_, Error err, _, _)
        | (_, _, Error err, _)
        | (_, _, _, Error err) -> Error err
      )
  | Some _ -> Error PackageSectionMustBeTable
  | None -> Ok default_publish_metadata

let resolve_workspace_dependency: Package_name.t -> dependency list -> dependency = fun
  name workspace_deps ->
  match List.find workspace_deps ~fn:(fun (d: dependency) -> Package_name.equal d.name name) with
  | Some dep -> dep
  | None ->
      panic
        ("Dependency '"
        ^ Package_name.to_string name
        ^ "' with { workspace = true } not found in workspace dependencies")

let validate_requirement = fun ~dependency_name requirement ->
  let trimmed = String.trim requirement in
  match Version.parse_requirement trimmed with
  | Ok requirement -> Ok requirement
  | Error error -> Error (InvalidDependencyRequirement { dependency_name; requirement; error })

let requirement_is_any = fun requirement ->
  String.equal
    (Version.requirement_to_string requirement)
    "*"

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

let make_source = fun
  ?(workspace = false) ?(builtin = false) ?path ?source_locator ?ref_ ?version () ->
  {
    workspace;
    builtin;
    path;
    source_locator;
    ref_;
    version;
  }

let validate_dependency_source = fun ~dependency_name source ->
  if
    source.workspace
    && (source.builtin
    || Option.is_some source.path
    || Option.is_some source.source_locator
    || Option.is_some source.ref_
    || Option.is_some source.version)
  then
    Error (WorkspaceDependencyCannotSpecifyOverrides { dependency_name })
  else if Option.is_some source.ref_ && Option.is_none source.source_locator then
    Error (DependencyRefRequiresSource { dependency_name })
  else if
    source.builtin
    && (Option.is_some source.path
    || Option.is_some source.source_locator
    || Option.is_some source.ref_)
  then
    Error (BuiltinDependencyCannotSpecifyOverrides { dependency_name })
  else if source.builtin then
    match source.version with
    | None -> Ok { source with version = Some Version.any }
    | Some version when requirement_is_any version -> Ok source
    | Some version ->
        Error (BuiltinDependencyVersionRequirementNotSupported {
          dependency_name;
          requirement = Version.requirement_to_string version;
        })
  else if
    source.workspace
    || Option.is_some source.path
    || Option.is_some source.source_locator
    || Option.is_some source.version
  then
    Ok source
  else
    Ok { source with version = Some Version.any }

let parse_dependency:
  string ->
  Toml.value ->
  workspace_deps:dependency list ->
  (dependency, dependency_error) result = fun raw_name value ~workspace_deps ->
  let* name =
    Package_name.from_string raw_name
    |> Result.map_err ~fn:(fun error -> InvalidDependencyName { raw_name; error })
  in
  let dependency_name = Package_name.to_string name in
  match value with
  | Toml.Table attrs -> (
      match Fields.get "workspace" attrs with
      | Some (Toml.Bool true) -> (
          let source = {
            (resolve_workspace_dependency name workspace_deps).source with
            workspace = true;
          }
          in
          validate_dependency_source ~dependency_name source
          |> Result.map ~fn:(fun source -> { name; source })
        )
      | Some _ -> Error (NonBooleanWorkspaceFlag { dependency_name })
      | _ -> (
          let path =
            match Fields.get "path" attrs with
            | Some (Toml.String path_str) -> Ok (Some (Path.v path_str))
            | Some _ -> Error (NonStringDependencyField { dependency_name; field = DependencyPath })
            | None -> Ok None
          in
          let source_locator =
            match (Fields.get "source" attrs, Fields.get "github" attrs) with
            | (Some _, Some _) -> Error (DependencyCannotSpecifySourceAndGithub { dependency_name })
            | (Some (Toml.String locator), None) -> Ok (Some (normalize_source_locator locator))
            | (Some _, None) ->
                Error (NonStringDependencyField { dependency_name; field = DependencySource })
            | (None, Some (Toml.String github)) -> Ok (Some (github_locator_of_value github))
            | (None, Some _) ->
                Error (NonStringDependencyField { dependency_name; field = DependencyGithub })
            | (None, None) -> Ok None
          in
          let ref_ =
            match Fields.get "ref" attrs with
            | Some (Toml.String ref_) -> Ok (Some (String.trim ref_))
            | Some _ -> Error (NonStringDependencyField { dependency_name; field = DependencyRef })
            | None -> Ok None
          in
          let version =
            match Fields.get "version" attrs with
            | Some (Toml.String requirement) ->
                validate_requirement ~dependency_name requirement
                |> Result.map ~fn:(fun version -> Some version)
            | Some _ ->
                Error (NonStringDependencyField { dependency_name; field = DependencyVersion })
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
                (make_source
                  ~builtin:(is_builtin_dependency_name dependency_name)
                  ?path
                  ?source_locator
                  ?ref_
                  ?version
                  ())
              |> Result.map ~fn:(fun source -> { name; source })
        )
    )
  | Toml.String requirement -> (
      match validate_requirement ~dependency_name requirement with
      | Error _ as err -> err
      | Ok version ->
          validate_dependency_source
            ~dependency_name
            (make_source ~builtin:(is_builtin_dependency_name dependency_name) ~version ())
          |> Result.map ~fn:(fun source -> { name; source })
    )
  | _ -> Error (DependencyMustBeStringOrTable { dependency_name })

let parse_dependencies:
  (string * Toml.value) list ->
  workspace_deps:dependency list ->
  (dependency list, dependency_error) result = fun items ~workspace_deps ->
  let rec loop acc entries =
    match entries with
    | [] -> Ok (List.reverse acc)
    | (name, value) :: rest -> (
        match parse_dependency name value ~workspace_deps with
        | Ok dep -> loop (dep :: acc) rest
        | Error _ as err -> err
      )
  in
  loop [] items

let parse_dependency_section = fun section_name items ~(workspace_deps:dependency list) ->
  match Fields.get section_name items with
  | Some (Toml.Table dep_items) ->
      parse_dependencies dep_items ~workspace_deps
      |> Result.map_err ~fn:(fun error -> InvalidDependency error)
  | Some _ -> Error (DependencySectionMustBeTable { section_name })
  | None -> Ok []

let dependency_source_to_json = fun source ->
  let fields = [] in
  let fields =
    if source.workspace then
      ("workspace", Json.Bool true) :: fields
    else
      fields
  in
  let fields =
    if source.builtin then
      ("builtin", Json.Bool true) :: fields
    else
      fields
  in
  let fields =
    match source.path with
    | Some path -> ("path", Json.String (Path.to_string path)) :: fields
    | None -> fields
  in
  let fields =
    match source.source_locator with
    | Some source_locator -> ("source", Json.String source_locator) :: fields
    | None -> fields
  in
  let fields =
    match source.ref_ with
    | Some ref_ -> ("ref", Json.String ref_) :: fields
    | None -> fields
  in
  let fields =
    match source.version with
    | Some version -> ("version", Json.String (Version.requirement_to_string version)) :: fields
    | None -> fields
  in
  Json.Object (List.reverse fields)

let dependency_source_of_json = fun json ->
  match json with
  | Json.String "workspace" ->
      Ok {
        workspace = true;
        builtin = false;
        path = None;
        source_locator = None;
        ref_ = None;
        version = None;
      }
  | Json.String source_path ->
      Ok {
        workspace = false;
        builtin = false;
        path = Some (Path.v source_path);
        source_locator = None;
        ref_ = None;
        version = None;
      }
  | Json.Object fields -> (
      match Fields.get "kind" fields with
      | Some (Json.String "workspace") ->
          Ok {
            workspace = true;
            builtin = false;
            path = None;
            source_locator = None;
            ref_ = None;
            version = None;
          }
      | Some (Json.String "builtin") ->
          Ok {
            workspace = false;
            builtin = true;
            path = None;
            source_locator = None;
            ref_ = None;
            version = Some Version.any;
          }
      | Some (Json.String "path") -> (
          let path =
            match Fields.get "path" fields with
            | Some (Json.String path) -> Ok (Some (Path.v path))
            | _ -> Error "path dependency source is missing a string path"
          in
          let source_locator =
            match Fields.get "source" fields with
            | Some (Json.String locator) -> Ok (Some (normalize_source_locator locator))
            | Some Json.Null
            | None -> Ok None
            | Some _ -> Error "path dependency source has non-string source locator"
          in
          let ref_ =
            match Fields.get "ref" fields with
            | Some (Json.String ref_) -> Ok (Some ref_)
            | Some Json.Null
            | None -> Ok None
            | Some _ -> Error "path dependency source has non-string ref"
          in
          let version =
            match Fields.get "version" fields with
            | Some Json.Null
            | None -> Ok None
            | Some (Json.String requirement) ->
                validate_requirement ~dependency_name:"<json>" requirement
                |> Result.map_err ~fn:dependency_error_message
                |> Result.map ~fn:(fun version -> Some version)
            | _ -> Error "path dependency source has non-string version requirement"
          in
          match (path, source_locator, ref_, version) with
          | (Ok path, Ok source_locator, Ok ref_, Ok version) ->
              validate_dependency_source
                ~dependency_name:"<json>"
                {
                  workspace = false;
                  builtin = false;
                  path;
                  source_locator;
                  ref_;
                  version;
                }
              |> Result.map_err ~fn:dependency_error_message
          | (Error err, _, _, _)
          | (_, Error err, _, _)
          | (_, _, Error err, _)
          | (_, _, _, Error err) -> Error err
        )
      | Some (Json.String "registry") -> (
          match Fields.get "version" fields with
          | Some Json.Null
          | None ->
              Ok {
                workspace = false;
                builtin = false;
                path = None;
                source_locator = None;
                ref_ = None;
                version = Some Version.any;
              }
          | Some (Json.String requirement) ->
              validate_requirement ~dependency_name:"<json>" requirement
              |> Result.map_err ~fn:dependency_error_message
              |> Result.map
                ~fn:(fun version ->
                  {
                    workspace = false;
                    builtin = false;
                    path = None;
                    source_locator = None;
                    ref_ = None;
                    version = Some version;
                  })
          | _ -> Error "registry dependency source has non-string version requirement"
        )
      | Some (Json.String kind) -> Error ("unknown dependency source kind: " ^ kind)
      | _ ->
          let workspace =
            match Fields.get "workspace" fields with
            | Some (Json.Bool value) -> Ok value
            | Some _ -> Error "dependency source workspace flag must be boolean"
            | None -> Ok false
          in
          let builtin =
            match Fields.get "builtin" fields with
            | Some (Json.Bool value) -> Ok value
            | Some _ -> Error "dependency source builtin flag must be boolean"
            | None -> Ok false
          in
          let path =
            match Fields.get "path" fields with
            | Some (Json.String path) -> Ok (Some (Path.v path))
            | Some Json.Null -> Ok None
            | Some _ -> Error "dependency source path must be a string"
            | None -> Ok None
          in
          let source_locator =
            match Fields.get "source" fields with
            | Some (Json.String locator) -> Ok (Some (normalize_source_locator locator))
            | Some Json.Null -> Ok None
            | Some _ -> Error "dependency source source must be a string"
            | None -> Ok None
          in
          let ref_ =
            match Fields.get "ref" fields with
            | Some (Json.String ref_) -> Ok (Some ref_)
            | Some Json.Null -> Ok None
            | Some _ -> Error "dependency source ref must be a string"
            | None -> Ok None
          in
          let version =
            match Fields.get "version" fields with
            | Some (Json.String requirement) ->
                validate_requirement ~dependency_name:"<json>" requirement
                |> Result.map_err ~fn:dependency_error_message
                |> Result.map ~fn:(fun version -> Some version)
            | Some Json.Null -> Ok None
            | Some _ -> Error "dependency source version must be a string"
            | None -> Ok None
          in
          match (workspace, builtin, path, source_locator, ref_, version) with
          | (Ok workspace, Ok builtin, Ok path, Ok source_locator, Ok ref_, Ok version) ->
              validate_dependency_source
                ~dependency_name:"<json>"
                {
                  workspace;
                  builtin;
                  path;
                  source_locator;
                  ref_;
                  version;
                }
              |> Result.map_err ~fn:dependency_error_message
          | (Error err, _, _, _, _, _)
          | (_, Error err, _, _, _, _)
          | (_, _, Error err, _, _, _)
          | (_, _, _, Error err, _, _)
          | (_, _, _, _, Error err, _)
          | (_, _, _, _, _, Error err) -> Error err
    )
  | _ -> Error "dependency source must be a string or object"

let parse_foreign_dependency:
  string ->
  Toml.value ->
  package_path:Path.t ->
  (foreign_dependency, string) result = fun name value ~package_path ->
  match value with
  | Toml.Table attrs -> (
      let get_string key =
        match Fields.get key attrs with
        | Some (Toml.String s) -> Ok s
        | Some _ -> Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be a string")
        | None -> Error ("Foreign dependency '" ^ name ^ "': missing required field '" ^ key ^ "'")
      in
      let get_string_list key =
        match Fields.get key attrs with
        | Some (Toml.Array arr) ->
            let strings =
              List.filter_map
                ~fn:(fun __tmp1 ->
                  match __tmp1 with
                  | Toml.String s -> Some s
                  | _ -> None)
                arr
            in
            if List.length strings = List.length arr then
              Ok strings
            else
              Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be an array of strings")
        | Some _ -> Error ("Foreign dependency '" ^ name ^ "': '" ^ key ^ "' must be an array")
        | None -> Error ("Foreign dependency '" ^ name ^ "': missing required field '" ^ key ^ "'")
      in
      let get_string_list_opt key =
        match Fields.get key attrs with
        | Some (Toml.Array arr) ->
            let strings =
              List.filter_map
                ~fn:(fun __tmp1 ->
                  match __tmp1 with
                  | Toml.String s -> Some s
                  | _ -> None)
                arr
            in
            if List.length strings = List.length arr then
              Some strings
            else
              None
        | _ -> None
      in
      let get_env () =
        match Fields.get "env" attrs with
        | Some (Toml.Table env_items) ->
            List.filter_map
              ~fn:(fun (k, v) ->
                match v with
                | Toml.String s -> Some (k, s)
                | _ -> None)
              env_items
        | _ -> []
      in
      match (get_string "path", get_string_list "build_cmd", get_string_list "outputs") with
      | (Ok path_str, Ok build_cmd, Ok outputs) ->
          let dep_path = Path.(package_path / v path_str) in
          let output_paths = List.map outputs ~fn:Path.v in
          let clean_cmd = get_string_list_opt "clean_cmd" in
          let test_cmd = get_string_list_opt "test_cmd" in
          let env = get_env () in
          (* Scan for foreign dependency source files *)
          let scan_foreign_inputs foreign_path =
            let walker =
              match Fs.Walker.create ~roots:[ foreign_path ] ~sort:true ~follow_symlinks:true () with
              | Ok walker -> walker
              | Error _ -> panic "foreign dependency walker configuration should be valid"
            in
            let walker =
              Fs.Walker.filter_entry
                walker
                ~f:(fun (entry: Fs.Walker.FileItem.t) ->
                  let path = Fs.Walker.FileItem.path entry in
                  if Int.equal (Fs.Walker.FileItem.depth entry) 0 then
                    true
                  else
                    match Path.strip_prefix path ~prefix:foreign_path with
                    | Error _ -> false
                    | Ok rel_path ->
                        let entry_name = Path.basename rel_path in
                        let exclude_dirs = [ "target"; "_build"; "build"; "dist"; "node_modules"; ]
                        in
                        let should_skip =
                          String.starts_with ~prefix:"." entry_name
                          || List.contains exclude_dirs ~value:entry_name
                        in
                        if should_skip then
                          false
                        else
                          match Fs.Walker.FileItem.kind entry with
                          | Directory -> true
                          | File ->
                              String.ends_with ~suffix:".rs" entry_name
                              || String.ends_with ~suffix:".c" entry_name
                              || String.ends_with ~suffix:".h" entry_name
                              || String.ends_with ~suffix:".cpp" entry_name
                              || String.ends_with ~suffix:".hpp" entry_name
                              || entry_name = "Cargo.toml"
                              || entry_name = "Cargo.lock"
                              || entry_name = "build.rs"
                              || entry_name = "CMakeLists.txt"
                              || entry_name = "Makefile"
                          | Symlink
                          | Other -> false)
            in
            let iter = Fs.Walker.into_iter walker in
            let rec loop acc iter =
              match Iter.Iterator.next iter with
              | (None, _) -> List.reverse acc
              | (Some (Error _), iter') -> loop acc iter'
              | (Some (Ok (entry: Fs.Walker.FileItem.t)), iter') -> (
                  let path = Fs.Walker.FileItem.path entry in
                  match Fs.Walker.FileItem.kind entry with
                  | File -> (
                      match Path.strip_prefix path ~prefix:foreign_path with
                      | Ok rel_path -> loop (rel_path :: acc) iter'
                      | Error _ -> loop acc iter'
                    )
                  | Directory
                  | Symlink
                  | Other -> loop acc iter'
                )
            in
            loop [] iter
          in
          let inputs = scan_foreign_inputs dep_path in
          Log.debug
            ("[PACKAGE] Foreign dependency '"
            ^ name
            ^ "' found "
            ^ Int.to_string (List.length inputs)
            ^ " input files");
          Ok {
            name;
            path = dep_path;
            inputs;
            build_cmd;
            clean_cmd;
            test_cmd;
            outputs = output_paths;
            env;
          }
      | (Error e, _, _) -> Error e
      | (_, Error e, _) -> Error e
      | (_, _, Error e) -> Error e
    )
  | _ -> Error ("Foreign dependency '" ^ name ^ "' must be a table")

let parse_foreign_dependencies:
  (string * Toml.value) list ->
  package_path:Path.t ->
  (foreign_dependency list, string) result = fun items ~package_path ->
  Log.debug "[PACKAGE] parse_foreign_dependencies: checking for 'foreign-dependencies' key";
  Log.debug
    ("[PACKAGE] Available keys: " ^ String.concat ", " (List.map items ~fn:(fun (key, _) -> key)));
  (* Collect all keys that start with "foreign-dependencies." *)
  let foreign_dep_items =
    List.filter_map
      ~fn:(fun (key, value) ->
        if String.starts_with ~prefix:"foreign-dependencies." key then
          let prefix_len = String.length "foreign-dependencies." in
          let dep_name = String.sub key ~offset:prefix_len ~len:(String.length key - prefix_len) in
          Some (dep_name, value)
        else
          None)
      items
  in
  if not (List.is_empty foreign_dep_items) then
    Log.debug
      ("[PACKAGE] Found "
      ^ Int.to_string (List.length foreign_dep_items)
      ^ " foreign dependencies via dotted keys");
  let nested_deps =
    match Fields.get "foreign-dependencies" items with
    | Some (Toml.Table deps) ->
        Log.debug
          ("[PACKAGE] Found foreign-dependencies table with "
          ^ Int.to_string (List.length deps)
          ^ " entries");
        deps
    | Some _ ->
        Log.warn "[PACKAGE] foreign-dependencies exists but is not a table";
        []
    | None ->
        Log.debug "[PACKAGE] No 'foreign-dependencies' table found";
        []
  in
  (* Combine both sources *)
  let all_deps = foreign_dep_items @ nested_deps in
  if all_deps = [] then
    Ok []
  else
    let results =
      List.map all_deps ~fn:(fun (name, value) -> parse_foreign_dependency name value ~package_path)
    in
    let errors =
      List.filter_map
        ~fn:(fun r ->
          match r with
          | Error e -> Some e
          | Ok _ -> None)
        results
    in
    if errors != [] then
      Error (String.concat "; " errors)
    else
      Ok (
        List.filter_map
          ~fn:(fun r ->
            match r with
            | Ok d -> Some d
            | Error _ -> None)
          results
      )

let parse_binary: Toml.value -> package_path:Path.t -> (binary, string) result = fun
  value ~package_path ->
  match value with
  | Toml.Table items -> (
      match (Fields.get "name" items, Fields.get "path" items) with
      | (Some (Toml.String name), Some (Toml.String path_str)) ->
          let bin_path = Path.v path_str in
          Ok { name; path = bin_path }
      | (Some (Toml.String _), None) -> Error "Binary entry missing required 'path' field"
      | (None, Some (Toml.String _)) -> Error "Binary entry missing required 'name' field"
      | (Some (Toml.String _), Some _) -> Error "Binary 'path' field must be a string"
      | (Some _, Some _) -> Error "Binary 'name' field must be a string"
      | (Some _, None) -> Error "Binary 'name' field must be a string"
      | (None, Some _) -> Error "Binary 'path' field must be a string"
      | (None, None) -> Error "Binary entry missing required 'name' and 'path' fields"
    )
  | _ -> Error "Binary entry must be a table"

let parse_binaries:
  (string * Toml.value) list ->
  package_path:Path.t ->
  (binary list, string) result = fun items ~package_path ->
  match Fields.get "bin" items with
  | None -> Ok []
  | Some (Toml.Array bin_entries) ->
      let results = List.map bin_entries ~fn:(parse_binary ~package_path) in
      let errors =
        List.filter_map
          ~fn:(fun r ->
            match r with
            | Error e -> Some e
            | Ok _ -> None)
          results
      in
      if errors != [] then
        Error (String.concat "; " errors)
      else
        Ok (
          List.filter_map
            ~fn:(fun r ->
              match r with
              | Ok b -> Some b
              | Error _ -> None)
            results
        )
  | Some _ -> Error "[[bin]] must be an array of tables"

let parse_library:
  (string * Toml.value) list ->
  package_path:Path.t ->
  package_name:Package_name.t ->
  (library option, string) result = fun items ~package_path ~package_name ->
  let package_name = Package_name.to_string package_name in
  match Fields.get "lib" items with
  | None ->
      (* Autodiscover: if src/<package_name>.ml exists, use it as library *)
      let default_lib_path = Path.(package_path / Path.v "src" / Path.v (package_name ^ ".ml")) in
      (
        match Fs.exists default_lib_path with
        | Ok true -> Ok (Some { path = default_lib_path })
        | Ok false
        | Error _ -> Ok None
      )
  | Some (Toml.Table lib_items) -> (
      match Fields.get "path" lib_items with
      | Some (Toml.String path_str) ->
          let lib_path = Path.(package_path / Path.v path_str) in
          Ok (Some { path = lib_path })
      | None ->
          let default_path = Path.(package_path / Path.v "src" / Path.v (package_name ^ ".ml")) in
          Ok (Some { path = default_path })
      | Some _ -> Error "Library 'path' field must be a string"
    )
  | Some _ -> Error "[lib] must be a table"

let parse_compiler_config: (string * Toml.value) list -> compiler_config = fun items ->
  let profile_overrides =
    match Fields.get "profile" items with
    | Some (Toml.Table profile_table) ->
        List.filter_map
          ~fn:(fun (profile_name, value) ->
            match value with
            | Toml.Table profile_items ->
                Some (profile_name, Profile.override_from_toml profile_items)
            | _ -> None)
          profile_table
    | _ -> []
  in
  (* Parse [target.macos], [target.linux], etc. sections *)
  let target_overrides =
    match Fields.get "target" items with
    | Some (Toml.Table target_table) ->
        List.filter_map
          ~fn:(fun (platform, value) ->
            match value with
            | Toml.Table platform_items ->
                let profile_override = Profile.override_from_toml platform_items in
                Some (platform, { profile_override = Some profile_override })
            | _ -> None)
          target_table
    | _ -> []
  in
  { profile_overrides; target_overrides }

let provider_excluded_relpaths = fun ~(package_path:Path.t) providers ->
  let collect_relative_files ~root ~keep_file ~skip_dir =
    let walker =
      match Fs.Walker.create ~roots:[ root ] ~sort:true ~follow_symlinks:true () with
      | Ok walker -> walker
      | Error _ -> panic "package walker configuration should be valid"
    in
    let walker =
      Fs.Walker.filter_entry
        walker
        ~f:(fun (entry: Fs.Walker.FileItem.t) ->
          let path = Fs.Walker.FileItem.path entry in
          if Int.equal (Fs.Walker.FileItem.depth entry) 0 then
            true
          else
            match Path.strip_prefix path ~prefix:package_path with
            | Error _ -> false
            | Ok rel_path -> (
                match Fs.Walker.FileItem.kind entry with
                | Directory -> not (skip_dir rel_path)
                | File -> keep_file rel_path
                | Symlink
                | Other -> false
              ))
    in
    let iter = Fs.Walker.into_iter walker in
    let rec loop acc iter =
      match Iter.Iterator.next iter with
      | (None, _) -> List.reverse acc
      | (Some (Error _), iter') -> loop acc iter'
      | (Some (Ok (entry: Fs.Walker.FileItem.t)), iter') -> (
          let path = Fs.Walker.FileItem.path entry in
          match Fs.Walker.FileItem.kind entry with
          | File -> (
              match Path.strip_prefix path ~prefix:package_path with
              | Ok rel_path -> loop (rel_path :: acc) iter'
              | Error _ -> loop acc iter'
            )
          | Directory
          | Symlink
          | Other -> loop acc iter'
        )
    in
    loop [] iter
  in
  let ocaml_source_suffix path_str =
    String.ends_with ~suffix:".ml" path_str || String.ends_with ~suffix:".mli" path_str
  in
  let collect_provider_tree rel_path =
    let provider_parent = Path.dirname rel_path in
    let parent_basename = Path.basename provider_parent in
    let basename = Path.basename rel_path in
    if
      String.equal basename "riot_fix_rules.ml" && String.equal parent_basename "riot_fix_rules"
    then
      let provider_dir = Path.(package_path / provider_parent) in
      collect_relative_files
        ~root:provider_dir
        ~skip_dir:(fun _ -> false)
        ~keep_file:(fun rel_path -> ocaml_source_suffix (Path.to_string rel_path))
    else
      [ rel_path ]
  in
  providers
  |> List.filter_map
    ~fn:(fun (provider: Fix_provider.t) ->
      match Path.strip_prefix provider.source_path ~prefix:package_path with
      | Ok rel_path -> Some (collect_provider_tree rel_path)
      | Error _ -> None)
  |> List.concat
  |> List.unique
    ~compare:(fun left right -> String.compare (Path.to_string left) (Path.to_string right))

type source_bucket =
  | Src
  | Native
  | Tests
  | Examples
  | Bench

let source_buckets_for_intent = fun __tmp1 ->
  match __tmp1 with
  | Build -> []
  | Runtime -> [ Src; Native ]
  | Dev -> [ Src; Native; Tests; Examples; Bench; ]
  | Run -> [ Src; Native; Examples ]
  | Test -> [ Src; Native; Tests ]
  | Bench -> [ Src; Native; Bench ]
  | Doc -> [ Src; Examples ]
  | Check -> [ Src; Native; Tests; Examples; Bench; ]

let source_bucket_enabled buckets bucket = List.contains buckets ~value:bucket

let elapsed_us_since = fun started_at ->
  Time.Instant.elapsed started_at
  |> Time.Duration.to_micros

let model_trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_MODEL_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_package = fun message ->
  if model_trace_enabled () then
    eprintln ("riot-model package " ^ message)

let string_of_realization_intent = fun __tmp1 ->
  match __tmp1 with
  | Build -> "build"
  | Runtime -> "runtime"
  | Dev -> "dev"
  | Run -> "run"
  | Test -> "test"
  | Bench -> "bench"
  | Doc -> "doc"
  | Check -> "check"

let bucket_of_rel_path path_components =
  match path_components with
  | "src" :: _ -> Some Src
  | "tests" :: _ -> Some Tests
  | "native" :: _ -> Some Native
  | "examples" :: _ -> Some Examples
  | "bench" :: _ -> Some Bench
  | _ -> None

let scan_roots_for_intent = fun ~(intent:realization_intent) ~(package_path:Path.t) ->
  let roots =
    match intent with
    | Runtime -> [ Path.(package_path / Path.v "src"); Path.(package_path / Path.v "native") ]
    | _ -> [ package_path ]
  in
  List.filter roots ~fn:Path.exists

let scan_sources_for_intent
  ~(intent:realization_intent)
  ~(package_path:Path.t)
  ?(excluded_relpaths = [])
  () =
  let started_at = Time.Instant.now () in
  let excluded_relpath_strings =
    excluded_relpaths
    |> List.map ~fn:Path.to_string
  in
  let path_components path =
    Path.components path
    |> List.map ~fn:Path.to_string
  in
  let enabled_buckets = source_buckets_for_intent intent in
  if List.is_empty enabled_buckets then
    empty_sources
  else
    let scan_roots = scan_roots_for_intent ~intent ~package_path in
    let should_skip_source_entry filename =
      String.starts_with ~prefix:"." (Path.basename filename)
    in
    let should_skip_test_support_path rel_path =
      match path_components rel_path with
      | "tests" :: (
        "fixtures"
        | "generated"
        | "diagnostics"
        | "deps_fixtures"
        | "autofix_fixtures"
        | "workspace_fixtures"
      ) :: _ -> true
      | _ -> false
    in
    let is_ocaml_module_file rel_path =
      match Path.extension rel_path with
      | Some ".ml"
      | Some ".mli" -> true
      | _ -> false
    in
    if not (Path.exists package_path) || List.is_empty scan_roots then
      empty_sources
    else
      let src = ref [] in
      let tests = ref [] in
      let native = ref [] in
      let examples = ref [] in
      let bench = ref [] in
      let visited_entries = ref 0 in
      let visited_directories = ref 0 in
      let visited_files = ref 0 in
      let walker =
        match Ignore.Walker.create ~roots:scan_roots () with
        | Ok walker -> walker
        | Error _ -> panic "package walker configuration should be valid"
      in
      match Ignore.Walker.to_list walker with
      | Ok entries ->
          List.for_each
            entries
            ~fn:(fun (entry: Fs.Walker.FileItem.t) ->
              let () =
                visited_entries := !visited_entries + 1
              in
              let path = Fs.Walker.FileItem.path entry in
              if not (Int.equal (Fs.Walker.FileItem.depth entry) 0) then
                match Path.strip_prefix path ~prefix:package_path with
                | Error _ -> ()
                | Ok rel_path -> (
                    let rel_path_string = Path.to_string rel_path in
                    let rel_path_components = path_components rel_path in
                    match Fs.Walker.FileItem.kind entry with
                    | Directory ->
                        let () =
                          visited_directories := !visited_directories + 1
                        in
                        ()
                    | File ->
                        let () =
                          visited_files := !visited_files + 1
                        in
                        if
                          not
                            (should_skip_source_entry rel_path
                            || should_skip_test_support_path rel_path
                            || List.contains excluded_relpath_strings ~value:rel_path_string)
                        then (
                          match bucket_of_rel_path rel_path_components with
                          | Some bucket when not (source_bucket_enabled enabled_buckets bucket) ->
                              ()
                          | Some Src
                          | Some Tests
                          | Some Examples
                          | Some Bench when not (is_ocaml_module_file rel_path) -> ()
                          | Some Src -> src := rel_path :: !src
                          | Some Tests -> tests := rel_path :: !tests
                          | Some Native -> native := rel_path :: !native
                          | Some Examples -> examples := rel_path :: !examples
                          | Some Bench -> bench := rel_path :: !bench
                          | None -> ()
                        )
                    | Symlink
                    | Other -> ()
                  ));
          let sources = {
            src = List.reverse !src;
            tests = List.reverse !tests;
            native = List.reverse !native;
            examples = List.reverse !examples;
            bench = List.reverse !bench;
          }
          in
          let () =
            trace_package
              ("scan-sources path="
              ^ Path.to_string package_path
              ^ " intent="
              ^ string_of_realization_intent intent
              ^ " total_us="
              ^ Int.to_string (elapsed_us_since started_at)
              ^ " visited_entries="
              ^ Int.to_string !visited_entries
              ^ " visited_directories="
              ^ Int.to_string !visited_directories
              ^ " visited_files="
              ^ Int.to_string !visited_files
              ^ " kept_src="
              ^ Int.to_string (List.length sources.src)
              ^ " kept_tests="
              ^ Int.to_string (List.length sources.tests)
              ^ " kept_native="
              ^ Int.to_string (List.length sources.native)
              ^ " kept_examples="
              ^ Int.to_string (List.length sources.examples)
              ^ " kept_bench="
              ^ Int.to_string (List.length sources.bench))
          in
          sources
      | Error _ ->
          let () =
            trace_package
              ("scan-sources-failed path="
              ^ Path.to_string package_path
              ^ " intent="
              ^ string_of_realization_intent intent
              ^ " total_us="
              ^ Int.to_string (elapsed_us_since started_at))
          in
          empty_sources

let scan_sources ~(package_path:Path.t) ?(excluded_relpaths = []) () =
  scan_sources_for_intent ~intent:Dev ~package_path ~excluded_relpaths ()

(** Autodiscover test binaries from test files ending in _tests.ml or -tests.ml *)
let autodiscover_test_binaries: sources -> package_path:Path.t -> binary list = fun
  sources ~package_path ->
  List.filter_map
    ~fn:(fun test_file ->
      let filename = Path.basename test_file in
      if
        String.ends_with ~suffix:"_tests.ml" filename
        || String.ends_with ~suffix:"-tests.ml" filename
      then
        let binary_name =
          Path.remove_extension (Path.v filename)
          |> Path.to_string
        in
        (* test_file is already relative to package (e.g., tests/foo_tests.ml) *)
        let binary_path = test_file in
        Some { name = binary_name; path = binary_path }
      else
        None)
    sources.tests

(** Autodiscover a default runtime binary from src/main.ml when no explicit [[bin]] exists. *)
let autodiscover_main_binary: sources -> package_name:Package_name.t -> binary list = fun
  sources ~package_name ->
  if List.any sources.src ~fn:(fun path -> Path.equal path (Path.v "src/main.ml")) then
    [ { name = Package_name.to_string package_name; path = Path.v "src/main.ml" } ]
  else
    []

(** Autodiscover example binaries from any .ml file in examples/ directory *)
let autodiscover_example_binaries: sources -> package_path:Path.t -> binary list = fun
  sources ~package_path ->
  List.filter_map
    ~fn:(fun example_file ->
      let filename = Path.basename example_file in
      if String.ends_with ~suffix:".ml" filename then
        let binary_name =
          Path.remove_extension (Path.v filename)
          |> Path.to_string
        in
        (* example_file is already relative to package (e.g., examples/sqltool.ml) *)
        Some { name = binary_name; path = example_file }
      else
        None)
    sources.examples

(** Autodiscover benchmark binaries from bench files ending in _bench.ml *)
let autodiscover_bench_binaries: sources -> package_path:Path.t -> binary list = fun
  sources ~package_path ->
  List.filter_map
    ~fn:(fun bench_file ->
      let filename = Path.basename bench_file in
      if String.ends_with ~suffix:"_bench.ml" filename then
        let binary_name =
          Path.remove_extension (Path.v filename)
          |> Path.to_string
        in
        (* bench_file is already relative to package (e.g., bench/foo_bench.ml) *)
        Some { name = binary_name; path = bench_file }
      else
        None)
    sources.bench

let binary_bucket = fun (bin: binary) ->
  let path_str = Path.to_string bin.path in
  if String.starts_with ~prefix:"tests/" path_str then
    Some Tests
  else if String.starts_with ~prefix:"examples/" path_str then
    Some Examples
  else if String.starts_with ~prefix:"bench/" path_str then
    Some Bench
  else
    Some Src

let has_declared_binary_in_bucket = fun binaries bucket ->
  List.any
    binaries
    ~fn:(fun bin ->
      match binary_bucket bin with
      | Some bin_bucket -> bin_bucket = bucket
      | None -> false)

let declared_binaries_for_intent = fun ~(intent:realization_intent) binaries ->
  let keep bucket =
    match intent with
    | Build -> false
    | Runtime -> bucket = Src
    | Dev -> true
    | Run -> bucket = Src || bucket = Examples
    | Test -> bucket = Tests
    | Bench -> bucket = Bench
    | Doc -> bucket = Src
    | Check -> false
  in
  List.filter
    binaries
    ~fn:(fun bin ->
      match binary_bucket bin with
      | Some bucket -> keep bucket
      | None -> false)

let autodiscovered_binaries_for_intent = fun
  ~(intent:realization_intent)
  ~(sources:sources)
  ~package_name
  ~package_path
  ~declared_binaries ->
  let runtime_binaries =
    if has_declared_binary_in_bucket declared_binaries Src then
      []
    else
      autodiscover_main_binary sources ~package_name
  in
  let test_binaries =
    if has_declared_binary_in_bucket declared_binaries Tests then
      []
    else
      autodiscover_test_binaries sources ~package_path
  in
  let example_binaries =
    if has_declared_binary_in_bucket declared_binaries Examples then
      []
    else
      autodiscover_example_binaries sources ~package_path
  in
  let bench_binaries =
    if has_declared_binary_in_bucket declared_binaries Bench then
      []
    else
      autodiscover_bench_binaries sources ~package_path
  in
  match intent with
  | Build -> []
  | Runtime -> runtime_binaries
  | Dev -> ((runtime_binaries @ test_binaries) @ example_binaries) @ bench_binaries
  | Run -> runtime_binaries @ example_binaries
  | Test -> test_binaries
  | Bench -> bench_binaries
  | Doc -> runtime_binaries
  | Check -> []

let merge_binaries: declared:binary list -> autodiscovered:binary list -> binary list = fun
  ~declared ~autodiscovered ->
  let seen_paths =
    declared
    |> List.map ~fn:(fun (bin: binary) -> Path.to_string bin.path)
  in
  let (_, discovered) =
    List.fold_left
      autodiscovered
      ~init:(seen_paths, [])
      ~fn:(fun (seen_paths, acc) (bin: binary) ->
        let path = Path.to_string bin.path in
        if List.contains seen_paths ~value:path then
          (seen_paths, acc)
        else
          (path :: seen_paths, bin :: acc))
  in
  declared @ List.reverse discovered

let commands_for_intent = fun ~(intent:realization_intent) commands ->
  match intent with
  | Doc -> commands
  | Build
  | Runtime
  | Dev
  | Run
  | Test
  | Bench
  | Check -> commands

let relative_to_package = fun ~package_path path ->
  let relative =
    if Path.is_absolute path then
      match Path.strip_prefix path ~prefix:package_path with
      | Ok path -> path
      | Error _ -> path
    else
      path
  in
  Path.normalize relative

let executable_source_excluded_relpaths_for_intent = fun
  ~(intent:realization_intent) ~package_path ~declared_binaries ~commands ->
  match intent with
  | Doc ->
      let declared_binary_sources =
        declared_binaries
        |> List.map ~fn:(fun (binary: binary) -> relative_to_package ~package_path binary.path)
      in
      let command_sources =
        commands
        |> List.map
          ~fn:(fun (command: Package_command.t) ->
            relative_to_package
              ~package_path
              command.command_source)
      in
      let default_runtime_binary_sources =
        if has_declared_binary_in_bucket declared_binaries Src then
          []
        else
          [ Path.v "src/main.ml" ]
      in
      (declared_binary_sources @ command_sources) @ default_runtime_binary_sources
      |> List.unique ~compare:Path.compare
  | Build
  | Runtime
  | Dev
  | Run
  | Test
  | Bench
  | Check -> []

let parse_manifest_spec:
  Toml.value ->
  workspace_deps:dependency list ->
  workspace_dev_deps:dependency list ->
  workspace_build_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (manifest_spec, manifest_error) result = fun
  toml ~workspace_deps ~workspace_dev_deps ~workspace_build_deps ~path ~relative_path ->
  match toml with
  | Toml.Table items -> (
      let fallback_name = Path.basename path in
      let* name = parse_name items fallback_name in
      match parse_publish_metadata items with
      | Error error -> Error (InvalidPublishMetadata error)
      | Ok publish ->
          match parse_dependency_section "dependencies" items ~workspace_deps with
          | Error _ as err -> err
          | Ok dependencies ->
              match parse_dependency_section
                "dev-dependencies"
                items
                ~workspace_deps:workspace_dev_deps with
              | Error _ as err -> err
              | Ok dev_dependencies ->
                  match parse_dependency_section
                    "build-dependencies"
                    items
                    ~workspace_deps:workspace_build_deps with
                  | Error _ as err -> err
                  | Ok build_dependencies ->
                      let declared_binaries =
                        match parse_binaries items ~package_path:path with
                        | Ok bins -> bins
                        | Error msg ->
                            Log.warn
                              ("[PACKAGE] Failed to parse binaries for "
                              ^ Package_name.to_string name
                              ^ ": "
                              ^ msg);
                            []
                      in
                      let library =
                        match parse_library items ~package_path:path ~package_name:name with
                        | Ok lib -> lib
                        | Error msg ->
                            Log.warn
                              ("[PACKAGE] Failed to parse library for "
                              ^ Package_name.to_string name
                              ^ ": "
                              ^ msg);
                            None
                      in
                      let foreign_dependencies =
                        match parse_foreign_dependencies items ~package_path:path with
                        | Ok deps -> deps
                        | Error msg ->
                            Log.warn
                              ("[PACKAGE] Failed to parse foreign dependencies for "
                              ^ Package_name.to_string name
                              ^ ": "
                              ^ msg);
                            []
                      in
                      let fix_providers =
                        Fix_provider.parse_from_toml items ~package_name:name ~package_path:path
                      in
                      let compiler = parse_compiler_config items in
                      let commands =
                        match Fields.get "command" items with
                        | Some (Toml.Array cmd_entries) ->
                            Package_command.parse_from_toml
                              cmd_entries
                              ~package_name:name
                              ~package_path:path
                        | _ -> []
                      in
                      Ok (
                        canonicalize_manifest_spec
                          {
                            name;
                            path;
                            relative_path;
                            dependencies;
                            dev_dependencies;
                            build_dependencies;
                            foreign_dependencies;
                            declared_binaries;
                            library;
                            compiler;
                            commands;
                            fix_providers;
                            publish;
                          }
                      )
    )
  | _ -> Error ManifestMustBeTable

let realize_manifest_spec = fun ~(intent:realization_intent) (manifest: manifest_spec) ->
  let excluded_relpaths =
    provider_excluded_relpaths ~package_path:manifest.path manifest.fix_providers
    @ executable_source_excluded_relpaths_for_intent
      ~intent
      ~package_path:manifest.path
      ~declared_binaries:manifest.declared_binaries
      ~commands:manifest.commands
  in
  let sources = scan_sources_for_intent ~intent ~package_path:manifest.path ~excluded_relpaths () in
  let declared_binaries = declared_binaries_for_intent ~intent manifest.declared_binaries in
  let autodiscovered =
    autodiscovered_binaries_for_intent
      ~intent
      ~sources
      ~package_name:manifest.name
      ~package_path:manifest.path
      ~declared_binaries
  in
  let binaries = merge_binaries ~declared:declared_binaries ~autodiscovered in
  make
    ~name:manifest.name
    ~path:manifest.path
    ~relative_path:manifest.relative_path
    ~dependencies:manifest.dependencies
    ~dev_dependencies:manifest.dev_dependencies
    ~build_dependencies:manifest.build_dependencies
    ~foreign_dependencies:manifest.foreign_dependencies
    ~binaries
    ?library:manifest.library
    ~sources
    ~compiler:manifest.compiler
    ~commands:(commands_for_intent ~intent manifest.commands)
    ~fix_providers:manifest.fix_providers
    ~publish:manifest.publish
    ()

let from_manifest_spec = fun (manifest: manifest_spec) ->
  make
    ~name:manifest.name
    ~path:manifest.path
    ~relative_path:manifest.relative_path
    ~dependencies:manifest.dependencies
    ~dev_dependencies:manifest.dev_dependencies
    ~build_dependencies:manifest.build_dependencies
    ~foreign_dependencies:manifest.foreign_dependencies
    ~binaries:manifest.declared_binaries
    ?library:manifest.library
    ~compiler:manifest.compiler
    ~commands:manifest.commands
    ~fix_providers:manifest.fix_providers
    ~publish:manifest.publish
    ()

let from_toml:
  Toml.value ->
  workspace_deps:dependency list ->
  workspace_dev_deps:dependency list ->
  workspace_build_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (t, manifest_error) result = fun
  toml ~workspace_deps ~workspace_dev_deps ~workspace_build_deps ~path ~relative_path ->
  parse_manifest_spec
    toml
    ~workspace_deps
    ~workspace_dev_deps
    ~workspace_build_deps
    ~path
    ~relative_path
  |> Result.map ~fn:(realize_manifest_spec ~intent:Dev)

let to_json: t -> Json.t = fun pkg ->
  let dependencies_json = Json.Array (List.map
    pkg.dependencies
    ~fn:(fun (dep: dependency) ->
      Json.Object [
        ("name", Json.String (Package_name.to_string dep.name));
        ("source", dependency_source_to_json dep.source);
      ]))
  in
  let dev_dependencies_json = Json.Array (List.map
    pkg.dev_dependencies
    ~fn:(fun (dep: dependency) ->
      Json.Object [
        ("name", Json.String (Package_name.to_string dep.name));
        ("source", dependency_source_to_json dep.source);
      ]))
  in
  let build_dependencies_json = Json.Array (List.map
    pkg.build_dependencies
    ~fn:(fun (dep: dependency) ->
      Json.Object [
        ("name", Json.String (Package_name.to_string dep.name));
        ("source", dependency_source_to_json dep.source);
      ]))
  in
  let binaries_json = Json.Array (List.map
    pkg.binaries
    ~fn:(fun (bin: binary) ->
      Json.Object [
        ("name", Json.String bin.name);
        ("path", Json.String (Path.to_string bin.path));
      ]))
  in
  let library_json =
    match pkg.library with
    | Some lib -> Json.Object [ ("path", Json.String (Path.to_string lib.path)); ]
    | None -> Json.Null
  in
  let fix_providers_json = Json.Array (List.map pkg.fix_providers ~fn:Fix_provider.to_json) in
  Json.Object [
    ("name", Json.String (Package_name.to_string pkg.name));
    ("path", Json.String (Path.to_string pkg.path));
    ("relative_path", Json.String (Path.to_string pkg.relative_path));
    ("dependencies", dependencies_json);
    ("dev_dependencies", dev_dependencies_json);
    ("build_dependencies", build_dependencies_json);
    ("binaries", binaries_json);
    ("library", library_json);
    ("fix_providers", fix_providers_json);
    ("publish", Json.Object (
      []
      |> (fun fields ->
        match pkg.publish.version with
        | Some version -> ("version", Json.String (Version.to_string version)) :: fields
        | None -> fields)
      |> (fun fields ->
        match pkg.publish.description with
        | Some description -> ("description", Json.String description) :: fields
        | None -> fields)
      |> (fun fields ->
        match pkg.publish.license with
        | Some license -> ("license", Json.String license) :: fields
        | None -> fields)
      |> (fun fields ->
        match pkg.publish.is_public with
        | Some is_public -> ("public", Json.Bool is_public) :: fields
        | None -> fields)
      |> List.reverse
    ));
  ]

let from_json: Json.t -> (t, string) result = fun json ->
  match json with
  | Json.Object fields -> (
      match (Fields.get "name" fields, Fields.get "path" fields, Fields.get "relative_path" fields) with
      | (Some (Json.String name), Some (Json.String path_str), Some (Json.String rel_path_str)) -> (
          let* name =
            Package_name.from_string name
            |> Result.map_err ~fn:Package_name.error_message
          in
          let parse_dependencies_field field_name =
            match Fields.get field_name fields with
            | Some (Json.Array deps) ->
                let rec loop acc entries =
                  match entries with
                  | [] -> Ok (List.reverse acc)
                  | entry :: rest -> (
                      match entry with
                      | Json.Object dep_fields -> (
                          match (Fields.get "name" dep_fields, Fields.get "source" dep_fields) with
                          | (Some (Json.String dep_name), Some source_json) -> (
                              let* dep_name =
                                Package_name.from_string dep_name
                                |> Result.map_err ~fn:Package_name.error_message
                              in
                              let* source = dependency_source_of_json source_json in
                              loop ({ name = dep_name; source } :: acc) rest
                            )
                          | _ -> Error ("Invalid dependency entry in '" ^ field_name ^ "'")
                        )
                      | _ -> Error ("Invalid dependency entry in '" ^ field_name ^ "'")
                    )
                in
                loop [] deps
            | _ -> Ok []
          in
          match Path.from_string path_str with
          | Error _ -> Error ("Invalid path in package JSON: " ^ path_str)
          | Ok path -> (
              match Path.from_string rel_path_str with
              | Error _ -> Error ("Invalid relative_path in package JSON: " ^ rel_path_str)
              | Ok relative_path -> (
                  match parse_dependencies_field "dependencies" with
                  | Error _ as err -> err
                  | Ok dependencies -> (
                      match parse_dependencies_field "dev_dependencies" with
                      | Error _ as err -> err
                      | Ok dev_dependencies -> (
                          match parse_dependencies_field "build_dependencies" with
                          | Error _ as err -> err
                          | Ok build_dependencies ->
                              let binaries =
                                match Fields.get "binaries" fields with
                                | Some (Json.Array bins) ->
                                    List.filter_map
                                      ~fn:(fun __tmp1 ->
                                        match __tmp1 with
                                        | Json.Object bin_fields -> (
                                            match (
                                              Fields.get "name" bin_fields,
                                              Fields.get "path" bin_fields
                                            ) with
                                            | (
                                                Some (Json.String bin_name),
                                                Some (Json.String bin_path)
                                              ) ->
                                                Some { name = bin_name; path = Path.v bin_path }
                                            | _ -> None
                                          )
                                        | _ -> None)
                                      bins
                                | _ -> []
                              in
                              let library =
                                match Fields.get "library" fields with
                                | Some (Json.Object lib_fields) -> (
                                    match Fields.get "path" lib_fields with
                                    | Some (Json.String lib_path) ->
                                        Some { path = Path.v lib_path }
                                    | _ -> None
                                  )
                                | _ -> None
                              in
                              let publish =
                                match Fields.get "publish" fields with
                                | Some (Json.Object publish_fields) ->
                                    let version =
                                      match Fields.get "version" publish_fields with
                                      | Some (Json.String raw_version) -> (
                                          match Version.parse raw_version with
                                          | Ok version -> Ok (Some version)
                                          | Error err ->
                                              Error ("Invalid package publish version in JSON: "
                                              ^ version_parse_error_to_string err)
                                        )
                                      | Some Json.Null
                                      | None -> Ok None
                                      | Some _ -> Error "Package publish version must be a string"
                                    in
                                    let description =
                                      match Fields.get "description" publish_fields with
                                      | Some (Json.String description) -> Ok (Some description)
                                      | Some Json.Null
                                      | None -> Ok None
                                      | Some _ ->
                                          Error "Package publish description must be a string"
                                    in
                                    let license =
                                      match Fields.get "license" publish_fields with
                                      | Some (Json.String license) -> Ok (Some license)
                                      | Some Json.Null
                                      | None -> Ok None
                                      | Some _ -> Error "Package publish license must be a string"
                                    in
                                    let is_public =
                                      match Fields.get "public" publish_fields with
                                      | Some (Json.Bool value) -> Ok (Some value)
                                      | Some Json.Null
                                      | None -> Ok None
                                      | Some _ ->
                                          Error "Package publish public flag must be a boolean"
                                    in
                                    (
                                      match (version, description, license, is_public) with
                                      | (Ok version, Ok description, Ok license, Ok is_public) ->
                                          Ok {
                                            version;
                                            description;
                                            license;
                                            is_public;
                                          }
                                      | (Error err, _, _, _)
                                      | (_, Error err, _, _)
                                      | (_, _, Error err, _)
                                      | (_, _, _, Error err) -> Error err
                                    )
                                | Some _ -> Error "Package publish metadata must be an object"
                                | None -> Ok default_publish_metadata
                              in
                              match publish with
                              | Error _ as err -> err
                              | Ok publish ->
                                  Ok (
                                    canonicalize
                                      {
                                        name;
                                        path;
                                        relative_path;
                                        dependencies;
                                        dev_dependencies;
                                        build_dependencies;
                                        foreign_dependencies = [];
                                        binaries;
                                        library;
                                        sources =
                                          {
                                            src = [];
                                            native = [];
                                            tests = [];
                                            examples = [];
                                            bench = [];
                                          };
                                        compiler = { profile_overrides = []; target_overrides = [] };
                                        commands = [];
                                        fix_providers = [];
                                        publish;
                                      }
                                  )
                        )
                    )
                )
            )
        )
      | _ -> Error "Invalid package JSON"
    )
  | _ -> Error "Package must be a JSON object"

(** Hash package metadata into a hasher state *)
module type Hash_writer = sig
  type state

  val write: state -> string -> unit

  val write_iovec: state -> IO.IoVec.t -> unit

  val write_int: state -> int -> unit

  val write_float: state -> float -> unit

  val write_bool: state -> bool -> unit

  val write_list: (state -> 'a -> unit) -> state -> 'a list -> unit
end

let hash_with = fun (type s) (module H : Hash_writer with type state = s) state (pkg: t) ->
  let hash_string_option value =
    match value with
    | Some value ->
        H.write_bool state true;
        H.write state value
    | None -> H.write_bool state false
  in
  let hash_path_option value =
    match value with
    | Some value ->
        H.write_bool state true;
        H.write state (Path.to_string value)
    | None -> H.write_bool state false
  in
  let rec hash_version (version: Version.t) =
    H.write_int state version.major;
    H.write_int state version.minor;
    H.write_int state version.patch;
    H.write_list
      (fun state segment ->
        match segment with
        | Version.Numeric n ->
            H.write_bool state true;
            H.write_int state n
        | Version.Alphanumeric s ->
            H.write_bool state false;
            H.write state s)
      state
      version.pre;
    hash_string_option version.build
  in
  let hash_requirement requirement =
    match Version.view_requirement requirement with
    | Version.AnyRequirement -> H.write_int state 0
    | Version.ExactRequirement version ->
        H.write_int state 1;
        hash_version version
    | Version.NotEqualRequirement version ->
        H.write_int state 2;
        hash_version version
    | Version.GreaterThanRequirement version ->
        H.write_int state 3;
        hash_version version
    | Version.GreaterThanOrEqualRequirement version ->
        H.write_int state 4;
        hash_version version
    | Version.LessThanRequirement version ->
        H.write_int state 5;
        hash_version version
    | Version.LessThanOrEqualRequirement version ->
        H.write_int state 6;
        hash_version version
    | Version.TildeRequirement version ->
        H.write_int state 7;
        hash_version version
    | Version.PrefixMajorRequirement major ->
        H.write_int state 8;
        H.write_int state major
    | Version.PrefixMinorRequirement (major, minor) ->
        H.write_int state 9;
        H.write_int state major;
        H.write_int state minor
  in
  let hash_kind_override value =
    match value with
    | Profile.Inherit -> H.write_int state 0
    | Profile.Override kind ->
        H.write_int state 1;
        H.write_int
          state
          (
            match kind with
            | Ocaml_compiler.Bytecode -> 0
            | Native -> 1
          )
  in
  let hash_inline_override value =
    match value with
    | Profile.Inherit -> H.write_int state 0
    | Profile.Override (Some n) ->
        H.write_int state 1;
        H.write_int state n
    | Profile.Override None -> H.write_int state 2
  in
  let hash_bool_override value =
    match value with
    | Profile.Inherit -> H.write_int state 0
    | Profile.Override value ->
        H.write_int state 1;
        H.write_bool state value
  in
  let hash_string_list_override value =
    match value with
    | Profile.Inherit -> H.write_int state 0
    | Profile.Override values ->
        H.write_int state 1;
        H.write_list H.write state values
  in
  H.write state (Package_name.to_string pkg.name);
  (* Dependencies metadata *)
  let hash_dependency (dep: dependency) =
    H.write state (Package_name.to_string dep.name);
    H.write_bool state dep.source.workspace;
    H.write_bool state dep.source.builtin;
    hash_path_option dep.source.path;
    hash_string_option dep.source.source_locator;
    hash_string_option dep.source.ref_;
    (
      match dep.source.version with
      | Some version ->
          H.write_bool state true;
          hash_requirement version
      | None -> H.write_bool state false
    )
  in
  List.for_each pkg.dependencies ~fn:hash_dependency;
  List.for_each pkg.dev_dependencies ~fn:hash_dependency;
  List.for_each pkg.build_dependencies ~fn:hash_dependency;
  (
    match pkg.publish.version with
    | Some version ->
        H.write_bool state true;
        hash_version version
    | None -> H.write_bool state false
  );
  (
    match pkg.publish.description with
    | Some description ->
        H.write_bool state true;
        H.write state description
    | None -> H.write_bool state false
  );
  (
    match pkg.publish.license with
    | Some license ->
        H.write_bool state true;
        H.write state license
    | None -> H.write_bool state false
  );
  (
    match pkg.publish.is_public with
    | Some is_public ->
        H.write_bool state true;
        H.write_bool state is_public
    | None -> H.write_bool state false
  );
  (* Binaries metadata *)
  List.for_each
    pkg.binaries
    ~fn:(fun (bin: binary) ->
      H.write state bin.name;
      H.write state (Path.to_string bin.path));
  List.for_each
    pkg.fix_providers
    ~fn:(fun (provider: Fix_provider.t) ->
      H.write state provider.name;
      H.write state (Path.to_string provider.source_path);
      H.write_list H.write state provider.rules);
  (* Library metadata *)
  (
    match pkg.library with
    | Some lib ->
        H.write_bool state true;
        H.write state (Path.to_string lib.path)
    | None -> H.write_bool state false
  );
  (* Compiler configuration - profile and target overrides *)
  let hash_override (override: profile_override) =
    hash_kind_override override.kind;
    hash_inline_override override.inline;
    hash_bool_override override.no_assert;
    hash_bool_override override.compact;
    hash_bool_override override.unsafe;
    hash_bool_override override.no_alias_deps;
    hash_string_list_override override.open_modules;
    hash_string_list_override override.cc_flags;
    hash_string_list_override override.ocamlc_flags
  in
  List.for_each
    pkg.compiler.profile_overrides
    ~fn:(fun ((profile_name, override): string * profile_override) ->
      H.write state profile_name;
      hash_override override);
  List.for_each
    pkg.compiler.target_overrides
    ~fn:(fun ((platform_name, target): string * target_override) ->
      H.write state platform_name;
      (
        match target.profile_override with
        | Some override ->
            H.write_bool state true;
            hash_override override
        | None -> H.write_bool state false
      ));
  (* Source file contents - include explicit [[bin]] entries that may not be in source dirs *)
  let seen_source_files = HashSet.with_capacity ~size:32 in
  let hash_file_content = fun abs_path ->
    match Fs.File.open_read abs_path with
    | Error _ -> false
    | Ok file ->
        let reader = Fs.File.to_reader file in
        let buffer = IO.Buffer.create ~size:16_384 in
        let rec loop () =
          IO.Buffer.clear buffer;
          match IO.Reader.read reader ~into:buffer with
          | Ok 0 -> true
          | Ok _ ->
              H.write_iovec state (IO.Buffer.to_iovec buffer);
              loop ()
          | Error _ -> false
        in
        let success = loop () in
        let _ = Fs.File.close file in
        success
  in
  let hash_source_file file_path =
    let path_str = Path.to_string file_path in
    if HashSet.insert seen_source_files ~value:path_str then (
      let abs_path =
        if Path.is_absolute file_path then
          file_path
        else
          Path.(pkg.path / file_path)
      in
      let path_str = Path.to_string file_path in
      H.write state path_str;
      ignore (hash_file_content abs_path)
    )
  in
  List.for_each pkg.sources.src ~fn:hash_source_file;
  List.for_each pkg.sources.native ~fn:hash_source_file;
  List.for_each pkg.sources.tests ~fn:hash_source_file;
  List.for_each pkg.sources.examples ~fn:hash_source_file;
  List.for_each pkg.sources.bench ~fn:hash_source_file;
  List.for_each
    pkg.binaries
    ~fn:(fun (bin: binary) ->
      let path_str = Path.to_string bin.path in
      if String.ends_with ~suffix:".ml" path_str || String.ends_with ~suffix:".mli" path_str then
        hash_source_file bin.path);
  (* Foreign dependency sources *)
  List.for_each
    pkg.foreign_dependencies
    ~fn:(fun (fdep: foreign_dependency) ->
      H.write state fdep.name;
      H.write state (Path.to_string fdep.path);
      List.for_each fdep.build_cmd ~fn:(H.write state);
      (* Hash all input files *)
      List.for_each
        fdep.inputs
        ~fn:(fun input_path ->
          let abs_path = Path.(fdep.path / input_path) in
          H.write state (Path.to_string input_path);
          ignore (hash_file_content abs_path)))

let hash = fun state pkg -> hash_with (module Crypto.Sha256) state pkg

module Tests = struct
  let package_name value =
    Package_name.from_string value
    |> Result.expect ~msg:"expected valid package name"

  let source = fun
    ?(workspace = false) ?(builtin = false) ?path ?source_locator ?ref_ ?version () ->
    {
      workspace;
      builtin;
      path;
      source_locator;
      ref_;
      version;
    }

  let publish = default_publish_metadata

  let test_parse_dependency_classes () =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
std = { workspace = true }

[dev-dependencies]
propane = { workspace = true }

[build-dependencies]
fixme = { path = "../fixme" }
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let workspace_dep name = { name = package_name name; source = source ~workspace:true () } in
    let pkg =
      from_toml
        toml
        ~workspace_deps:[ workspace_dep "std" ]
        ~workspace_dev_deps:[ workspace_dep "propane" ]
        ~workspace_build_deps:[]
        ~path:(Path.v "/tmp/example")
        ~relative_path:(Path.v "packages/example")
      |> Result.expect ~msg:"expected package manifest"
    in
    if
      List.map pkg.dependencies ~fn:(fun (dep: dependency) -> dep.name) = [ package_name "std" ]
      && List.map pkg.dev_dependencies ~fn:(fun (dep: dependency) -> dep.name)
      = [ package_name "propane" ]
      && List.map pkg.build_dependencies ~fn:(fun (dep: dependency) -> dep.name)
      = [ package_name "fixme" ]
    then
      Ok ()
    else
      Error "expected dependency classes to round-trip" [@test]

  let test_parse_registry_requirement () =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
std = ">= 1.2.3"
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let pkg =
      from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:(Path.v "/tmp/example")
        ~relative_path:(Path.v "packages/example")
      |> Result.expect ~msg:"expected package manifest"
    in
    match pkg.dependencies with
    | [
        {
          source = {
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
          Error "expected parsed dependency requirement to be preserved structurally"
    | _ -> Error "expected a registry dependency with a parsed requirement" [@test]

  let test_parse_builtin_dependency () =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
stdlib = "*"
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let pkg =
      from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:(Path.v "/tmp/example")
        ~relative_path:(Path.v "packages/example")
      |> Result.expect ~msg:"expected package manifest"
    in
    match pkg.dependencies with
    | [ { name; source = { builtin = true; version = Some requirement; _ } } ] when Package_name.equal
      name
      (package_name "stdlib")
    && requirement_is_any requirement -> Ok ()
    | _ -> Error "expected stdlib '*' to parse as a builtin dependency" [@test]

  let test_parse_github_dependency_shorthand () =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
widgets = { github = "riot-tests/widgets" }
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let pkg =
      from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:(Path.v "/tmp/example")
        ~relative_path:(Path.v "packages/example")
      |> Result.expect ~msg:"expected package manifest"
    in
    match pkg.dependencies with
    | [ { name; source = { source_locator = Some "github.com/riot-tests/widgets"; ref_ = None; _ } } ] when Package_name.equal
      name
      (package_name "widgets") -> Ok ()
    | _ -> Error "expected github shorthand to normalize into a source locator" [@test]

  let test_parse_source_dependency_with_ref_and_path () =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
widgets = { source = "https://github.com/riot-tests/monorepo/packages/widgets", ref = "main" }
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    let pkg =
      from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:(Path.v "/tmp/example")
        ~relative_path:(Path.v "packages/example")
      |> Result.expect ~msg:"expected package manifest"
    in
    match pkg.dependencies with
    | [
        {
          name;
          source = {
            source_locator = Some "github.com/riot-tests/monorepo/packages/widgets";
            ref_ = Some "main";
            _;
          };
        };
      ] when Package_name.equal name (package_name "widgets") -> Ok ()
    | _ -> Error "expected source dependency to preserve locator and ref" [@test]

  let test_builtin_dependency_rejects_version_constraints () =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
stdlib = ">= 1.0.0"
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    match from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example") with
    | Ok _ -> Error "expected builtin dependency version constraints to fail"
    | Error _ -> Ok () [@test]

  let test_invalid_registry_requirement_fails_manifest_parse () =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "example"
version = "0.1.0"

[dependencies]
std = "definitely-not-semver"
|}
      |> Result.expect ~msg:"expected package toml to parse"
    in
    match from_toml
      toml
      ~workspace_deps:[]
      ~workspace_dev_deps:[]
      ~workspace_build_deps:[]
      ~path:(Path.v "/tmp/example")
      ~relative_path:(Path.v "packages/example") with
    | Ok _ -> Error "expected invalid semver requirement to fail package parsing"
    | Error _ -> Ok () [@test]

  let test_package_json_round_trips_registry_requirement () =
    let requirement =
      Version.parse_requirement ">= 1.2.3"
      |> Result.expect ~msg:"expected requirement to parse"
    in
    let package = {
      name = package_name "example";
      path = Path.v "/tmp/example";
      relative_path = Path.v "packages/example";
      dependencies = [ { name = package_name "std"; source = source ~version:requirement () }; ];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources =
        {
          src = [];
          native = [];
          tests = [];
          examples = [];
          bench = [];
        };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      publish;
    }
    in
    match from_json (to_json package) with
    | Error err -> Error err
    | Ok decoded ->
        (
          match decoded.dependencies with
          | [
              {
                source = {
                  workspace = false;
                  builtin = false;
                  path = None;
                  source_locator = None;
                  ref_ = None;
                  version = Some decoded_requirement;
                };
                _;
              };
            ] ->
              if String.equal (Version.requirement_to_string decoded_requirement) ">= 1.2.3" then
                Ok ()
              else
                Error "expected registry requirement to survive package json roundtrip"
          | _ -> Error "expected registry dependency after package json roundtrip"
        ) [@test]

  let test_package_json_round_trips_source_dependency () =
    let package = {
      name = package_name "example";
      path = Path.v "/tmp/example";
      relative_path = Path.v "packages/example";
      dependencies = [
        {
          name = package_name "widgets";
          source = source ~source_locator:"github.com/riot-tests/widgets" ~ref_:"main" ();
        };
      ];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources =
        {
          src = [];
          native = [];
          tests = [];
          examples = [];
          bench = [];
        };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      publish;
    }
    in
    match from_json (to_json package) with
    | Ok {
        dependencies = [
            {
              name;
              source = {
                source_locator = Some "github.com/riot-tests/widgets";
                ref_ = Some "main";
                _;
              };
            };
        ];
        _;
      } when Package_name.equal name (package_name "widgets") ->
        Ok ()
    | Ok _ -> Error "expected source dependency to survive package json roundtrip"
    | Error err -> Error err [@test]

  let test_resolve_projects_runtime_and_build_edges () =
    let toml =
      Std.Data.Toml.parse
        {|
[package]
name = "app"
version = "0.1.0"

[dependencies]
std = {}

[build-dependencies]
ppx = {}
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let package =
      from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:(Path.v "/workspace/packages/app")
        ~relative_path:(Path.v "packages/app")
      |> Result.expect ~msg:"expected package manifest"
    in
    let lock_package: Lockfile.package = {
      id =
        {
          registry = None;
          name = package_name "app";
          version = None;
          sha256 = None;
        };
      root = Some (Path.v "packages/app");
      provenance = Lockfile.Workspace;
      dependencies =
        [
          {
            name = package_name "std";
            package =
              {
                registry = Some "pkgs.ml";
                name = package_name "std";
                version = Some "0.1.0";
                sha256 = Some "deadbeef";
              };
          };
        ];
      build_dependencies =
        [
          {
            name = package_name "ppx";
            package =
              {
                registry = Some "pkgs.ml";
                name = package_name "ppx";
                version = Some "1.2.3";
                sha256 = Some "cafebabe";
              };
          };
        ];
      dev_dependencies = [];
    }
    in
    match resolve
      ~package
      ~lock_package
      ~manifest_path:Path.(package.path / Path.v "riot.toml")
      ~materialized_root:package.path with
    | Ok resolved ->
        if List.length resolved.runtime_resolved = 1 && List.length resolved.build_resolved = 1 && (
          match List.get resolved.runtime_resolved ~at:0 with
          | Some resolved_package ->
              Package_name.equal resolved_package.resolved_id.name (package_name "std")
          | None -> false
        ) && (
          match List.get resolved.build_resolved ~at:0 with
          | Some resolved_package -> resolved_package.resolved_id.version = Some "1.2.3"
          | None -> false
        ) then
          Ok ()
        else
          Error "expected resolved package projection to preserve exact ids"
    | Error err -> Error err [@test]

  let test_resolve_requires_all_declared_dependencies () =
    let toml =
      Std.Data.Toml.parse {|
[package]
name = "app"
version = "0.1.0"

[dependencies]
std = {}
|}
      |> Result.expect ~msg:"expected test toml to parse"
    in
    let package =
      from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:(Path.v "/workspace/packages/app")
        ~relative_path:(Path.v "packages/app")
      |> Result.expect ~msg:"expected package manifest"
    in
    let lock_package: Lockfile.package = {
      id =
        {
          registry = None;
          name = package_name "app";
          version = None;
          sha256 = None;
        };
      root = Some (Path.v "packages/app");
      provenance = Lockfile.Workspace;
      dependencies = [];
      build_dependencies = [];
      dev_dependencies = [];
    }
    in
    match resolve
      ~package
      ~lock_package
      ~manifest_path:Path.(package.path / Path.v "riot.toml")
      ~materialized_root:package.path with
    | Ok _ ->
        Error "expected resolve to fail when a declared dependency is missing from the lockfile"
    | Error _ -> Ok () [@test]

  let test_resolve_ignores_builtin_dependencies () =
    let package = {
      name = package_name "app";
      path = Path.v "/workspace/packages/app";
      relative_path = Path.v "packages/app";
      dependencies = [
        { name = package_name "stdlib"; source = source ~builtin:true ~version:Version.any () };
      ];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources =
        {
          src = [];
          native = [];
          tests = [];
          examples = [];
          bench = [];
        };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      publish;
    }
    in
    let lock_package: Lockfile.package = {
      id =
        {
          registry = None;
          name = package_name "app";
          version = None;
          sha256 = None;
        };
      root = Some (Path.v "packages/app");
      provenance = Lockfile.Workspace;
      dependencies = [];
      build_dependencies = [];
      dev_dependencies = [];
    }
    in
    match resolve
      ~package
      ~lock_package
      ~manifest_path:Path.(package.path / Path.v "riot.toml")
      ~materialized_root:package.path with
    | Ok resolved when resolved.runtime_resolved = [] -> Ok ()
    | Ok _ -> Error "expected builtin dependencies to stay out of the resolved lock graph"
    | Error err -> Error err [@test]

  let test_build_graph_dependencies_exclude_build_only_deps () =
    let pkg = {
      name = package_name "example";
      path = Path.v "/tmp/example";
      relative_path = Path.v "packages/example";
      dependencies = [ { name = package_name "std"; source = source ~workspace:true () } ];
      dev_dependencies = [
        { name = package_name "propane"; source = source ~workspace:true () };
      ];
      build_dependencies = [
        { name = package_name "fixme"; source = source ~workspace:true () };
      ];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources =
        {
          src = [];
          native = [];
          tests = [];
          examples = [];
          bench = [];
        };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      publish;
    }
    in
    let build_graph =
      build_graph_dependencies pkg
      |> List.map ~fn:(fun (dep: dependency) -> dep.name)
    in
    let all =
      all_dependencies pkg
      |> List.map ~fn:(fun (dep: dependency) -> dep.name)
    in
    if
      build_graph = [ package_name "std"; package_name "propane" ]
      && all = [ package_name "std"; package_name "propane"; package_name "fixme" ]
    then
      Ok ()
    else
      Error "expected build graph dependencies to exclude build-only deps" [@test]

  let test_dev_scope_dependencies_include_regular_dependencies () =
    let pkg = {
      name = package_name "example";
      path = Path.v "/tmp/example";
      relative_path = Path.v "packages/example";
      dependencies = [ { name = package_name "std"; source = source ~workspace:true () } ];
      dev_dependencies = [
        { name = package_name "propane"; source = source ~workspace:true () };
      ];
      build_dependencies = [
        { name = package_name "fixme"; source = source ~workspace:true () };
      ];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources =
        {
          src = [];
          native = [];
          tests = [];
          examples = [];
          bench = [];
        };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      publish;
    }
    in
    let dev_deps =
      dependencies_for_scope Dev pkg
      |> List.map ~fn:(fun (dep: dependency) -> dep.name)
    in
    if dev_deps = [ package_name "std"; package_name "propane" ] then
      Ok ()
    else
      Error "expected dev scope dependencies to include regular dependencies" [@test]

  let test_root_module_name_sanitizes_hyphenated_package_names () =
    let pkg =
      synthetic
        ~name:(package_name "kernel-new")
        ~path:(Path.v "/tmp/kernel-new")
        ~relative_path:(Path.v "packages/kernel-new")
    in
    if String.equal (root_module_name pkg) "Kernel_new" then
      Ok ()
    else
      Error "expected hyphenated package names to sanitize to a valid root module name" [@test]
end
