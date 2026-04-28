open Std

type link_set = {
  docs_url: string option;
  package_url: string option;
  homepage_url: string option;
  repository_url: string option;
  source_url: string option;
}

type source_kind =
  | Workspace
  | Registry

type t = {
  requested: string;
  name: Riot_model.Package_name.t;
  source_kind: source_kind;
  resolved_version: string option;
  root: Path.t;
  relative_path: string option;
  workspace_root: Path.t option;
  package_path: string option;
  manifest_path: Path.t;
  manifest: Data.Toml.value option;
  manifest_error: string option;
  is_public: bool option;
  registry_name: string option;
  registry_root: Path.t option;
  registry_package_path: Path.t option;
  description: string option;
  license: string option;
  load_errors: string list;
  links: link_set;
}

type error = { kind: string; message: string }

let registry_name = "pkgs.ml"

let docs_url = fun ~package ~version -> "https://docs.pkgs.ml/p/" ^ package ^ "/" ^ version ^ "/"

let package_url = fun ~package ~version -> "https://pkgs.ml/p/" ^ package ^ "/" ^ version

let rec toml_json = function
  | Data.Toml.String value -> Data.Json.String value
  | Data.Toml.Int value -> Data.Json.Int value
  | Data.Toml.Array values -> Data.Json.Array (List.map values ~fn:toml_json)
  | Data.Toml.Table fields ->
      Data.Json.Object (List.map fields ~fn:(fun (key, value) -> (key, toml_json value)))
  | Data.Toml.Bool value -> Data.Json.Bool value

let json_string_or_null = function
  | Some value -> Data.Json.String value
  | None -> Data.Json.Null

let source_kind_string = function
  | Workspace -> "workspace"
  | Registry -> "registry"

let error = fun ~kind ~message -> Error { kind; message }

let manifest_path = fun root -> Path.normalize Path.(root / Path.v "riot.toml")

let load_manifest = fun path ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.load_riot_toml workspace_manager path with
  | Ok manifest -> (Some manifest, None)
  | Error err -> (None, Some (Riot_model.Workspace_manager.manifest_load_error_message err))

let registry_of_optional = fun ?registry () ->
  match registry with
  | Some registry -> Ok registry
  | None ->
      Pkgs_ml.Registry.create_filesystem ~registry_name ()
      |> Result.map_err ~fn:Pkgs_ml.Registry_cache.create_error_message

let local_workspace_package:
  (Riot_model.Workspace_manifest.t * Riot_model.Workspace_manager.load_error list) option ->
  Riot_model.Package_name.t ->
  (Riot_model.Workspace_manifest.t * Riot_model.Workspace_manager.load_error list * Riot_model.Package_manifest.t) option = fun
  local_workspace package_name ->
  match local_workspace with
  | None -> None
  | Some ((workspace: Riot_model.Workspace_manifest.t), load_errors) ->
      workspace.packages
      |> List.filter ~fn:Riot_model.Package_manifest.is_workspace_member
      |> List.find
        ~fn:(fun (pkg: Riot_model.Package_manifest.t) ->
          Riot_model.Package_name.equal
            pkg.name
            package_name)
      |> Option.map ~fn:(fun pkg -> (workspace, load_errors, pkg))

let package_links = fun ~name ~version ?homepage_url ?repository_url ?source_url () ->
  match version with
  | None ->
      {
        docs_url = None;
        package_url = None;
        homepage_url;
        repository_url;
        source_url;
      }
  | Some version ->
      let package = Riot_model.Package_name.to_string name in
      {
        docs_url = Some (docs_url ~package ~version);
        package_url = Some (package_url ~package ~version);
        homepage_url;
        repository_url;
        source_url;
      }

let workspace_package_links = fun () ->
  {
    docs_url = None;
    package_url = None;
    homepage_url = None;
    repository_url = None;
    source_url = None;
  }

let matching_release = fun (document: Pkgs_ml.Sparse_index.package_document) requirement ->
  document.releases
  |> List.filter_map
    ~fn:(fun (release: Pkgs_ml.Sparse_index.release) ->
      match Std.Version.parse release.version with
      | Error _ -> None
      | Ok version ->
          if Std.Version.matches requirement version then
            Some (version, release)
          else
            None)
  |> List.sort
    ~compare:(fun (left, _) (right, _) ->
      match Std.Version.compare left right with
      | Order.LT -> Order.GT
      | Order.EQ -> Order.EQ
      | Order.GT -> Order.LT)
  |> List.head

let registry_package_info = fun ?registry ~target (parsed: Riot_deps.Registry_package_spec.t) () ->
  match registry_of_optional ?registry () with
  | Error err -> error ~kind:"registry_initialization_failed" ~message:err
  | Ok registry ->
      let package_name = Riot_model.Package_name.to_string parsed.name in
      let requirement = Option.unwrap_or ~default:Std.Version.any parsed.requirement in
      let requirement_string = Std.Version.requirement_to_string requirement in
      let cache = Pkgs_ml.Registry.cache registry in
      let registry_root = Pkgs_ml.Registry_cache.registry_dir cache in
      match Pkgs_ml.Registry.read_package_document registry ~package_name with
      | Error err -> error ~kind:"registry_lookup_failed" ~message:err
      | Ok None ->
          error
            ~kind:"package_not_found"
            ~message:("package '"
            ^ package_name
            ^ "' was not found in "
            ^ Pkgs_ml.Registry.name registry)
      | Ok (Some document) -> (
          match matching_release document requirement with
          | None ->
              error
                ~kind:"version_not_found"
                ~message:("no release of '" ^ package_name ^ "' matches " ^ requirement_string)
          | Some (_, release) -> (
              match Pkgs_ml.Registry.materialize_release
                registry
                ~package_name
                ~version:release.version with
              | Error err -> error ~kind:"materialization_failed" ~message:err
              | Ok _ ->
                  let root =
                    Pkgs_ml.Registry_cache.package_src_dir
                      cache
                      ~package_name
                      ~version:release.version
                  in
                  let manifest_path = manifest_path root in
                  let (manifest, manifest_error) = load_manifest manifest_path in
                  Ok {
                    requested = target;
                    name = parsed.name;
                    source_kind = Registry;
                    resolved_version = Some release.version;
                    root;
                    relative_path = None;
                    workspace_root = None;
                    package_path = None;
                    manifest_path;
                    manifest;
                    manifest_error;
                    is_public = None;
                    registry_name = Some (Pkgs_ml.Registry.name registry);
                    registry_root = Some registry_root;
                    registry_package_path = Some root;
                    description = release.description;
                    license = release.license;
                    load_errors = [];
                    links = package_links
                      ~name:parsed.name
                      ~version:(Some release.version)
                      ?homepage_url:release.homepage
                      ?repository_url:release.repository
                      ?source_url:(Some release.canonical_locator)
                      ();
                  }
            )
        )

let workspace_package_info = fun
  ~workspace_root ~target ~load_errors (pkg: Riot_model.Package_manifest.t) ->
  let version = Option.map pkg.publish.version ~fn:Std.Version.to_string in
  let root = Path.normalize pkg.path in
  let manifest_path = manifest_path root in
  let (manifest, manifest_error) = load_manifest manifest_path in
  Ok {
    requested = target;
    name = pkg.name;
    source_kind = Workspace;
    resolved_version = version;
    root;
    relative_path = Some (Path.to_string pkg.relative_path);
    workspace_root = Some (Path.normalize workspace_root);
    package_path = Some (Path.to_string Path.(pkg.relative_path / Path.v "riot.toml"));
    manifest_path;
    manifest;
    manifest_error;
    is_public = Some (Option.unwrap_or ~default:false pkg.publish.is_public);
    registry_name = None;
    registry_root = None;
    registry_package_path = None;
    description = pkg.publish.description;
    license = pkg.publish.license;
    load_errors = List.map load_errors ~fn:Riot_model.Workspace_manager.load_error_to_string;
    links = workspace_package_links ();
  }

let resolve = fun ?registry ~local_workspace ~target () ->
  let explicit_requirement = String.contains target "@" in
  match Riot_deps.Registry_package_spec.from_string target with
  | Error err ->
      error ~kind:"invalid_target" ~message:(Riot_deps.Registry_package_spec.error_message err)
  | Ok parsed -> (
      match (explicit_requirement, local_workspace_package local_workspace parsed.name) with
      | (false, Some (workspace, load_errors, pkg)) ->
          workspace_package_info ~workspace_root:workspace.root ~target ~load_errors pkg
      | _ -> registry_package_info ?registry ~target parsed ()
    )

let links_json = fun (links: link_set) ->
  Data.Json.Object [
    ("docs_url", json_string_or_null links.docs_url);
    ("package_url", json_string_or_null links.package_url);
    ("homepage_url", json_string_or_null links.homepage_url);
    ("repository_url", json_string_or_null links.repository_url);
    ("source_url", json_string_or_null links.source_url);
  ]

let to_json = fun info ->
  Data.Json.Object [
    ("type", Data.Json.String "package_info");
    ("requested", Data.Json.String info.requested);
    ("name", Data.Json.String (Riot_model.Package_name.to_string info.name));
    ("source_kind", Data.Json.String (source_kind_string info.source_kind));
    ("resolved_version", json_string_or_null info.resolved_version);
    ("root", Data.Json.String (Path.to_string info.root));
    ("relative_path", json_string_or_null info.relative_path);
    ("workspace_root", match info.workspace_root with
    | Some path -> Data.Json.String (Path.to_string path)
    | None -> Data.Json.Null);
    ("package_path", json_string_or_null info.package_path);
    ("manifest_path", Data.Json.String (Path.to_string info.manifest_path));
    ("manifest", match info.manifest with
    | Some manifest -> toml_json manifest
    | None -> Data.Json.Null);
    ("manifest_error", json_string_or_null info.manifest_error);
    ("public", match info.is_public with
    | Some is_public -> Data.Json.Bool is_public
    | None -> Data.Json.Null);
    ("registry", match (info.registry_name, info.registry_root) with
    | (Some registry_name, Some registry_root) ->
        Data.Json.Object [
          ("name", Data.Json.String registry_name);
          ("root", Data.Json.String (Path.to_string registry_root));
          ("package_path", match info.registry_package_path with
          | Some path -> Data.Json.String (Path.to_string path)
          | None -> Data.Json.Null);
        ]
    | _ -> Data.Json.Null);
    ("description", json_string_or_null info.description);
    ("license", json_string_or_null info.license);
    ("load_errors", Data.Json.Array (List.map info.load_errors ~fn:Data.Json.string));
    ("links", links_json info.links);
  ]

let error_to_json = fun ~error ->
  Data.Json.Object [
    ("type", Data.Json.String "package_info_error");
    ("kind", Data.Json.String error.kind);
    ("error", Data.Json.String error.message);
  ]
