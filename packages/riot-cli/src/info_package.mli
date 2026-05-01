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

val resolve:
  ?registry:Pkgs_ml.Registry.t ->
  local_workspace:(Riot_model.Workspace_manifest.t * Riot_model.Workspace_manager.load_error list) option ->
  target:string ->
  unit ->
  (t, error) result

val to_json: t -> Data.Json.t

val error_to_json: error:error -> Data.Json.t
