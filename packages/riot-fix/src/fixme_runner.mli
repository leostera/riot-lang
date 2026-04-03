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
  package_name: string;
  binary_name: string;
  package: Riot_model.Package.t;
  providers: generated_provider list;
}
val plan: workspace_root:Path.t -> target_dir_root:Path.t -> Riot_model.Fix_provider.t list -> plan

val registry_source: Riot_model.Fix_provider.t list -> string

val package_dependencies:
  workspace_root:Path.t -> generated_provider list -> Riot_model.Package.dependency list

val attach_to_workspace: Riot_model.Workspace.t -> plan -> Riot_model.Workspace.t

val materialize:
  workspace_root:Path.t -> target_dir_root:Path.t -> Riot_model.Fix_provider.t list -> plan
