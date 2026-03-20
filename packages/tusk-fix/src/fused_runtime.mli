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

val plan :
  workspace_root:Path.t ->
  target_dir_root:Path.t ->
  Tusk_model.Fix_provider.t list ->
  plan

val registry_source : Tusk_model.Fix_provider.t list -> string

val materialize :
  workspace_root:Path.t ->
  target_dir_root:Path.t ->
  Tusk_model.Fix_provider.t list ->
  plan
