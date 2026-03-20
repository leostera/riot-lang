open Std

type plan = {
  provider_hash : string;
  generated_dir : Path.t;
  registry_path : Path.t;
  package_name : string;
  binary_name : string;
}

val plan :
  target_dir_root:Path.t ->
  Tusk_model.Fix_provider.t list ->
  plan

val registry_source : Tusk_model.Fix_provider.t list -> string
