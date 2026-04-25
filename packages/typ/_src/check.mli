open Std
open Analysis
open Model

type prepared_source = {
  display_path: Path.t;
  internal_module_name: LocalModules.InternalName.t;
  local_module_name: LocalModules.AmbientName.t;
  public_module_name: LocalModules.AmbientName.t option;
  source: Source.t;
}

type checked_source = { path: Path.t; analysis: SourceAnalysis.t }

type finished_group = {
  module_name: LocalModules.InternalName.t;
  checked_sources: checked_source list;
  module_result: ModuleTypings.t;
}

type 'acc package_check_result = {
  acc: 'acc;
  loaded_modules: LoadedModules.t;
  public_module_typings: LoadedModules.t;
}

type error =
  | MissingRequirements of {
    module_name: LocalModules.InternalName.t;
    requirements: MissingRequirements.t;
  }
  | MissingModuleTypings of { module_name: LocalModules.InternalName.t }
  | MissingAnalysis of { module_name: LocalModules.InternalName.t; path: Path.t }
  | StoreFailure of { module_name: LocalModules.InternalName.t; reason: string }
  | PackageStoreFailure of { package_name: string; reason: string }

val check: config:TypConfig.t -> source:Source.t -> Check_result.t

(**
   Incrementally check one ordered package source list, one internal module
   group at a time, using authoritative per-group compiled module results as
   the ambient for later groups.

   When [package_name] and [package_fingerprint] are provided, the same
   authoritative public-module typings produced by the incremental engine are
   persisted once as the package bundle after a successful run.

   The callback is invoked once per finished module group in dependency order.
   The returned [package_check_result] carries both the final authoritative
   loaded-module index and the final authoritative public-module typings
   bundle assembled inside the engine.
*)
val fold_package_sources: ?package_name:string -> ?package_fingerprint:Crypto.hash -> config:TypConfig.t -> ordered_sources:prepared_source list -> init:'acc -> f:('acc -> finished_group -> 'acc) -> unit -> ('acc package_check_result, error) result
