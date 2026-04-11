open Std
open Analysis
open Model

type prepared_source = {
  display_path: Path.t;
  internal_module_name: Session.LocalModules.InternalName.t;
  local_module_name: Session.LocalModules.AmbientName.t;
  public_module_name: Session.LocalModules.AmbientName.t option;
  source: Source.t;
}
type checked_source = {
  path: Path.t;
  analysis: Session.SourceAnalysis.t;
}
type finished_group = {
  module_name: string;
  checked_sources: checked_source list;
  module_typings: ModuleTypings.t;
  loaded_modules: LoadedModules.t;
}
type 'acc package_check_result = {
  acc: 'acc;
  loaded_modules: LoadedModules.t;
  public_module_typings: LoadedModules.t;
}
type error =
  | MissingRequirements of { module_name: string; requirements: Session.MissingRequirements.t }
  | MissingModuleTypings of { module_name: string }
  | MissingAnalysis of { module_name: string; path: Path.t }
  | StoreFailure of { module_name: string; reason: string }
  | PackageStoreFailure of { package_name: string; reason: string }

(** Backwards-compatible one-shot entrypoint over [Batch.check_source].

    New library consumers should prefer [Session], [Session.Snapshot], and
    [Query]. *)
val check_source_with_config:
  config:TypConfig.t ->
  filename:Path.t ->
  parse_result:Syn.Parser.parse_result ->
  cst:Syn.Cst.source_file ->
  Check_result.t

val check_source:
  filename:Path.t ->
  parse_result:Syn.Parser.parse_result ->
  cst:Syn.Cst.source_file ->
  Check_result.t

(** Incrementally check one ordered package source list, one internal module
    group at a time, using authoritative per-group module typings as the
    ambient for later groups.

    When [package_name] and [package_fingerprint] are provided, the same
    authoritative public-module typings produced by the incremental engine are
    persisted once as the package bundle after a successful run.

    The callback is invoked once per finished module group in dependency order.
    The returned [package_check_result] carries both the final authoritative
    loaded-module index and the final authoritative public-module typings
    bundle assembled inside the engine. *)
val fold_package_sources:
  ?package_name:string ->
  ?package_fingerprint:Crypto.hash ->
  config:TypConfig.t ->
  ordered_sources:prepared_source list ->
  init:'acc ->
  f:('acc -> finished_group -> 'acc) ->
  unit ->
  ('acc package_check_result, error) result
