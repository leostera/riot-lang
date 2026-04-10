open Std
open Analysis
open Model

type prepared_source = {
  display_path: Path.t;
  internal_module_name: string;
  local_module_name: string;
  public_module_name: string option;
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
  local_alias_typings: ModuleTypings.t list;
  public_module_typings: ModuleTypings.t list;
  loaded_modules: LoadedModules.t;
}
type error =
  | MissingRequirements of { module_name: string; requirements: Session.MissingRequirements.t }
  | MissingModuleTypings of { module_name: string }
  | MissingAnalysis of { module_name: string; path: Path.t }
  | StoreFailure of { module_name: string; reason: string }

(** Backwards-compatible one-shot entrypoint over [Batch.check_source].

    New library consumers should prefer [Session], [Session.Snapshot], and
    [Query]. *)
val check_source:
  filename:Path.t ->
  parse_result:Syn.Parser.parse_result ->
  cst:Syn.Cst.source_file ->
  Check_result.t

(** Incrementally check one ordered package source list, one internal module
    group at a time, using authoritative per-group module typings as the
    ambient for later groups.

    The callback is invoked once per finished module group in dependency order.
    The returned [LoadedModules.t] is the final authoritative loaded-module
    index after processing every group. *)
val fold_package_sources:
  config:TypConfig.t ->
  ordered_sources:prepared_source list ->
  init:'acc ->
  f:('acc -> finished_group -> 'acc) ->
  ('acc * LoadedModules.t, error) result
