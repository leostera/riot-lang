open Std
open Infer
open Model

(** Immutable analysis view over one [Session] revision. *)
type t

module SharedCaches: sig
  type t

  (** Create one reusable cross-snapshot cache scope.

      Reusing the same cache scope across repeated rooted snapshots lets hosts
      avoid reanalyzing sources whose revision, loaded-module set, and
      dependency analysis modes are unchanged. *)
  val create: unit -> t
end

(** Build a rooted snapshot from the current session sources.

    Analyses are prepared lazily per source and only forced by queries that
    actually need them. Before the final per-source analysis runs, the snapshot
    may synthesize an ambient environment from sibling source exports and
    host-loaded module typings so implicit file modules can participate in
    name resolution.

    This constructor creates one standalone cache scope for the snapshot. Hosts
    that prepare many rooted snapshots in one execution should prefer
    [make_with_shared_caches]. *)
val make: revision:int -> roots:SourceId.t list -> config:TypConfig.t -> sources:Source.t list -> t

(** Build a rooted snapshot while reusing one host-owned shared cache scope. *)
val make_with_shared_caches:
  revision:int ->
  roots:SourceId.t list ->
  config:TypConfig.t ->
  sources:Source.t list ->
  shared_caches:SharedCaches.t ->
  t

(** Monotonic session revision captured by this snapshot. *)
val revision: t -> int

(** Root sources this snapshot was prepared for. *)
val roots: t -> SourceId.t list

(** Enumerate all per-source analyses in this snapshot.

    Calling this forces every lazy per-root analysis. *)
val analyses: t -> SourceAnalysis.t list

(** Enumerate the in-memory export summaries for every source in this snapshot.

    Calling this forces every lazy per-root analysis. *)
val file_summaries: t -> FileSummary.t list

(** Enumerate the canonical host-facing module typings for every rooted module in this
    snapshot.

    When a rooted module has both [.ml] and [.mli] sources in the session, the
    returned typings are paired and interface-shaped. Calling this forces every
    lazy per-root analysis needed to produce those canonical module typings. *)
val module_typings: t -> ModuleTypings.t list

(** Find the canonical host-facing module typings for one rooted source's
    module, when present.

    This preserves the rooted query boundary while ensuring callers see the
    same canonical [ModuleTypings] that [module_typings] enumerates for that
    module. *)
val find_module_typings: t -> SourceId.t -> ModuleTypings.t option

(** Find visible module typings for one module name within this snapshot. *)
val find_module_typings_by_name: t -> string -> ModuleTypings.t option

(** Find the analysis for one logical source within this snapshot.

    For paired [.ml]/[.mli] modules, the returned analysis includes any
    signature-inclusion diagnostics derived from comparing the implementation
    against the interface. Calling this forces analysis only for the requested
    root source and any same-module siblings needed to pair it. *)
val find_analysis: t -> SourceId.t -> SourceAnalysis.t option
