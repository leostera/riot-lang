open Std

(** Immutable analysis view over one [Session] revision. *)
type t

(** Build a snapshot from the current session sources.

    Analyses are prepared lazily per source and only forced by queries that
    actually need them. Before the final per-source analysis runs, the snapshot
    may synthesize an ambient environment from sibling source exports and
    host-loaded module summaries so implicit file modules can participate in
    name resolution. *)
val make: revision:int -> config:TypConfig.t -> sources:Source.t list -> t

(** Monotonic session revision captured by this snapshot. *)
val revision: t -> int

(** Enumerate all per-source analyses in this snapshot.

    Calling this forces every lazy per-source analysis. *)
val analyses: t -> SourceAnalysis.t list

(** Enumerate the in-memory export summaries for every source in this snapshot.

    Calling this forces every lazy per-source analysis. *)
val file_summaries: t -> FileSummary.t list

(** Enumerate the host-facing persisted summaries for every source in this
    snapshot.

    Calling this forces every lazy per-source analysis. *)
val persisted_summaries: t -> PersistedSummary.t list

(** Enumerate the host-facing module summaries for every source in this
    snapshot.

    Calling this forces every lazy per-source analysis. *)
val module_summaries: t -> ModuleSummary.t list

(** Find the analysis for one logical source within this snapshot.

    Calling this forces analysis only for the requested source. *)
val find_analysis: t -> SourceId.t -> SourceAnalysis.t option
