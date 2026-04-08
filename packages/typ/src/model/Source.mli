open Std

(** Logical source tracked by one [Session]. *)
type kind =
  (** Source text backed by an on-disk file. *)
  | File
  (** Source text synthesized for a fragment-oriented query. *)
  | Fragment
  (** Source text produced by a generator or macro lane. *)
  | Generated
type origin =
  (** Host path for a file-backed source. *)
  | Path of Path.t
  (** Human-readable label for fragments and synthetic inputs. *)
  | Label of string
type t = {
  (** Stable identity preserved across text updates to this logical source. *)
  source_id: SourceId.t;
  (** Host-declared source category. *)
  kind: kind;
  (** Canonical module identity for this logical source inside the current
      host-owned session graph. *)
  module_name: string;
  (** Semantic equivalent of host-side compiler `-open` flags applied before
      checking this source. *)
  implicit_opens: IdentPath.t list;
  (** Host-owned origin label. *)
  origin: origin;
  (** Stable typing-input hash for this source revision. *)
  source_hash: Crypto.hash;
  (** Monotonic revision number for this source snapshot. *)
  revision: int;
  (** Prepared parse result for this exact source text. *)
  parse_result: Syn.Parser.parse_result;
  (** Prepared CST lift for this exact source text. *)
  cst: Syn.Cst.source_file;
}

(** Compute the stable hash for one prepared typing input. *)
val hash: implicit_opens:IdentPath.t list -> cst:Syn.Cst.source_file -> Crypto.hash

(** Build one logical source record from host-prepared parse and CST
    artifacts. *)
val make_prepared:
  source_id:SourceId.t ->
  kind:kind ->
  module_name:string ->
  implicit_opens:IdentPath.t list ->
  origin:origin ->
  revision:int ->
  source_hash:Crypto.hash ->
  parse_result:Syn.Parser.parse_result ->
  cst:Syn.Cst.source_file ->
  t

(** Host-side fallback for simple file-backed inputs that do not already know a
    richer planner/module identity. *)
val infer_module_name: origin -> string

(** Read the canonical module name for this logical source. *)
val module_name: t -> string

(** Compute a stable content hash for cacheing the source's exported summary.

    The hash is based on the prepared semantic syntax plus ambient implicit
    opens, but not on [source_id] or [revision], so equivalent logical sources
    can reuse cached summaries across sessions. *)
val input_hash: t -> Crypto.hash

(** Render the best available human-facing label for this source. *)
val display_name: t -> string
