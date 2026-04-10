open Std
open Model

module Snapshot: module type of Snapshot

module SourceAnalysis: module type of SourceAnalysis

module ModulePairing: module type of ModulePairing

module ModuleSurface: module type of ModuleSurface

module MissingRequirements: module type of MissingRequirements

module LocalModules: module type of LocalModules

(** Mutable host-owned set of logical sources. *)
type t

(** Start a session with one host-supplied configuration. *)
val empty: config:TypConfig.t -> t

(** Inspect the host configuration carried by the session. *)
val config: t -> TypConfig.t

(** Replace the host configuration while preserving the current sources. *)
val with_config: t -> config:TypConfig.t -> t

(** Add one logical source whose parse result and CST were prepared by the
    host ahead of time and return its stable [SourceId].

    Hosts should use this when they already have planner-owned or editor-owned
    parse artifacts and want to seed a session without reparsing the same
    source again. *)
val create_source:
  t ->
  kind:Source.kind ->
  module_name:string ->
  implicit_opens:SurfacePath.t list ->
  origin:Source.origin ->
  source_hash:Crypto.hash ->
  parse_result:Syn.Parser.parse_result ->
  cst:Syn.Cst.source_file ->
  t * SourceId.t

(** Register one additional module-name alias for an existing source.

    Hosts use this when planner-owned sources have an internal canonical module
    name but should also satisfy local dependency discovery through shorter
    package-relative names such as [Cell] or [Sync.Cell]. *)
val register_source_alias: t -> SourceId.t -> module_name:string -> t

(** Replace one existing source while preserving its [SourceId]. *)
val update_source:
  t ->
  SourceId.t ->
  source_hash:Crypto.hash ->
  parse_result:Syn.Parser.parse_result ->
  cst:Syn.Cst.source_file ->
  t

(** Remove one logical source from future snapshots. *)
val remove_source: t -> SourceId.t -> t

(** Prepare one rooted immutable snapshot.

    Preparing a rooted snapshot validates that all requested root sources are
    present in the session, expands each rooted logical module to any sibling
    [.ml]/[.mli] sources already present in the session, discovers module
    dependencies for that rooted closure, and returns a structured
    missing-requirements payload when they are not available. *)
val prepare_snapshot: t -> roots:SourceId.t list -> (Snapshot.t, MissingRequirements.t) result

(** Freeze the current session state into one immutable [Snapshot].

    Per-source analyses are loaded lazily from the resulting snapshot so LSP
    callers can query only the files they touch, while compiler-style callers
    can still force everything through [Snapshot.analyses] or the batch lane.

    This is the compatibility wrapper for preparing a rooted snapshot over all
    current session sources. *)
val snapshot: t -> Snapshot.t
