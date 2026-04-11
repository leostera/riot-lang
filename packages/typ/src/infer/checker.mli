open Analysis
open Diagnostics
open Model

module Env: module type of Env

module Solver: module type of Solver

(** Result of inferring types for one semantic tree. *)
type t = {
  (** File-local exports after filtering the configured prelude. *)
  exports: FileSummary.exports;
  (** Export-facing binding references used for definition queries. *)
  export_bindings: Check_result.binding_ref list;
  (** Exported lowered type declarations, including reexports. *)
  type_decls: FileSummary.type_decl list;
  (** Per-item export snapshots produced during inference. *)
  item_traces: Check_result.item_trace list;
  (** Per-expression environment and type snapshots produced during inference. *)
  expr_traces: Check_result.expr_trace list;
  (** Typing diagnostics emitted while inferring the file. *)
  diagnostics: Diagnostic.t list;
}

(** Infer types for a semantic tree using the shared checker core.

    The host configuration supplies only the intrinsic prelude plus ambient
    module summaries, so package-check and query callers share the same
    inference rules without hardcoding package/library APIs in the inferencer. *)
val initial_env_of_config: config:TypConfig.t -> Env.t

val infer_file: ?initial_env:Env.t -> config:TypConfig.t -> source:Source.t -> SemanticTree.file -> t
