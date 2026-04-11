open Analysis
open Diagnostics
open Model

module Env: module type of Env

module Summary2: module type of Summary2

module Region: module type of Region

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

    Imported package/module state is resolved through [ImportedWorld]. The
    local checker env then carries only lexical/local typing state for the
    current source. *)
val infer_file:
  imported_world:ImportedWorld.t -> config:TypConfig.t -> source:Source.t -> SemanticTree.file -> t
