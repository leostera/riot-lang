open Std
open Model

(** Host-supplied configuration for one [Session]. *)
type env = (SurfacePath.t * TypeScheme.t) list
type t = {
  (**
     Intrinsic language-level bindings visible in every source.

     This must stay small. Library/module APIs should arrive through persisted
     module summaries and host-provided ambient inputs, not by growing the
     default prelude beyond syntax-backed language forms.
  *)
  prelude: env;
  (**
     Host-loaded reusable module typings available to every source in the
     session.
  *)
  loaded_modules: LoadedModules.t;
  (**
     Optional semantic store used to hydrate canonical module typings during
     rooted snapshot preparation.
  *)
  store: Store.t option;
  (**
     Whether inference should retain trace payloads such as per-expression
     environments and per-item export snapshots. Hosts that only need
     diagnostics and module typings can disable this to avoid building large
     trace structures.
  *)
  capture_traces: bool;
  (** Snapshot-scoped bindings synthesized from sibling sources or host context. *)
  ambient: env;
  (**
     Names that should be hidden from export summaries because they originate
     outside the current source, such as prelude bindings or ambient module
     surfaces.

     Package checking may override this directly while keeping the actual
     checker input env in a separate persistent structure.
  *)
  hidden_export_names: SurfacePath.t list;
  (** Snapshot-scoped lowered type declarations synthesized from summaries. *)
  ambient_type_decls: FileSummary.type_decl list;
  (** Snapshot-scoped visible type context built from [ambient_type_decls]. *)
  ambient_visible_types: VisibleTypes.t;
  (**
     Optional structured event sink used to observe snapshot preparation and
     source analysis progress without scraping logs.
  *)
  on_event: (Event.t -> unit) option;
}

(**
   Default host configuration used by the current prototype and tests.

   The default keeps [prelude] limited to syntax-backed language intrinsics
   and starts with an empty loaded-module index. Hosts are expected to hydrate
   reusable module summaries through [with_loaded_modules] or
   [with_loaded_module_index].
*)
val default: t

(** Replace the snapshot ambient environment while preserving the base prelude. *)
val with_ambient: t -> ambient:env -> t

(** Recover the current visible ambient type declarations. *)
val ambient_type_decls: t -> FileSummary.type_decl list

(** Recover the current hidden-export name set. *)
val hidden_export_names: t -> SurfacePath.t list

(**
   Replace the hidden-export name set directly.

   This is the natural entrypoint for package checking, where the checker may
   type against one persistent env while still needing an explicit export
   filter for imported module surfaces.
*)
val with_hidden_export_names: t -> hidden_export_names:SurfacePath.t list -> t

(**
   Replace the snapshot ambient type declarations while preserving the base
   prelude, loaded modules, and ambient value environment.
*)
val with_ambient_type_decls: t -> ambient_type_decls:FileSummary.type_decl list -> t

(**
   Replace the snapshot visible type context directly. This is the natural
   entrypoint for hosts that already maintain an incremental visible-type view
   and want to avoid rebuilding it from raw type-decl lists on every config
   mutation.
*)
val with_ambient_visible_types: t -> ambient_visible_types:VisibleTypes.t -> t

(**
   Replace the host-loaded reusable module typings while preserving the
   base prelude and snapshot ambient environment.
*)
val with_loaded_modules: t -> loaded_modules:ModuleTypings.t list -> t

(** Replace the host-loaded reusable module index directly. *)
val with_loaded_module_index: t -> loaded_modules:LoadedModules.t -> t

(**
   Replace the optional semantic store used during rooted snapshot
   preparation.
*)
val with_store: t -> store:Store.t option -> t

(**
   Toggle retention of expression and item traces. When disabled, the checker
   still computes diagnostics and module typings, but omits trace payloads and
   leaves the type index empty.
*)
val with_capture_traces: t -> capture_traces:bool -> t

(** Attach one structured event sink to the config. *)
val with_on_event: t -> on_event:(Event.t -> unit) -> t

(** Remove any structured event sink from the config. *)
val without_on_event: t -> t

(**
   Emit one structured event when the config carries a sink. The thunk is only
   forced when event delivery is enabled.
*)
val emit_event: t -> (unit -> Event.kind) -> unit
