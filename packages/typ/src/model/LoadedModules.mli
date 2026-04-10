open Std

(** Indexed host-loaded module typings.

    The checker hot path should not carry loaded module summaries around as raw
    lists. [LoadedModules] stores those summaries keyed by module name and
    exposes keyed/folded access so sessions and snapshots can avoid repeated
    list merges and lookups. *)
type t

(** Empty loaded-module index. *)
val empty: t

(** Copy an index so callers can take ownership of later in-place updates
    without mutating the original. *)
val copy: t -> t

(** Insert or replace one module typings in place.

    This mutates the owned index directly and invalidates cached views. *)
val add: t -> ModuleTypings.t -> unit

(** Build an index from a list of module typings.

    When duplicates appear, later entries replace earlier ones. *)
val of_list: ModuleTypings.t list -> t

(** Merge two indices while resolving duplicates through [combine].

    [preferred] is inserted first, so duplicate module names are combined as
    [combine existing incoming] where [existing] came from [preferred]. *)
val merge:
  preferred:t -> fallback:t -> combine:(ModuleTypings.t -> ModuleTypings.t -> ModuleTypings.t) -> t

(** Number of loaded modules in the index. *)
val len: t -> int

(** Whether the index is empty. *)
val is_empty: t -> bool

(** Find module typings by module name. *)
val get: t -> module_name:string -> ModuleTypings.t option

(** Whether a module name is present. *)
val contains: t -> module_name:string -> bool

(** Iterate all loaded modules. Iteration order is unspecified. *)
val iter: (string -> ModuleTypings.t -> unit) -> t -> unit

(** Fold all loaded modules. Iteration order is unspecified. *)
val fold: (string -> ModuleTypings.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

(** Recover the loaded module typings as an unordered list. *)
val values: t -> ModuleTypings.t list

(** Recover all loaded module names as an unordered list. *)
val names: t -> string list

(** Stable cache key for the whole loaded-module set. *)
val stable_key: t -> string
