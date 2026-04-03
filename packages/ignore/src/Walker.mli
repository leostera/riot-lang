open Std

(** Error surfaced by ignore-aware traversal. *)
type error =
  | File_system of { path: Path.t option; cause: Fs.error }
  | Invalid_glob of {
      path: Path.t;
      line: int;
      input: string;
      message: string;
      offset: int option;
    }

(** An ignore-aware walk plan. *)
type t

(** Create a recursive walk plan with ignore-aware pruning.

    Defaults:
    - `sort = false`
    - `follow_symlinks = false`
    - `hidden = true`
    - `parents = true`
    - `ignore = true`
    - `git_ignore = true`
    - `custom_ignore_filenames = []`
    - `overrides = []` *)
val create:
  roots:Path.t list ->
  ?sort:bool ->
  ?follow_symlinks:bool ->
  ?hidden:bool ->
  ?parents:bool ->
  ?ignore:bool ->
  ?git_ignore:bool ->
  ?custom_ignore_filenames:string list ->
  ?overrides:string list ->
  unit ->
  (t, Glob.glob_error) Result.t

(** Traverse the plan with pre-descent pruning. *)
val walk:
  t ->
  f:(Fs.Walker.FileItem.t -> Fs.Walker.step) ->
  (unit, error) Result.t

(** Collect all yielded entries into a list. *)
val to_list: t -> (Fs.Walker.FileItem.t list, error) Result.t
