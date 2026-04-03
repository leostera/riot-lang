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
    - `concurrency = System.available_parallelism`
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
  ?concurrency:int ->
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

(** Traverse the plan with pre-descent pruning.

    When [concurrency > 1], traversal may visit sibling subtrees in parallel.
    Callback order is therefore not deterministic. The callback [f] may run
    concurrently on multiple worker actors and must therefore be thread-safe
    and free of order-sensitive side effects. [Skip_subtree] applies only to
    the current directory branch, while [Stop] stops traversal globally. *)
val walk:
  t ->
  f:(Fs.Walker.FileItem.t -> Fs.Walker.step) ->
  (unit, error) Result.t

(** Collect all yielded entries into a list. *)
val to_list: t -> (Fs.Walker.FileItem.t list, error) Result.t
