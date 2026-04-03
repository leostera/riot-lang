open Global
open Iter

(** Recursive filesystem walking with an iterator-first API modeled after
    `walkdir`.
    The fast path keeps directory handles open and streams entries lazily.
    Directory contents are only buffered eagerly when sorting is enabled or
    when the open-directory budget is exceeded.

    `Walker` is intentionally iterator-first:
    - [`create`] validates configuration and returns a reusable walk plan
    - [`filter_entry`] refines the walk plan with lazy subtree pruning
    - [`into_iter`] turns that plan into a lazy iterator

    The convenience helpers [`walk`] and [`to_list`] are layered on top of that
    iterator surface. *)
(** Lightweight classification for yielded entries.

    This is derived from the directory entry kind on the common path, with a
    metadata fallback only when the platform reports an unknown kind or when
    symlink-following requires it. `Other` covers non-regular filesystem nodes
    such as block devices, fifos, sockets, and character devices. *)
type entry_kind =
  | File
  | Directory
  | Symlink
  | Other
(** A yielded filesystem entry.

    - `path` is the full path to the discovered entry
    - `depth` is relative to the configured roots:
      - root entries are emitted at depth `0`
      - direct children of a root are depth `1`
      - and so on
    - `kind` is the entry classification used by pruning/filtering helpers *)
type entry = {
  path: Path.t;
  depth: int;
  kind: entry_kind;
}
(** Structured traversal error.

    Errors are yielded inline by the iterator so callers can choose whether to
    abort, report, or continue.

    - `path` is the path being processed when the error happened, when known
    - `depth` is the traversal depth associated with that path
    - `cause` is the underlying filesystem error *)
type error = {
  path: Path.t option;
  depth: int;
  cause: Common.error;
}
(** One yielded walker item.

    The iterator never raises traversal errors. Instead:
    - `Ok entry` yields a filesystem entry
    - `Error error` yields a structured error for the current path *)
type file_item = (entry, error) result
(** Validation errors for walker construction. *)
type create_error =
  | MinDepthCannotBeMoreThanMaxDepth of { min_depth: int; max_depth: int }
(** Control signal returned by [`walk`]'s callback.

    - `Continue` keeps traversing normally
    - `Skip_subtree` prunes the current directory after yielding it
    - `Stop` ends the walk successfully without visiting remaining entries *)
type step =
  | Continue
  | Skip_subtree
  | Stop
(** A validated walk configuration. *)
type t

(** Create a validated filesystem walker.

    `roots` are traversed in the order supplied, unless `sort = true`, in
    which case roots are sorted with [`Path.compare`] before traversal starts.

    Options:
    - `sort`
      Sort entries within each directory. This forces directories to be buffered
      eagerly, so it is more deterministic but less streaming-friendly.
      Defaults to `false`.
    - `follow_symlinks`
      Use followed metadata (`stat`) instead of link metadata (`lstat`) when
      classifying entries. Defaults to `false`.
    - `follow_root_links`
      Treat a root symlink that points at a directory as a directory root and
      descend into it. Non-root symlinks are still governed by
      `follow_symlinks`. Defaults to `true`.
    - `max_open`
      Maximum number of open directory handles the iterator tries to keep live
      at once. When the budget is exceeded, older directories are closed and
      their remaining entries are buffered in memory. Values less than `1`
      clamp to `1`. Defaults to `10`.
    - `min_depth`
      Smallest emitted depth. Entries shallower than this are traversed but not
      yielded. Defaults to `0`.
    - `max_depth`
      Deepest emitted/traversed depth. Entries deeper than this are skipped.
      Defaults to `max_int`.
    - `contents_first`
      Yield directories after their children instead of before them. Defaults
      to `false`.

    Returns `Error (MinDepthCannotBeMoreThanMaxDepth ...)` if the depth range is
    invalid. *)
val create:
  roots:Path.t list ->
  ?sort:bool ->
  ?follow_symlinks:bool ->
  ?follow_root_links:bool ->
  ?max_open:int ->
  ?min_depth:int ->
  ?max_depth:int ->
  ?contents_first:bool ->
  unit ->
  (t, create_error) Result.t

(** Turn a walker into a lazy iterator.

    Each iterator has independent traversal state. Reusing the same walker with
    multiple calls to [`into_iter`] creates multiple fresh traversals.

    Even though the return type is [`Iterator.t`], this should be treated as a
    single-pass streaming iterator backed by live directory handles. Consume it
    linearly instead of relying on backtracking semantics. *)
val into_iter: t -> file_item Iterator.t

(** Refine a walker with a lazy entry filter.

    Entries for which `f` returns `false` are not yielded. If such an entry is a
    directory, its subtree is pruned before descent, mirroring `walkdir`'s
    `filter_entry` behavior.

    Filters compose: applying [`filter_entry`] multiple times keeps only entries
    accepted by all predicates. *)
val filter_entry: t -> f:(entry -> bool) -> t

(** Convenience wrapper over [`create`] + [`into_iter`] + repeated [`next`].

    `walk` is useful when callers want a simple callback-driven traversal
    instead of manual iterator control.

    Behavior:
    - directories are yielded according to the same ordering/options as
      [`create`]
    - returning [`Skip_subtree`] after a directory prunes that directory
    - the first yielded traversal error aborts the walk and returns
      `Error cause`
    - `Stop` ends the walk successfully

    This helper defaults `sort = true` for deterministic callback order. *)
val walk:
  roots:Path.t list ->
  ?sort:bool ->
  ?follow_symlinks:bool ->
  f:(entry -> step) ->
  unit ->
  (unit, Common.error) Result.t

(** Collect a traversal into a list.

    This is a convenience API for callers that genuinely need all entries in
    memory. It is less memory-efficient than using the iterator directly.

    Behavior:
    - aborts on the first yielded traversal error
    - preserves the traversal order of the underlying iterator
    - omits directory entries when `include_directories = false`

    This helper defaults `sort = true` for deterministic output. *)
val to_list:
  roots:Path.t list ->
  ?sort:bool ->
  ?follow_symlinks:bool ->
  ?include_directories:bool ->
  unit ->
  (entry list, Common.error) Result.t
