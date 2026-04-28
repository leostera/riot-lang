open Std

(**
   CLI support for reviewing pending snapshot updates.

   This module powers [riot snapshots], which finds [.expected.new] files,
   lets a caller review them, and either promotes or rejects the pending
   outputs.
*)
type pending_snapshot = {
  (** Approved snapshot path currently checked into the repo. *)
  approved: Path.t;
  (** Newly generated candidate snapshot waiting for review. *)
  pending: Path.t;
}
type review_decision = [ | `Approve | `Reject | `Ignore | `Quit]
type review_summary = {
  (** Number of pending snapshots approved during the review pass. *)
  approved_count: int;
  (** Number of pending snapshots rejected during the review pass. *)
  rejected_count: int;
  (** Number of pending snapshots left untouched. *)
  ignored_count: int;
  (** Whether the review loop stopped early due to [`Quit]. *)
  quit: bool;
}
type 'value scan_step =
  | Continue of 'value
  | Stop of 'value

(** Command definition for [riot snapshots]. *)
val command: ArgParser.command

(**
   Discover pending snapshot files under supported snapshot locations.

   Snapshot candidates are only searched under [.riot/snapshots] and
   [packages/*/tests]. Other workspace paths are intentionally ignored.

   Use [query] to narrow the scan to paths matching a substring.
*)
val discover_pending_snapshots:
  workspace_root:Path.t ->
  ?query:string ->
  unit ->
  (pending_snapshot list, IO.error) result

(**
   Fold pending snapshot files from supported snapshot locations as they are
   discovered.

   Returning [Stop value] ends discovery early. This is the streaming surface
   used by snapshot commands that should show feedback before the whole
   workspace has been scanned.
*)
val fold_pending_snapshots:
  workspace_root:Path.t ->
  ?query:string ->
  init:'value ->
  fn:('value -> pending_snapshot -> ('value scan_step, IO.error) result) ->
  unit ->
  ('value, IO.error) result

(** Promote pending snapshot files into their approved locations. *)
val approve_pending_snapshots: pending_snapshot list -> (unit, IO.error) result

(** Delete pending snapshot files without promoting them. *)
val reject_pending_snapshots: pending_snapshot list -> (unit, IO.error) result

(** Parse an interactive review choice such as ["a"], ["r"], ["i"], or ["q"]. *)
val parse_review_decision: string -> review_decision option

(**
   Review pending snapshots using a caller-provided decision function.

   Use this to share the core review loop between interactive and
   non-interactive frontends.
*)
val review_pending_snapshots_with_decider:
  workspace_root:Path.t ->
  pending_snapshot list ->
  decide:(pending_snapshot -> (review_decision, IO.error) result) ->
  (review_summary, IO.error) result

(** Run [riot snapshots] in a workspace. *)
val run: workspace:Riot_model.Workspace.t -> ArgParser.matches -> (unit, exn) result
