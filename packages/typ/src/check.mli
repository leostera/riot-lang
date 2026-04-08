open Std
open Analysis

(** Backwards-compatible one-shot entrypoint over [Batch.check_source].

    New library consumers should prefer [Session], [Session.Snapshot], and
    [Query]. *)
val check_source:
  filename:Path.t ->
  parse_result:Syn.Parser.parse_result ->
  cst:Syn.Cst.source_file ->
  Check_result.t
