open Std
open Analysis

(** One-shot batch wrapper built on top of [Session] and [Session.Snapshot].

    This is the compatibility lane for compiler-style callers that want one
    analyzed result for one prepared source without managing a persistent
    session. *)
val check_source:
  filename:Path.t ->
  parse_result:Syn.Parser.parse_result ->
  cst:Syn.Cst.source_file ->
  Check_result.t
