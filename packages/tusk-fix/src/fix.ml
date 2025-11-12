open Std

type text_edit = { span : Syn.Ceibo.Span.t; new_text : string }
type fix = { title : string; edits : text_edit list }

(* Future: Implementation of fix application will go here *)
