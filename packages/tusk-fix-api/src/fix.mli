open Std

(** Fix - Code Transformation Types
    
    This module defines types for representing code fixes and transformations.
    
    Currently contains only type definitions. Implementation of auto-fix
    application will be added in the future.
*)

(** {1 Types} *)

type text_edit = {
  span : Syn.Ceibo.Span.t;  (** Location to edit *)
  new_text : string;  (** Text to insert/replace *)
}
(** A single text edit operation.
    
    Represents replacing the text at [span] with [new_text].
    If [span] is zero-width (start = end), this is an insertion.
    
    Example:
    {[
      (* Replace "Hashtbl" with "Std.Collections.HashMap" *)
      { span = { start = 10; end_ = 17 }; 
        new_text = "Std.Collections.HashMap" }
    ]}
*)

type fix = {
  title : string;  (** Human-readable description *)
  edits : text_edit list;  (** List of edits to apply *)
}
(** A fix that can be applied to source code.
    
    A fix may contain multiple edits that should be applied together.
    Edits must not overlap.
    
    Example:
    {[
      {
        title = "Replace Hashtbl with Std.Collections.HashMap";
        edits = [
          { span = { start = 10; end_ = 17 }; 
            new_text = "Std.Collections.HashMap" }
        ]
      }
    ]}
*)

val make_text_edit : span:Syn.Ceibo.Span.t -> new_text:string -> text_edit
(** Construct a text edit. *)

val make : title:string -> edits:text_edit list -> fix
(** Construct a fix from a set of edits. *)

val title : fix -> string
(** Get the human-readable title for a fix. *)

val edits : fix -> text_edit list
(** Get the edits associated with a fix. *)

val apply_edit : source:string -> text_edit -> (string, string) result
(** Apply a single edit to source text. *)

val apply_fix : source:string -> fix -> (string, string) result
(** Apply a single fix to source text. *)

val apply_fixes : source:string -> fix list -> (string, string) result
(** Apply a list of fixes to source text, rejecting overlapping edits. *)

val validate_fix : source:string -> fix -> (unit, string) result
(** Validate that a fix is well-formed for the given source. *)
