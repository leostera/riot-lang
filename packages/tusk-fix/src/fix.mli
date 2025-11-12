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

(** {1 Future Functions}
    
    These will be implemented when auto-fix support is added:
    
    - [val apply_edit : source:string -> text_edit -> string]
    - [val apply_fix : source:string -> fix -> string]
    - [val apply_fixes : source:string -> fix list -> string]
    - [val validate_fix : source:string -> fix -> (unit, string) result]
*)
