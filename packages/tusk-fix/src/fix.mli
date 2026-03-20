open Std

type text_edit = Tusk_fix_api.Fix.text_edit = {
  span : Syn.Ceibo.Span.t;
  new_text : string;
}

type fix = Tusk_fix_api.Fix.fix = {
  title : string;
  edits : text_edit list;
}

val make_text_edit : span:Syn.Ceibo.Span.t -> new_text:string -> text_edit
val make : title:string -> edits:text_edit list -> fix
val title : fix -> string
val edits : fix -> text_edit list
val apply_edit : source:string -> text_edit -> (string, string) result
val apply_fix : source:string -> fix -> (string, string) result
val apply_fixes : source:string -> fix list -> (string, string) result
val validate_fix : source:string -> fix -> (unit, string) result
val to_json : fix -> Data.Json.t
