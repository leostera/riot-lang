open Std

type severity = Error | Warning | Info | Hint

type t = {
  severity : severity;
  message : string;
  span : Syn.Ceibo.Span.t;
  rule_id : string;
  suggestion : string option;
  fix : Fix.fix option;
}

val make :
  severity:severity ->
  message:string ->
  span:Syn.Ceibo.Span.t ->
  rule_id:string ->
  ?suggestion:string ->
  ?fix:Fix.fix ->
  unit ->
  t

val severity_to_string : severity -> string
val severity_to_colored_string : severity -> string
val to_string : t -> string
val to_colored_string : t -> string
val to_formatted_output : file:Path.t -> source:string -> t -> string
val to_json : t -> Data.Json.t
val severity : t -> severity
val message : t -> string
val span : t -> Syn.Ceibo.Span.t
val rule_id : t -> string
val suggestion : t -> string option
val fix : t -> Fix.fix option

type grouped = {
  severity : severity;
  message : string;
  spans : Syn.Ceibo.Span.t list;
  rule_id : string;
  suggestion : string option;
  fix : Fix.fix option;
}

val group_diagnostics : t list -> grouped list

val grouped_to_formatted_output :
  file:Path.t -> source:string -> grouped -> string
