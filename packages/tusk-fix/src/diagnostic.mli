open Std

type severity = Tusk_fix_api.Diagnostic.severity =
  | Error
  | Warning
  | Info
  | Hint

type kind = Tusk_fix_api.Diagnostic.kind =
  | Known of {
      rule_id : string;
      message : string;
    }
  | Generic of {
      rule_id : string;
      message : string;
    }

type t = Tusk_fix_api.Diagnostic.t = {
  severity : severity;
  kind : kind;
  span : Syn.Ceibo.Span.t;
  suggestion : string option;
  fix : Fix.fix option;
}

val make :
  severity:severity ->
  kind:kind ->
  span:Syn.Ceibo.Span.t ->
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
val kind : t -> kind
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

val grouped_list_to_formatted_output :
  file:Path.t -> source:string -> grouped list -> string
