open Std

type severity =
  | Error
  | Warning
  | Info
  | Hint
type kind =
  | Known of {
      rule_id : string;
      message : string;
    }
  | Generic of {
      rule_id : string;
      message : string;
    }
type t = {
  severity : severity;
  kind : kind;
  span : Syn.Ceibo.Span.t;
  suggestion : string option;
  fix : Fix.fix option;
}
val make : severity:severity ->
kind:kind ->
span:Syn.Ceibo.Span.t ->
?suggestion:string ->
?fix:Fix.fix ->
unit ->
t

val kind : t -> kind

val severity : t -> severity

val message : t -> string

val span : t -> Syn.Ceibo.Span.t

val rule_id : t -> string

val suggestion : t -> string option

val fix : t -> Fix.fix option
