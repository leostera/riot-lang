open Std

type severity =
  | Error
  | Warning
  | Info
  | Hint

type kind =
  | Known of {
      rule_id: Rule_id.t;
      message: string;
    }
  | Generic of {
      rule_id: Rule_id.t;
      message: string;
    }

type t = {
  severity: severity;
  kind: kind;
  span: Syn.Ceibo.Span.t;
  suggestion: string option;
  fix: Fix.fix option;
}

let make = fun ~severity ~kind ~span ?suggestion ?fix () ->
  {
    severity;
    kind;
    span;
    suggestion;
    fix;
  }

let kind = fun value ->
  match value with
  | diag -> diag.kind

let severity = fun diag -> diag.severity

let message = fun diagnostic ->
  match diagnostic with
  | { kind = Known { message; _ }; _ } -> message
  | { kind = Generic { message; _ }; _ } -> message

let span = fun diag -> diag.span

let rule_id = fun diagnostic ->
  match diagnostic with
  | { kind = Known { rule_id; _ }; _ } -> rule_id
  | { kind = Generic { rule_id; _ }; _ } -> rule_id

let suggestion = fun diag -> diag.suggestion

let fix = fun diag -> diag.fix
