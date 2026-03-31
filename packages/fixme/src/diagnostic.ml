open Std

type severity =
  Error
  | Warning
  | Info
  | Hint

type kind =
  | Known of {
      rule_id: string;
      message: string;
    }
  | Generic of {
      rule_id: string;
      message: string;
    }

type t = {
  severity: severity;
  kind: kind;
  span: Syn.Ceibo.Span.t;
  suggestion: string option;
  fix: Fix.fix option;
}

let make = fun ~severity ~kind ~span ?suggestion ?fix () -> {severity; kind; span; suggestion; fix}

let kind = fun diag -> diag.kind

let severity = fun diag -> diag.severity

let message =
  function
  | { kind=Known { message; _ }; _ } -> message
  | { kind=Generic { message; _ }; _ } -> message

let span = fun diag -> diag.span

let rule_id =
  function
  | { kind=Known { rule_id; _ }; _ } -> rule_id
  | { kind=Generic { rule_id; _ }; _ } -> rule_id

let suggestion = fun diag -> diag.suggestion

let fix = fun diag -> diag.fix
