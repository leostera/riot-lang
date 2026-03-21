open Std

type severity = Error | Warning | Info | Hint

type kind =
  | Known of {
      code : string;
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

let make ~severity ~kind ~span ?suggestion ?fix () =
  { severity; kind; span; suggestion; fix }

let kind diag = diag.kind
let severity diag = diag.severity

let message = function
  | { kind = Known { message; _ }; _ } -> message
  | { kind = Generic { message; _ }; _ } -> message

let span diag = diag.span

let rule_id = function
  | { kind = Known { rule_id; _ }; _ } -> rule_id
  | { kind = Generic { rule_id; _ }; _ } -> rule_id

let code = function
  | { kind = Known { code; _ }; _ } -> Some code
  | { kind = Generic _; _ } -> None

let code_id diag = code diag

let suggestion diag = diag.suggestion
let fix diag = diag.fix
