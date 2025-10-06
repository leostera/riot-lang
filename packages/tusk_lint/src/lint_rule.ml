open Std

type severity = Error | Warning | Info

type lint_issue = {
  rule_name : string;
  severity : severity;
  message : string;
  suggestion : string option;
  fix : Syn.TokenTree.t list option;
}

module type Rule = sig
  val name : string
  val check : Syn.TokenTree.t list -> lint_issue list
end

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"

let format_issue issue =
  let severity_str = severity_to_string issue.severity in
  let suggestion_str =
    match issue.suggestion with
    | None -> ""
    | Some s -> format "\n  Suggestion: %s" s
  in
  format "[%s] %s: %s%s" severity_str issue.rule_name issue.message
    suggestion_str
