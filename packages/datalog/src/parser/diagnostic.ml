open Std

type t = { message : string; span : Ceibo.Span.t; help : string option }

let make ~message ~span ~help = { message; span; help }

let expected ~expected ~found ~span =
  {
    message =
      "Expected " ^ expected ^ ", found " ^ Token.to_string found.Token.kind;
    span;
    help = None;
  }

let unexpected ~found ~span =
  {
    message = "Unexpected token " ^ Token.to_string found.Token.kind;
    span;
    help = None;
  }

let unterminated_string ~span =
  {
    message = "Unterminated string literal";
    span;
    help = Some "Add closing \" to string";
  }

let lowercase_variable ~name ~span =
  {
    message =
      "Variables must start with uppercase letter, found '" ^ name ^ "'";
    span;
    help =
      Some ("Change '" ^ name ^ "' to '" ^ String.capitalize_ascii name ^ "'");
  }

let missing_rule_body ~span =
  {
    message = "Rule body cannot be empty";
    span;
    help = Some "Add at least one clause after ':-'";
  }

let unclosed_paren ~span =
  { message = "Unclosed parenthesis"; span; help = Some "Add closing ')'" }

let missing_statement_terminator ~span =
  {
    message = "Expected '.' to end statement";
    span;
    help = Some "Add '.' at the end of the statement";
  }

let missing_closing_paren ~span =
  {
    message = "Expected ')' to close argument list";
    span;
    help = Some "Add ')' to close the argument list";
  }

let to_string t =
  match t.help with
  | Some help -> t.message ^ "\n  help: " ^ help
  | None -> t.message
