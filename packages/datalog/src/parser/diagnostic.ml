open Std

type t = { message : string; span : Ceibo.Span.t; help : string option }

let make ~message ~span ~help = { message; span; help }

let expected ~expected ~found ~span =
  {
    message =
      format "Expected %s, found %s" expected (Token.to_string found.Token.kind);
    span;
    help = None;
  }

let unexpected ~found ~span =
  {
    message = format "Unexpected token %s" (Token.to_string found.Token.kind);
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
      format "Variables must start with uppercase letter, found '%s'" name;
    span;
    help =
      Some (format "Change '%s' to '%s'" name (String.capitalize_ascii name));
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
  | Some help -> format "%s\n  help: %s" t.message help
  | None -> t.message
