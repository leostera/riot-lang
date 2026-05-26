type action =
  | Output(String)
  | Actor(String)
  | Value(String)

type verdict =
  | Accepted(String)
  | Rejected(String)

fn has_effect(actions: List<action>) -> bool {
  match actions {
    [] -> false,
    [action, ..rest] ->
      match action {
        Output(_) -> true,
        Actor(_) -> true,
        Value(_) -> has_effect(rest)
      }
  }
}

fn render_action(action: action) -> String {
  match action {
    Output(label) -> string_concat("output:", label),
    Actor(label) -> string_concat("actor:", label),
    Value(label) -> string_concat("value:", label)
  }
}

fn render_actions(actions: List<action>) -> String {
  match actions {
    [] -> "",
    [action] -> render_action(action),
    [action, ..rest] -> string_concat(render_action(action), string_concat(",", render_actions(rest)))
  }
}

fn validate_main(actions: List<action>) -> verdict {
  if has_effect(actions) {
    Accepted(render_actions(actions))
  } else {
    Rejected("unsupported main body")
  }
}

fn render_verdict(verdict: verdict) -> String {
  match verdict {
    Accepted(details) -> string_concat("accepted:", details),
    Rejected(message) -> string_concat("rejected:", message)
  }
}

fn main() {
  let output_main = [Value("setup"), Output("println")];
  let actor_main = [Value("worker"), Actor("send")];
  let value_tail_main = [Value("1")];
  let first = render_verdict(validate_main(output_main));
  let second = render_verdict(validate_main(actor_main));
  let third = render_verdict(validate_main(value_tail_main));
  println(string_concat(first, string_concat("; ", string_concat(second, string_concat("; ", third)))))
}
