type typ = TUnknown | TInt | TString

type actor_ref = Scoped(typ) | Existential

type send_result = Accepted(actor_ref) | Rejected(String)

fn render_type(type_: typ) -> String {
  match type_ {
    TUnknown -> "Unknown",
    TInt -> "i64",
    TString -> "String"
  }
}

fn message_matches(expected: typ, actual: typ) {
  match expected {
    TUnknown -> true,
    TInt -> match actual { TInt -> true, _ -> false },
    TString -> match actual { TString -> true, _ -> false }
  }
}

fn send_once(actor: actor_ref, message: typ) -> send_result {
  match actor {
    Existential -> Accepted(Existential),
    Scoped(current) -> match current {
      TUnknown -> Accepted(Scoped(message)),
      _ -> if message_matches(current, message) { Accepted(actor) } else { Rejected(string_concat("expected ", string_concat(render_type(current), string_concat(", got ", render_type(message))))) }
    }
  }
}

fn send_sequence(actor: actor_ref, messages: List<typ>) -> String {
  match messages {
    [] -> "ok",
    [message, ..rest] -> match send_once(actor, message) {
      Accepted(next) -> send_sequence(next, rest),
      Rejected(text) -> text
    }
  }
}

fn main() {
  dbg(string_concat("scoped same: ", send_sequence(Scoped(TUnknown), [TString, TString])));
  dbg(string_concat("scoped mixed: ", send_sequence(Scoped(TUnknown), [TString, TInt])));
  dbg(string_concat("existential mixed: ", send_sequence(Existential, [TString, TInt])))
}
