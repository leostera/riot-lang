type typ =
  | TInt
  | TFloat
  | TBool
  | TString

type constraint =
  | NeedsNumber(String, typ)
  | NeedsI64(String, typ)
  | NeedsBool(String, typ)

fn render_type(typ: typ) -> String {
  match typ {
    TInt -> "i64",
    TFloat -> "f64",
    TBool -> "Bool",
    TString -> "String"
  }
}

fn render_constraint(constraint: constraint) -> String {
  match constraint {
    NeedsNumber(origin, actual) -> string_concat(origin, string_concat(" needs number, got ", render_type(actual))),
    NeedsI64(origin, actual) -> string_concat(origin, string_concat(" needs i64, got ", render_type(actual))),
    NeedsBool(origin, actual) -> string_concat(origin, string_concat(" needs Bool, got ", render_type(actual)))
  }
}

fn collect(constraints: List<constraint>) -> List<String> {
  match constraints {
    [] -> [],
    [constraint, ..rest] -> [render_constraint(constraint), ..collect(rest)]
  }
}

fn join(messages: List<String>) -> String {
  match messages {
    [] -> "",
    [message] -> message,
    [message, ..rest] -> string_concat(message, string_concat(" / ", join(rest)))
  }
}

fn main() {
  let constraints = [
    NeedsNumber("negation", TBool),
    NeedsI64("addition", TFloat),
    NeedsBool("if condition", TString),
    NeedsBool("logical expression", TInt)
  ];
  println(join(collect(constraints)))
}
