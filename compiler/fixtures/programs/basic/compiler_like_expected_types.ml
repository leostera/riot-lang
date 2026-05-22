type expected =
  | ExpectBool(String)
  | ExpectI64(String)
  | ExpectSame(String)

type actual =
  | GotBool
  | GotI64
  | GotString

type check = { expected: expected, actual: actual }

fn render_actual(actual: actual) -> String {
  match actual {
    GotBool -> "Bool",
    GotI64 -> "i64",
    GotString -> "String"
  }
}

fn render_expected(expected: expected) -> String {
  match expected {
    ExpectBool(context) -> string_concat(context, " expects Bool"),
    ExpectI64(context) -> string_concat(context, " expects i64"),
    ExpectSame(context) -> string_concat(context, " expects matching types")
  }
}

fn render_check(check: check) -> String {
  string_concat(render_expected(check.expected), string_concat(" but got ", render_actual(check.actual)))
}

fn append(left: List<String>, right: List<String>) -> List<String> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
  }
}

fn render_checks(checks: List<check>) -> List<String> {
  match checks {
    [] -> [],
    [check, ..rest] -> [render_check(check), ..render_checks(rest)]
  }
}

fn join(messages: List<String>) -> String {
  match messages {
    [] -> "",
    [message] -> message,
    [message, ..rest] -> string_concat(message, string_concat("; ", join(rest)))
  }
}

fn main() {
  let checks = [
    check { expected: ExpectBool("if condition"), actual: GotI64 },
    check { expected: ExpectBool("logical expression"), actual: GotString },
    check { expected: ExpectI64("arithmetic expression"), actual: GotBool },
    check { expected: ExpectSame("match arm"), actual: GotString }
  ];
  println(join(render_checks(checks)))
}
