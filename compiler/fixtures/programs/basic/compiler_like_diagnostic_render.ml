type span = { path: String, line: i64, column: i64 }
type severity = Error | Warning | Info
type diagnostic = Diagnostic(severity, span, String)

fn severity_text(severity: severity) -> String {
  match severity {
    Error -> "error",
    Warning -> "warning",
    Info -> "info"
  }
}

fn render_location(span: span) -> String {
  string_concat(span.path, string_concat(":", "line"))
}

fn render(diagnostic: diagnostic) -> String {
  match diagnostic {
    Diagnostic(severity, location, message) ->
      string_concat(severity_text(severity), string_concat(":", string_concat(render_location(location), string_concat(":", message))))
  }
}

fn main() {
  let location = span { path: "main.ml", line: 3, column: 7 };
  println(render(Diagnostic(Error, location, "expected expression")))
}
