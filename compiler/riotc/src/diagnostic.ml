/// Diagnostic severity reported by riotc.
type Severity = Error | Warning | Info

/// A source location label. Spans will replace the plain string later.
type Label = Label(String)

/// One structured compiler diagnostic.
type Diagnostic = Diagnostic(Severity, Label, String)

fn error(label: String, message: String) -> Diagnostic {
  Diagnostic(Error, Label(label), message)
}

fn warning(label: String, message: String) -> Diagnostic {
  Diagnostic(Warning, Label(label), message)
}

fn severity_name(severity: Severity) -> String {
  match severity {
    Error -> "error",
    Warning -> "warning",
    Info -> "info"
  }
}

fn render(diagnostic: Diagnostic) -> String {
  match diagnostic {
    Diagnostic(severity, label, message) ->
      match label {
        Label(text) ->
          string_concat(string_concat(string_concat(severity_name(severity), ": "), text), string_concat(": ", message))
      }
  }
}
