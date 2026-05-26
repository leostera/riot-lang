type checked_module = { name: String, function_count: i64 }
type diagnostic = Diagnostic(String)

fn check_module(name: String, function_count: i64) -> Result<checked_module, diagnostic> {
  if function_count < 1 {
    Err(Diagnostic("module has no functions"))
  } else {
    Ok(checked_module { name: name, function_count: function_count })
  }
}

fn render(result: Result<checked_module, diagnostic>) -> String {
  match result {
    Ok(module) -> string_concat(module.name, string_concat(":", "ok")),
    Err(diagnostic) ->
      match diagnostic {
        Diagnostic(message) -> string_concat("error:", message)
      }
  }
}

fn main() {
  println(render(check_module("Main", 2)));
  println(render(check_module("Empty", 0)))
}
