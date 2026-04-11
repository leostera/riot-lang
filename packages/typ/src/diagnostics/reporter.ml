type t = {
  mutable diagnostics: Diagnostic.t list;
}

let create = fun () -> { diagnostics = [] }

let report = fun diagnostic reporter ->
  reporter.diagnostics <- diagnostic :: reporter.diagnostics
