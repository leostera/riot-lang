type diagnostic_module = {
  name: String,
  spans: i64,
  primary_messages: i64,
  hints: i64,
  tests: i64,
  snapshots: i64,
  regressions: i64
}

type diagnostic_snapshot = {
  modules: i64,
  spans: i64,
  primary_messages: i64,
  hints: i64,
  tests: i64,
  snapshots: i64,
  regressions: i64
}

fn empty_snapshot() -> diagnostic_snapshot {
  diagnostic_snapshot {
    modules: 0,
    spans: 0,
    primary_messages: 0,
    hints: 0,
    tests: 0,
    snapshots: 0,
    regressions: 0
  }
}

fn add_module(total: diagnostic_snapshot, module: diagnostic_module) -> diagnostic_snapshot {
  diagnostic_snapshot {
    modules: total.modules + 1,
    spans: total.spans + module.spans,
    primary_messages: total.primary_messages + module.primary_messages,
    hints: total.hints + module.hints,
    tests: total.tests + module.tests,
    snapshots: total.snapshots + module.snapshots,
    regressions: total.regressions + module.regressions
  }
}

fn summarize_modules(modules: List<diagnostic_module>, total: diagnostic_snapshot) -> diagnostic_snapshot {
  match modules {
    [] -> total,
    [module, ..rest] -> summarize_modules(rest, add_module(total, module))
  }
}

fn review_module(module: diagnostic_module) -> String {
  if module.regressions == 0 {
    if module.snapshots < module.tests {
      string_concat(module.name, ":needs-snapshot")
    } else {
      string_concat(module.name, ":pinned")
    }
  } else {
    string_concat(module.name, ":regression")
  }
}

fn review_modules(modules: List<diagnostic_module>, reviews: List<String>) -> List<String> {
  match modules {
    [] -> reviews,
    [module, ..rest] -> review_modules(rest, [review_module(module), ..reviews])
  }
}

fn main() {
  let lexer = diagnostic_module { name: "LexerDiag", spans: 4, primary_messages: 3, hints: 2, tests: 6, snapshots: 5, regressions: 0 };
  let parser = diagnostic_module { name: "ParserDiag", spans: 7, primary_messages: 8, hints: 6, tests: 12, snapshots: 11, regressions: 1 };
  let checker = diagnostic_module { name: "CheckerDiag", spans: 9, primary_messages: 10, hints: 8, tests: 15, snapshots: 15, regressions: 0 };
  let backend = diagnostic_module { name: "BackendDiag", spans: 3, primary_messages: 4, hints: 2, tests: 7, snapshots: 6, regressions: 0 };
  let modules = [lexer, parser, checker, backend];
  let summary = summarize_modules(modules, empty_snapshot());
  let reviews = review_modules(modules, []);
  dbg(summary.modules);
  dbg(summary.spans);
  dbg(summary.primary_messages);
  dbg(summary.hints);
  dbg(summary.tests);
  dbg(summary.snapshots);
  dbg(summary.regressions);
  match reviews {
    [first, .._] -> dbg(first),
    [] -> dbg("no-review")
  }
}
