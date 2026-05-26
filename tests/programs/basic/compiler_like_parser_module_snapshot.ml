type parser_module = {
  name: String,
  imports: i64,
  exports: i64,
  token_cases: i64,
  productions: i64,
  recovery_rules: i64,
  snapshots: i64,
  diagnostics: i64
}

type parser_snapshot = {
  modules: i64,
  imports: i64,
  exports: i64,
  token_cases: i64,
  productions: i64,
  recovery_rules: i64,
  snapshots: i64,
  diagnostics: i64
}

fn empty_snapshot() -> parser_snapshot {
  parser_snapshot {
    modules: 0,
    imports: 0,
    exports: 0,
    token_cases: 0,
    productions: 0,
    recovery_rules: 0,
    snapshots: 0,
    diagnostics: 0
  }
}

fn add_module(total: parser_snapshot, module: parser_module) -> parser_snapshot {
  parser_snapshot {
    modules: total.modules + 1,
    imports: total.imports + module.imports,
    exports: total.exports + module.exports,
    token_cases: total.token_cases + module.token_cases,
    productions: total.productions + module.productions,
    recovery_rules: total.recovery_rules + module.recovery_rules,
    snapshots: total.snapshots + module.snapshots,
    diagnostics: total.diagnostics + module.diagnostics
  }
}

fn summarize_modules(modules: List<parser_module>, total: parser_snapshot) -> parser_snapshot {
  match modules {
    [] -> total,
    [module, ..rest] -> summarize_modules(rest, add_module(total, module))
  }
}

fn review_module(module: parser_module) -> String {
  if module.recovery_rules == 0 {
    if module.diagnostics == 0 {
      string_concat(module.name, ":clean")
    } else {
      string_concat(module.name, ":diagnostics")
    }
  } else {
    string_concat(module.name, ":recovery")
  }
}

fn review_modules(modules: List<parser_module>, reviews: List<String>) -> List<String> {
  match modules {
    [] -> reviews,
    [module, ..rest] -> review_modules(rest, [review_module(module), ..reviews])
  }
}

fn main() {
  let scanner = parser_module { name: "Scanner", imports: 0, exports: 2, token_cases: 8, productions: 0, recovery_rules: 0, snapshots: 1, diagnostics: 0 };
  let ast = parser_module { name: "Ast", imports: 1, exports: 4, token_cases: 0, productions: 6, recovery_rules: 0, snapshots: 2, diagnostics: 0 };
  let parser = parser_module { name: "Parser", imports: 2, exports: 3, token_cases: 8, productions: 9, recovery_rules: 3, snapshots: 3, diagnostics: 1 };
  let recovery = parser_module { name: "Recovery", imports: 2, exports: 1, token_cases: 3, productions: 2, recovery_rules: 5, snapshots: 2, diagnostics: 2 };
  let modules = [scanner, ast, parser, recovery];
  let summary = summarize_modules(modules, empty_snapshot());
  let reviews = review_modules(modules, []);
  dbg(summary.modules);
  dbg(summary.imports);
  dbg(summary.exports);
  dbg(summary.token_cases);
  dbg(summary.productions);
  dbg(summary.recovery_rules);
  dbg(summary.snapshots);
  dbg(summary.diagnostics);
  match reviews {
    [first, .._] -> dbg(first),
    [] -> dbg("no-review")
  }
}
