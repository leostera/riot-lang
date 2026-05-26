type checker_module = {
  name: String,
  imports: i64,
  exports: i64,
  declarations: i64,
  constraints: i64,
  substitutions: i64,
  lowered_nodes: i64,
  diagnostics: i64
}

type checker_snapshot = {
  modules: i64,
  imports: i64,
  exports: i64,
  declarations: i64,
  constraints: i64,
  substitutions: i64,
  lowered_nodes: i64,
  diagnostics: i64
}

fn empty_snapshot() -> checker_snapshot {
  checker_snapshot {
    modules: 0,
    imports: 0,
    exports: 0,
    declarations: 0,
    constraints: 0,
    substitutions: 0,
    lowered_nodes: 0,
    diagnostics: 0
  }
}

fn add_module(total: checker_snapshot, module: checker_module) -> checker_snapshot {
  checker_snapshot {
    modules: total.modules + 1,
    imports: total.imports + module.imports,
    exports: total.exports + module.exports,
    declarations: total.declarations + module.declarations,
    constraints: total.constraints + module.constraints,
    substitutions: total.substitutions + module.substitutions,
    lowered_nodes: total.lowered_nodes + module.lowered_nodes,
    diagnostics: total.diagnostics + module.diagnostics
  }
}

fn summarize_modules(modules: List<checker_module>, total: checker_snapshot) -> checker_snapshot {
  match modules {
    [] -> total,
    [module, ..rest] -> summarize_modules(rest, add_module(total, module))
  }
}

fn review_module(module: checker_module) -> String {
  if module.diagnostics == 0 {
    if module.lowered_nodes == 0 {
      string_concat(module.name, ":types-only")
    } else {
      string_concat(module.name, ":lowered")
    }
  } else {
    string_concat(module.name, ":diagnostics")
  }
}

fn review_modules(modules: List<checker_module>, reviews: List<String>) -> List<String> {
  match modules {
    [] -> reviews,
    [module, ..rest] -> review_modules(rest, [review_module(module), ..reviews])
  }
}

fn main() {
  let resolver = checker_module { name: "Resolver", imports: 1, exports: 2, declarations: 7, constraints: 0, substitutions: 0, lowered_nodes: 0, diagnostics: 1 };
  let infer = checker_module { name: "Infer", imports: 2, exports: 3, declarations: 5, constraints: 14, substitutions: 9, lowered_nodes: 0, diagnostics: 0 };
  let checker = checker_module { name: "Checker", imports: 2, exports: 2, declarations: 6, constraints: 8, substitutions: 4, lowered_nodes: 5, diagnostics: 2 };
  let lowering = checker_module { name: "Lowering", imports: 3, exports: 2, declarations: 4, constraints: 3, substitutions: 2, lowered_nodes: 11, diagnostics: 0 };
  let modules = [resolver, infer, checker, lowering];
  let summary = summarize_modules(modules, empty_snapshot());
  let reviews = review_modules(modules, []);
  dbg(summary.modules);
  dbg(summary.imports);
  dbg(summary.exports);
  dbg(summary.declarations);
  dbg(summary.constraints);
  dbg(summary.substitutions);
  dbg(summary.lowered_nodes);
  dbg(summary.diagnostics);
  match reviews {
    [first, .._] -> dbg(first),
    [] -> dbg("no-review")
  }
}
