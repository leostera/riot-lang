type module_shape = {
  name: String,
  imports: i64,
  exports: i64,
  typed_nodes: i64,
  actor_nodes: i64,
  diagnostics: i64
}

type snapshot = {
  modules: i64,
  imports: i64,
  exports: i64,
  typed_nodes: i64,
  actor_nodes: i64,
  diagnostics: i64
}

fn empty_snapshot() -> snapshot {
  snapshot { modules: 0, imports: 0, exports: 0, typed_nodes: 0, actor_nodes: 0, diagnostics: 0 }
}

fn add_module(total: snapshot, module: module_shape) -> snapshot {
  snapshot {
    modules: total.modules + 1,
    imports: total.imports + module.imports,
    exports: total.exports + module.exports,
    typed_nodes: total.typed_nodes + module.typed_nodes,
    actor_nodes: total.actor_nodes + module.actor_nodes,
    diagnostics: total.diagnostics + module.diagnostics
  }
}

fn summarize_modules(modules: List<module_shape>, total: snapshot) -> snapshot {
  match modules {
    [] -> total,
    [module, ..rest] -> summarize_modules(rest, add_module(total, module))
  }
}

fn review_module(module: module_shape) -> String {
  if module.diagnostics == 0 {
    string_concat(module.name, ":clean")
  } else {
    string_concat(module.name, ":needs-review")
  }
}

fn review_modules(modules: List<module_shape>, reviews: List<String>) -> List<String> {
  match modules {
    [] -> reviews,
    [module, ..rest] -> review_modules(rest, [review_module(module), ..reviews])
  }
}

fn main() {
  let syntax = module_shape { name: "Syntax", imports: 0, exports: 2, typed_nodes: 8, actor_nodes: 0, diagnostics: 0 };
  let analyze = module_shape { name: "Analyze", imports: 1, exports: 1, typed_nodes: 13, actor_nodes: 0, diagnostics: 0 };
  let worker = module_shape { name: "Worker", imports: 2, exports: 1, typed_nodes: 11, actor_nodes: 3, diagnostics: 0 };
  let broken = module_shape { name: "BrokenParser", imports: 1, exports: 0, typed_nodes: 4, actor_nodes: 0, diagnostics: 2 };
  let modules = [syntax, analyze, worker, broken];
  let summary = summarize_modules(modules, empty_snapshot());
  let reviews = review_modules(modules, []);
  dbg(summary.modules);
  dbg(summary.imports);
  dbg(summary.exports);
  dbg(summary.typed_nodes);
  dbg(summary.actor_nodes);
  dbg(summary.diagnostics);
  match reviews {
    [first, .._] -> dbg(first),
    [] -> dbg("no-review")
  }
}
