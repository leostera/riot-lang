type backend_module = {
  name: String,
  imports: i64,
  exports: i64,
  lambda_nodes: i64,
  air_ops: i64,
  llvm_blocks: i64,
  runtime_hooks: i64,
  diagnostics: i64
}

type backend_snapshot = {
  modules: i64,
  imports: i64,
  exports: i64,
  lambda_nodes: i64,
  air_ops: i64,
  llvm_blocks: i64,
  runtime_hooks: i64,
  diagnostics: i64
}

fn empty_snapshot() -> backend_snapshot {
  backend_snapshot {
    modules: 0,
    imports: 0,
    exports: 0,
    lambda_nodes: 0,
    air_ops: 0,
    llvm_blocks: 0,
    runtime_hooks: 0,
    diagnostics: 0
  }
}

fn add_module(total: backend_snapshot, module: backend_module) -> backend_snapshot {
  backend_snapshot {
    modules: total.modules + 1,
    imports: total.imports + module.imports,
    exports: total.exports + module.exports,
    lambda_nodes: total.lambda_nodes + module.lambda_nodes,
    air_ops: total.air_ops + module.air_ops,
    llvm_blocks: total.llvm_blocks + module.llvm_blocks,
    runtime_hooks: total.runtime_hooks + module.runtime_hooks,
    diagnostics: total.diagnostics + module.diagnostics
  }
}

fn summarize_modules(modules: List<backend_module>, total: backend_snapshot) -> backend_snapshot {
  match modules {
    [] -> total,
    [module, ..rest] -> summarize_modules(rest, add_module(total, module))
  }
}

fn review_module(module: backend_module) -> String {
  if module.diagnostics == 0 {
    if module.runtime_hooks == 0 {
      string_concat(module.name, ":lowered")
    } else {
      string_concat(module.name, ":runtime")
    }
  } else {
    string_concat(module.name, ":diagnostics")
  }
}

fn review_modules(modules: List<backend_module>, reviews: List<String>) -> List<String> {
  match modules {
    [] -> reviews,
    [module, ..rest] -> review_modules(rest, [review_module(module), ..reviews])
  }
}

fn main() {
  let lambda = backend_module { name: "Lambda", imports: 2, exports: 3, lambda_nodes: 18, air_ops: 0, llvm_blocks: 0, runtime_hooks: 0, diagnostics: 0 };
  let actor = backend_module { name: "ActorLower", imports: 3, exports: 2, lambda_nodes: 7, air_ops: 11, llvm_blocks: 0, runtime_hooks: 2, diagnostics: 0 };
  let llvm = backend_module { name: "LlvmEmit", imports: 4, exports: 2, lambda_nodes: 4, air_ops: 8, llvm_blocks: 13, runtime_hooks: 3, diagnostics: 1 };
  let runtime = backend_module { name: "Runtime", imports: 1, exports: 4, lambda_nodes: 0, air_ops: 2, llvm_blocks: 3, runtime_hooks: 6, diagnostics: 0 };
  let modules = [lambda, actor, llvm, runtime];
  let summary = summarize_modules(modules, empty_snapshot());
  let reviews = review_modules(modules, []);
  dbg(summary.modules);
  dbg(summary.imports);
  dbg(summary.exports);
  dbg(summary.lambda_nodes);
  dbg(summary.air_ops);
  dbg(summary.llvm_blocks);
  dbg(summary.runtime_hooks);
  dbg(summary.diagnostics);
  match reviews {
    [first, .._] -> dbg(first),
    [] -> dbg("no-review")
  }
}
