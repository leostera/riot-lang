type driver_module = {
  name: String,
  commands: i64,
  artifacts: i64,
  signatures: i64,
  objects: i64,
  diagnostics: i64,
  smoke_tests: i64
}

type driver_snapshot = {
  modules: i64,
  commands: i64,
  artifacts: i64,
  signatures: i64,
  objects: i64,
  diagnostics: i64,
  smoke_tests: i64
}

fn empty_snapshot() -> driver_snapshot {
  driver_snapshot {
    modules: 0,
    commands: 0,
    artifacts: 0,
    signatures: 0,
    objects: 0,
    diagnostics: 0,
    smoke_tests: 0
  }
}

fn add_module(total: driver_snapshot, module: driver_module) -> driver_snapshot {
  driver_snapshot {
    modules: total.modules + 1,
    commands: total.commands + module.commands,
    artifacts: total.artifacts + module.artifacts,
    signatures: total.signatures + module.signatures,
    objects: total.objects + module.objects,
    diagnostics: total.diagnostics + module.diagnostics,
    smoke_tests: total.smoke_tests + module.smoke_tests
  }
}

fn summarize_modules(modules: List<driver_module>, total: driver_snapshot) -> driver_snapshot {
  match modules {
    [] -> total,
    [module, ..rest] -> summarize_modules(rest, add_module(total, module))
  }
}

fn review_module(module: driver_module) -> String {
  if module.diagnostics == 0 {
    string_concat(module.name, ":needs-diagnostics")
  } else {
    if module.smoke_tests < module.commands {
      string_concat(module.name, ":needs-smoke")
    } else {
      string_concat(module.name, ":pinned")
    }
  }
}

fn review_modules(modules: List<driver_module>, reviews: List<String>) -> List<String> {
  match modules {
    [] -> reviews,
    [module, ..rest] -> review_modules(rest, [review_module(module), ..reviews])
  }
}

fn main() {
  let driver = driver_module { name: "Driver", commands: 4, artifacts: 3, signatures: 2, objects: 2, diagnostics: 4, smoke_tests: 4 };
  let signature_store = driver_module { name: "SignatureStore", commands: 1, artifacts: 2, signatures: 7, objects: 0, diagnostics: 3, smoke_tests: 2 };
  let object_resolver = driver_module { name: "ObjectResolver", commands: 1, artifacts: 4, signatures: 3, objects: 5, diagnostics: 3, smoke_tests: 1 };
  let emit_modes = driver_module { name: "EmitModes", commands: 3, artifacts: 4, signatures: 2, objects: 1, diagnostics: 2, smoke_tests: 2 };
  let modules = [driver, signature_store, object_resolver, emit_modes];
  let summary = summarize_modules(modules, empty_snapshot());
  let reviews = review_modules(modules, []);
  dbg(summary.modules);
  dbg(summary.commands);
  dbg(summary.artifacts);
  dbg(summary.signatures);
  dbg(summary.objects);
  dbg(summary.diagnostics);
  dbg(summary.smoke_tests);
  match reviews {
    [first, .._] -> dbg(first),
    [] -> dbg("no-review")
  }
}
