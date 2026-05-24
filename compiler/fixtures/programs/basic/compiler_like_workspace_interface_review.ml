type module_name =
  | Syntax
  | Parser
  | Checker
  | Driver

type interface_change =
  | Stable(module_name)
  | Changed(module_name)
  | Added(module_name)
  | Removed(module_name)

type dependency = {
  module_name: module_name,
  depends_on: module_name
}

type review = {
  changed_modules: i64,
  added_modules: i64,
  removed_modules: i64,
  impacted_dependents: i64,
  stable_modules: i64,
  needs_review: bool
}

fn empty_review() -> review {
  review { changed_modules: 0, added_modules: 0, removed_modules: 0, impacted_dependents: 0, stable_modules: 0, needs_review: false }
}

fn same_module(left: module_name, right: module_name) -> bool {
  match left {
    Syntax -> match right { Syntax -> true, Parser -> false, Checker -> false, Driver -> false },
    Parser -> match right { Syntax -> false, Parser -> true, Checker -> false, Driver -> false },
    Checker -> match right { Syntax -> false, Parser -> false, Checker -> true, Driver -> false },
    Driver -> match right { Syntax -> false, Parser -> false, Checker -> false, Driver -> true }
  }
}

fn changed_module(change: interface_change) -> module_name {
  match change {
    Stable(module_name) -> module_name,
    Changed(module_name) -> module_name,
    Added(module_name) -> module_name,
    Removed(module_name) -> module_name
  }
}

fn is_changed(change: interface_change) -> bool {
  match change {
    Stable(_) -> false,
    Changed(_) -> true,
    Added(_) -> true,
    Removed(_) -> true
  }
}

fn module_changed(module_name: module_name, changes: List<interface_change>) -> bool {
  match changes {
    [] -> false,
    [change, ..rest] -> if is_changed(change) {
      if same_module(module_name, changed_module(change)) { true } else { module_changed(module_name, rest) }
    } else {
      module_changed(module_name, rest)
    }
  }
}

fn mark_change(total: review, change: interface_change) -> review {
  match change {
    Stable(_) -> review { changed_modules: total.changed_modules, added_modules: total.added_modules, removed_modules: total.removed_modules, impacted_dependents: total.impacted_dependents, stable_modules: total.stable_modules + 1, needs_review: total.needs_review },
    Changed(_) -> review { changed_modules: total.changed_modules + 1, added_modules: total.added_modules, removed_modules: total.removed_modules, impacted_dependents: total.impacted_dependents, stable_modules: total.stable_modules, needs_review: true },
    Added(_) -> review { changed_modules: total.changed_modules, added_modules: total.added_modules + 1, removed_modules: total.removed_modules, impacted_dependents: total.impacted_dependents, stable_modules: total.stable_modules, needs_review: true },
    Removed(_) -> review { changed_modules: total.changed_modules, added_modules: total.added_modules, removed_modules: total.removed_modules + 1, impacted_dependents: total.impacted_dependents, stable_modules: total.stable_modules, needs_review: true }
  }
}

fn review_changes(changes: List<interface_change>, total: review) -> review {
  match changes {
    [] -> total,
    [change, ..rest] -> review_changes(rest, mark_change(total, change))
  }
}

fn review_dependents(edges: List<dependency>, changes: List<interface_change>, total: review) -> review {
  match edges {
    [] -> total,
    [edge, ..rest] -> if module_changed(edge.depends_on, changes) {
      review_dependents(rest, changes, review { changed_modules: total.changed_modules, added_modules: total.added_modules, removed_modules: total.removed_modules, impacted_dependents: total.impacted_dependents + 1, stable_modules: total.stable_modules, needs_review: true })
    } else {
      review_dependents(rest, changes, total)
    }
  }
}

fn main() {
  let changes = [
    Changed(Parser),
    Added(Checker),
    Removed(Syntax),
    Stable(Driver)
  ];
  let dependencies = [
    dependency { module_name: Parser, depends_on: Syntax },
    dependency { module_name: Checker, depends_on: Parser },
    dependency { module_name: Driver, depends_on: Checker }
  ];
  let direct = review_changes(changes, empty_review());
  let summary = review_dependents(dependencies, changes, direct);
  dbg(summary.changed_modules);
  dbg(summary.added_modules);
  dbg(summary.removed_modules);
  dbg(summary.impacted_dependents);
  dbg(summary.stable_modules);
  dbg(summary.needs_review)
}
