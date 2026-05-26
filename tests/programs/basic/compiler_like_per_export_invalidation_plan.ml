type module_name =
  | Syntax
  | Parser
  | Checker
  | Driver

type export_name =
  | Token
  | Parse
  | Typecheck
  | Pretty

type changed_export = {
  provider: module_name,
  export_name: export_name
}

type dependency_edge = {
  consumer: module_name,
  provider: module_name,
  used_export: export_name
}

type invalidation_plan = {
  module_invalidations: i64,
  per_export_invalidations: i64,
  avoided_by_export_edges: i64,
  stable_edges: i64
}

fn empty_plan() -> invalidation_plan {
  invalidation_plan { module_invalidations: 0, per_export_invalidations: 0, avoided_by_export_edges: 0, stable_edges: 0 }
}

fn same_module(left: module_name, right: module_name) -> bool {
  match left {
    Syntax -> match right { Syntax -> true, Parser -> false, Checker -> false, Driver -> false },
    Parser -> match right { Syntax -> false, Parser -> true, Checker -> false, Driver -> false },
    Checker -> match right { Syntax -> false, Parser -> false, Checker -> true, Driver -> false },
    Driver -> match right { Syntax -> false, Parser -> false, Checker -> false, Driver -> true }
  }
}

fn same_export(left: export_name, right: export_name) -> bool {
  match left {
    Token -> match right { Token -> true, Parse -> false, Typecheck -> false, Pretty -> false },
    Parse -> match right { Token -> false, Parse -> true, Typecheck -> false, Pretty -> false },
    Typecheck -> match right { Token -> false, Parse -> false, Typecheck -> true, Pretty -> false },
    Pretty -> match right { Token -> false, Parse -> false, Typecheck -> false, Pretty -> true }
  }
}

fn provider_changed(provider: module_name, changes: List<changed_export>) -> bool {
  match changes {
    [] -> false,
    [change, ..rest] -> if same_module(provider, change.provider) { true } else { provider_changed(provider, rest) }
  }
}

fn used_export_changed(provider: module_name, export_name: export_name, changes: List<changed_export>) -> bool {
  match changes {
    [] -> false,
    [change, ..rest] -> if same_module(provider, change.provider) {
      if same_export(export_name, change.export_name) { true } else { used_export_changed(provider, export_name, rest) }
    } else {
      used_export_changed(provider, export_name, rest)
    }
  }
}

fn count_edge(edge: dependency_edge, changes: List<changed_export>, total: invalidation_plan) -> invalidation_plan {
  if provider_changed(edge.provider, changes) {
    if used_export_changed(edge.provider, edge.used_export, changes) {
      invalidation_plan {
        module_invalidations: total.module_invalidations + 1,
        per_export_invalidations: total.per_export_invalidations + 1,
        avoided_by_export_edges: total.avoided_by_export_edges,
        stable_edges: total.stable_edges
      }
    } else {
      invalidation_plan {
        module_invalidations: total.module_invalidations + 1,
        per_export_invalidations: total.per_export_invalidations,
        avoided_by_export_edges: total.avoided_by_export_edges + 1,
        stable_edges: total.stable_edges
      }
    }
  } else {
    invalidation_plan {
      module_invalidations: total.module_invalidations,
      per_export_invalidations: total.per_export_invalidations,
      avoided_by_export_edges: total.avoided_by_export_edges,
      stable_edges: total.stable_edges + 1
    }
  }
}

fn plan_edges(edges: List<dependency_edge>, changes: List<changed_export>, total: invalidation_plan) -> invalidation_plan {
  match edges {
    [] -> total,
    [edge, ..rest] -> plan_edges(rest, changes, count_edge(edge, changes, total))
  }
}

fn main() {
  let changes = [
    changed_export { provider: Syntax, export_name: Token },
    changed_export { provider: Parser, export_name: Pretty }
  ];
  let edges = [
    dependency_edge { consumer: Parser, provider: Syntax, used_export: Token },
    dependency_edge { consumer: Checker, provider: Syntax, used_export: Parse },
    dependency_edge { consumer: Driver, provider: Parser, used_export: Pretty },
    dependency_edge { consumer: Driver, provider: Checker, used_export: Typecheck }
  ];
  let plan = plan_edges(edges, changes, empty_plan());
  dbg(plan.module_invalidations);
  dbg(plan.per_export_invalidations);
  dbg(plan.avoided_by_export_edges);
  dbg(plan.stable_edges)
}
