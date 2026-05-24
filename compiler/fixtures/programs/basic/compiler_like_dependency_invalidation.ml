type dependency = {
  consumer: String,
  provider: String,
  module_changed: bool,
  used_export_changed: bool
}

type invalidation = {
  module_invalidations: i64,
  per_export_invalidations: i64,
  avoided_rebuilds: i64,
  stable_edges: i64
}

fn count_edge(edge: dependency, total: invalidation) -> invalidation {
  if edge.module_changed {
    if edge.used_export_changed {
      invalidation {
        module_invalidations: total.module_invalidations + 1,
        per_export_invalidations: total.per_export_invalidations + 1,
        avoided_rebuilds: total.avoided_rebuilds,
        stable_edges: total.stable_edges
      }
    } else {
      invalidation {
        module_invalidations: total.module_invalidations + 1,
        per_export_invalidations: total.per_export_invalidations,
        avoided_rebuilds: total.avoided_rebuilds + 1,
        stable_edges: total.stable_edges
      }
    }
  } else {
    invalidation {
      module_invalidations: total.module_invalidations,
      per_export_invalidations: total.per_export_invalidations,
      avoided_rebuilds: total.avoided_rebuilds,
      stable_edges: total.stable_edges + 1
    }
  }
}

fn count_edges(edges: List<dependency>, total: invalidation) -> invalidation {
  match edges {
    [] -> total,
    [edge, ..rest] -> count_edges(rest, count_edge(edge, total))
  }
}

fn main() {
  let empty = invalidation { module_invalidations: 0, per_export_invalidations: 0, avoided_rebuilds: 0, stable_edges: 0 };
  let edges = [
    dependency { consumer: "Parser", provider: "Syntax", module_changed: true, used_export_changed: true },
    dependency { consumer: "Checker", provider: "Syntax", module_changed: true, used_export_changed: false },
    dependency { consumer: "Driver", provider: "Emit", module_changed: false, used_export_changed: false }
  ];
  let summary = count_edges(edges, empty);
  dbg(summary.module_invalidations);
  dbg(summary.per_export_invalidations);
  dbg(summary.avoided_rebuilds);
  dbg(summary.stable_edges)
}
