type export_change =
  | Stable(String)
  | Changed(String)
  | Added(String)
  | Removed(String)

type review = {
  stable_exports: i64,
  changed_exports: i64,
  added_exports: i64,
  removed_exports: i64,
  needs_review: bool
}

fn mark_stable(total: review) -> review {
  review { stable_exports: total.stable_exports + 1, changed_exports: total.changed_exports, added_exports: total.added_exports, removed_exports: total.removed_exports, needs_review: total.needs_review }
}

fn mark_changed(total: review) -> review {
  review { stable_exports: total.stable_exports, changed_exports: total.changed_exports + 1, added_exports: total.added_exports, removed_exports: total.removed_exports, needs_review: true }
}

fn mark_added(total: review) -> review {
  review { stable_exports: total.stable_exports, changed_exports: total.changed_exports, added_exports: total.added_exports + 1, removed_exports: total.removed_exports, needs_review: true }
}

fn mark_removed(total: review) -> review {
  review { stable_exports: total.stable_exports, changed_exports: total.changed_exports, added_exports: total.added_exports, removed_exports: total.removed_exports + 1, needs_review: true }
}

fn review_export(change: export_change, total: review) -> review {
  match change {
    Stable(_) -> mark_stable(total),
    Changed(_) -> mark_changed(total),
    Added(_) -> mark_added(total),
    Removed(_) -> mark_removed(total)
  }
}

fn review_exports(changes: List<export_change>, total: review) -> review {
  match changes {
    [] -> total,
    [change, ..rest] -> review_exports(rest, review_export(change, total))
  }
}

fn main() {
  let empty = review { stable_exports: 0, changed_exports: 0, added_exports: 0, removed_exports: 0, needs_review: false };
  let changes = [
    Stable("Syntax.token"),
    Changed("Parser.parse"),
    Added("Checker.check"),
    Removed("Driver.old_emit")
  ];
  let summary = review_exports(changes, empty);
  dbg(summary.stable_exports);
  dbg(summary.changed_exports);
  dbg(summary.added_exports);
  dbg(summary.removed_exports);
  dbg(summary.needs_review)
}
