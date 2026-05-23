type annotation = Complete | MissingReturn | MissingParam | Unannotated

type dependency = SelfCall | LaterAnnotated | LaterUnannotated | LaterPartial | LaterValue

type declaration = { name: String, annotation: annotation, dependency: dependency }

type decision = { name: String, policy: String, visibility: String }

fn annotation_complete(annotation: annotation) -> bool {
  match annotation {
    Complete -> true,
    MissingReturn -> false,
    MissingParam -> false,
    Unannotated -> false
  }
}

fn dependency_requires_predeclaration(dependency: dependency) -> bool {
  match dependency {
    SelfCall -> false,
    LaterAnnotated -> true,
    LaterUnannotated -> true,
    LaterPartial -> true,
    LaterValue -> true
  }
}

fn dependency_visible(decl: declaration) -> bool {
  match decl.dependency {
    SelfCall -> annotation_complete(decl.annotation),
    LaterAnnotated -> annotation_complete(decl.annotation),
    LaterUnannotated -> false,
    LaterPartial -> false,
    LaterValue -> false
  }
}

fn predeclaration_policy(decl: declaration) -> String {
  if dependency_visible(decl) {
    "predeclare"
  } else {
    if dependency_requires_predeclaration(decl.dependency) {
      "top-down"
    } else {
      "direct"
    }
  }
}

fn visibility_reason(decl: declaration) -> String {
  match decl.dependency {
    SelfCall -> if annotation_complete(decl.annotation) { "self recursion ok" } else { "direct recursion needs facts" },
    LaterAnnotated -> if annotation_complete(decl.annotation) { "later annotated visible" } else { "caller not fully annotated" },
    LaterUnannotated -> "later unannotated hidden",
    LaterPartial -> "missing complete annotations",
    LaterValue -> "future values hidden"
  }
}

fn decide(decl: declaration) -> decision {
  decision { name: decl.name, policy: predeclaration_policy(decl), visibility: visibility_reason(decl) }
}

fn render(decision: decision) -> String {
  string_concat(decision.name, string_concat(":", string_concat(decision.policy, string_concat(":", decision.visibility))))
}

fn render_all(decls: List<declaration>) -> String {
  match decls {
    [] -> "done",
    [decl] -> render(decide(decl)),
    [decl, ..rest] -> string_concat(render(decide(decl)), string_concat(";", render_all(rest)))
  }
}

fn main() {
  let decls = [
    declaration { name: "direct annotated", annotation: Complete, dependency: SelfCall },
    declaration { name: "annotated mutual", annotation: Complete, dependency: LaterAnnotated },
    declaration { name: "annotated to unannotated", annotation: Complete, dependency: LaterUnannotated },
    declaration { name: "partial missing return", annotation: MissingReturn, dependency: LaterAnnotated },
    declaration { name: "partial missing param", annotation: MissingParam, dependency: LaterAnnotated },
    declaration { name: "unannotated forward", annotation: Unannotated, dependency: LaterValue }
  ];
  dbg(render_all(decls))
}
