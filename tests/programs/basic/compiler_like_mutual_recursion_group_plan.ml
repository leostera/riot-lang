type annotation = Complete | ParamOnly | ReturnOnly | Unannotated

type constraint_shape = BoolBranch | NumericCall | LaterCallSite | MissingParamFact | MismatchedReturn

type group_case = { name: String, annotation: annotation, constraint: constraint_shape }

type group_decision = { name: String, action: String, reason: String }

fn has_param_facts(annotation: annotation) -> bool {
  match annotation {
    Complete -> true,
    ParamOnly -> true,
    ReturnOnly -> false,
    Unannotated -> false
  }
}

fn has_return_facts(annotation: annotation) -> bool {
  match annotation {
    Complete -> true,
    ParamOnly -> false,
    ReturnOnly -> true,
    Unannotated -> false
  }
}

fn constraint_supplies_params(constraint: constraint_shape) -> bool {
  match constraint {
    BoolBranch -> true,
    NumericCall -> true,
    LaterCallSite -> true,
    MissingParamFact -> false,
    MismatchedReturn -> true
  }
}

fn constraint_consistent(constraint: constraint_shape) -> bool {
  match constraint {
    BoolBranch -> true,
    NumericCall -> true,
    LaterCallSite -> true,
    MissingParamFact -> true,
    MismatchedReturn -> false
  }
}

fn can_seed_group(case: group_case) -> bool {
  if has_param_facts(case.annotation) {
    true
  } else {
    constraint_supplies_params(case.constraint)
  }
}

fn can_finish_group(case: group_case) -> bool {
  if has_return_facts(case.annotation) {
    constraint_consistent(case.constraint)
  } else {
    match case.constraint {
      BoolBranch -> true,
      NumericCall -> true,
      LaterCallSite -> true,
      MissingParamFact -> false,
      MismatchedReturn -> false
    }
  }
}

fn decide(case: group_case) -> group_decision {
  if can_seed_group(case) {
    if can_finish_group(case) {
      group_decision { name: case.name, action: "infer group", reason: "seed placeholders and solve constraints" }
    } else {
      group_decision { name: case.name, action: "reject group", reason: "return constraints do not converge" }
    }
  } else {
    group_decision { name: case.name, action: "keep diagnostic", reason: "missing parameter facts for predeclaration" }
  }
}

fn render(decision: group_decision) -> String {
  string_concat(decision.name, string_concat(":", string_concat(decision.action, string_concat(":", decision.reason))))
}

fn render_all(cases: List<group_case>) -> String {
  match cases {
    [] -> "done",
    [case] -> render(decide(case)),
    [case, ..rest] -> string_concat(render(decide(case)), string_concat(";", render_all(rest)))
  }
}

fn main() {
  let cases = [
    group_case { name: "annotated even odd", annotation: Complete, constraint: BoolBranch },
    group_case { name: "param annotated numeric helpers", annotation: ParamOnly, constraint: NumericCall },
    group_case { name: "unannotated but constrained", annotation: Unannotated, constraint: NumericCall },
    group_case { name: "later call-site constrained", annotation: Unannotated, constraint: LaterCallSite },
    group_case { name: "missing param facts", annotation: ReturnOnly, constraint: MissingParamFact },
    group_case { name: "mismatched returns", annotation: ParamOnly, constraint: MismatchedReturn }
  ];
  dbg(render_all(cases))
}
