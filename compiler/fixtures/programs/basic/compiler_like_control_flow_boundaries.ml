type control_flow = Recursion | WhileLoop | MatchBranch | ActorReceive

type decision = { feature: String, status: String, reason: String }

fn decide(feature: control_flow) -> decision {
  match feature {
    Recursion -> decision { feature: "recursion", status: "supported", reason: "lowered as calls" },
    WhileLoop -> decision { feature: "while", status: "supported", reason: "parses to loop blocks" },
    MatchBranch -> decision { feature: "match", status: "supported", reason: "lowers to branches" },
    ActorReceive -> decision { feature: "receive", status: "supported", reason: "actor scheduler boundary" }
  }
}

fn render(decision: decision) -> String {
  string_concat(decision.feature, string_concat(":", string_concat(decision.status, string_concat(":", decision.reason))))
}

fn render_all(items: List<control_flow>) -> String {
  match items {
    [] -> "done",
    [item] -> render(decide(item)),
    [item, ..rest] -> string_concat(render(decide(item)), string_concat(";", render_all(rest)))
  }
}

fn main() {
  dbg(render_all([Recursion, WhileLoop, MatchBranch, ActorReceive]))
}
