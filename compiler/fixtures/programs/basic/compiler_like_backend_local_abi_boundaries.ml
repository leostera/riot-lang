type boundary = RawUnknownParam | UnresolvedCall | WildcardMatchScrutinee | IncompatibleMatchResult | ReceiveExpression | UnsupportedScalarLiteral | UnknownExternalAbi | UnknownAggregateItem | ConcreteAggregateItem | ApplyUnknownOperand | IfPredicate | CallSignatureFact

type decision = { name: String, policy: String }

fn boundary_name(boundary: boundary) -> String {
  match boundary {
    RawUnknownParam -> "raw unknown param",
    UnresolvedCall -> "unresolved call",
    WildcardMatchScrutinee -> "wildcard match scrutinee",
    IncompatibleMatchResult -> "incompatible match result",
    ReceiveExpression -> "receive expression",
    UnsupportedScalarLiteral -> "unsupported scalar literal",
    UnknownExternalAbi -> "unknown external abi",
    UnknownAggregateItem -> "unknown aggregate item",
    ConcreteAggregateItem -> "concrete aggregate item",
    ApplyUnknownOperand -> "apply unknown operand",
    IfPredicate -> "if predicate",
    CallSignatureFact -> "call signature fact"
  }
}

fn classify(boundary: boundary) -> decision {
  match boundary {
    RawUnknownParam -> decision { name: boundary_name(boundary), policy: "unsupported" },
    UnresolvedCall -> decision { name: boundary_name(boundary), policy: "conservative unknown" },
    WildcardMatchScrutinee -> decision { name: boundary_name(boundary), policy: "conservative unknown" },
    IncompatibleMatchResult -> decision { name: boundary_name(boundary), policy: "unsupported unknown" },
    ReceiveExpression -> decision { name: boundary_name(boundary), policy: "unsupported local abi" },
    UnsupportedScalarLiteral -> decision { name: boundary_name(boundary), policy: "unsupported local abi" },
    UnknownExternalAbi -> decision { name: boundary_name(boundary), policy: "diagnostic" },
    UnknownAggregateItem -> decision { name: boundary_name(boundary), policy: "refine boxed value" },
    ConcreteAggregateItem -> decision { name: boundary_name(boundary), policy: "preserve concrete abi" },
    ApplyUnknownOperand -> decision { name: boundary_name(boundary), policy: "refine boxed value" },
    IfPredicate -> decision { name: boundary_name(boundary), policy: "refine bool" },
    CallSignatureFact -> decision { name: boundary_name(boundary), policy: "refine from signature" }
  }
}

fn render(decision: decision) -> String {
  string_concat(decision.name, string_concat(":", decision.policy))
}

fn render_all(boundaries: List<boundary>) -> String {
  match boundaries {
    [] -> "done",
    [boundary] -> render(classify(boundary)),
    [boundary, ..rest] -> string_concat(render(classify(boundary)), string_concat(";", render_all(rest)))
  }
}

fn main() {
  let boundaries = [RawUnknownParam, UnresolvedCall, WildcardMatchScrutinee, IncompatibleMatchResult, ReceiveExpression, UnsupportedScalarLiteral, UnknownExternalAbi, UnknownAggregateItem, ConcreteAggregateItem, ApplyUnknownOperand, IfPredicate, CallSignatureFact];
  dbg(render_all(boundaries))
}
