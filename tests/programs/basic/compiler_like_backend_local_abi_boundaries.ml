type boundary = RawUnknownParam | UnresolvedPath | UnresolvedCall | WildcardMatchScrutinee | IncompatibleMatchScrutinee | IncompatibleMatchResult | ReceiveExpression | LocalFunctionScalarLiteral | EntrypointScalarLiteral | UnknownExternalAbi | CodegenUnknownGuard | UnknownAggregateItem | LetBoundAggregateItem | ConcreteAggregateItem | ApplyUnknownOperand | IfPredicate | CallSignatureFact

type decision = { name: String, policy: String }

fn boundary_name(boundary: boundary) -> String {
  match boundary {
    RawUnknownParam -> "raw unknown param",
    UnresolvedPath -> "unresolved path",
    UnresolvedCall -> "unresolved call",
    WildcardMatchScrutinee -> "wildcard match scrutinee",
    IncompatibleMatchScrutinee -> "incompatible match scrutinee",
    IncompatibleMatchResult -> "incompatible match result",
    ReceiveExpression -> "receive expression",
    LocalFunctionScalarLiteral -> "local function scalar literal",
    EntrypointScalarLiteral -> "entrypoint scalar literal",
    UnknownExternalAbi -> "unknown external abi",
    CodegenUnknownGuard -> "codegen unknown guard",
    UnknownAggregateItem -> "unknown aggregate item",
    LetBoundAggregateItem -> "let-bound aggregate item",
    ConcreteAggregateItem -> "concrete aggregate item",
    ApplyUnknownOperand -> "apply unknown operand",
    IfPredicate -> "if predicate",
    CallSignatureFact -> "call signature fact"
  }
}

fn classify(boundary: boundary) -> decision {
  match boundary {
    RawUnknownParam -> decision { name: boundary_name(boundary), policy: "unsupported" },
    UnresolvedPath -> decision { name: boundary_name(boundary), policy: "conservative unknown" },
    UnresolvedCall -> decision { name: boundary_name(boundary), policy: "conservative unknown" },
    WildcardMatchScrutinee -> decision { name: boundary_name(boundary), policy: "conservative unknown" },
    IncompatibleMatchScrutinee -> decision { name: boundary_name(boundary), policy: "conservative unknown" },
    IncompatibleMatchResult -> decision { name: boundary_name(boundary), policy: "unsupported unknown" },
    ReceiveExpression -> decision { name: boundary_name(boundary), policy: "unsupported local abi" },
    LocalFunctionScalarLiteral -> decision { name: boundary_name(boundary), policy: "unsupported local abi" },
    EntrypointScalarLiteral -> decision { name: boundary_name(boundary), policy: "static output value" },
    UnknownExternalAbi -> decision { name: boundary_name(boundary), policy: "boxed value bridge" },
    CodegenUnknownGuard -> decision { name: boundary_name(boundary), policy: "diagnostic only" },
    UnknownAggregateItem -> decision { name: boundary_name(boundary), policy: "refine boxed value" },
    LetBoundAggregateItem -> decision { name: boundary_name(boundary), policy: "refine through let" },
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
  let boundaries = [RawUnknownParam, UnresolvedPath, UnresolvedCall, WildcardMatchScrutinee, IncompatibleMatchScrutinee, IncompatibleMatchResult, ReceiveExpression, LocalFunctionScalarLiteral, EntrypointScalarLiteral, UnknownExternalAbi, CodegenUnknownGuard, UnknownAggregateItem, LetBoundAggregateItem, ConcreteAggregateItem, ApplyUnknownOperand, IfPredicate, CallSignatureFact];
  dbg(render_all(boundaries))
}
