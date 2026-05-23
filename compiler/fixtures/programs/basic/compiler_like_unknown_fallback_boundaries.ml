type boundary = UnknownPath | UnknownCallCallee | EmptyListElement | MissingTupleItem | MissingRecordField | UnknownMatchPattern | IncompatibleIfBranches | IncompatibleMatchArms | ReceiveBinder | FallbackCallSignature | MissingFunctionSignature | LocalBindingFacts | OperatorFacts | AggregateLiteralFacts | ConstructorFacts | ImportedAggregateFacts | ImportedConstructorFacts | NamedSignatureFacts | ImportedSignatureFacts | ExternalSignatureFacts | FunctionAnnotationFacts | LambdaAnnotationFacts | LetAnnotationFacts | ReceivePatternShapeFacts | ApplyExpressionResult | LambdaExpressionValue | UnannotatedLambdaParam | SpawnWithoutReceiveShape | PartialReceiveWrapper

type decision = { name: String, policy: String }

fn boundary_name(boundary: boundary) -> String {
  match boundary {
    UnknownPath -> "unknown path",
    UnknownCallCallee -> "unknown call callee",
    EmptyListElement -> "empty list element",
    MissingTupleItem -> "missing tuple item",
    MissingRecordField -> "missing record field",
    UnknownMatchPattern -> "unknown match pattern",
    IncompatibleIfBranches -> "incompatible if branches",
    IncompatibleMatchArms -> "incompatible match arms",
    ReceiveBinder -> "receive binder",
    FallbackCallSignature -> "fallback call signature",
    MissingFunctionSignature -> "missing function signature",
    LocalBindingFacts -> "local binding facts",
    OperatorFacts -> "operator facts",
    AggregateLiteralFacts -> "aggregate literal facts",
    ConstructorFacts -> "constructor facts",
    ImportedAggregateFacts -> "imported aggregate facts",
    ImportedConstructorFacts -> "imported constructor facts",
    NamedSignatureFacts -> "named signature facts",
    ImportedSignatureFacts -> "imported signature facts",
    ExternalSignatureFacts -> "external signature facts",
    FunctionAnnotationFacts -> "function annotation facts",
    LambdaAnnotationFacts -> "lambda annotation facts",
    LetAnnotationFacts -> "let annotation facts",
    ReceivePatternShapeFacts -> "receive pattern shape facts",
    ApplyExpressionResult -> "apply expression result",
    LambdaExpressionValue -> "lambda expression value",
    UnannotatedLambdaParam -> "unannotated lambda param",
    SpawnWithoutReceiveShape -> "spawn without receive shape",
    PartialReceiveWrapper -> "partial receive wrapper"
  }
}

fn classify(boundary: boundary) -> decision {
  match boundary {
    UnknownPath -> decision { name: boundary_name(boundary), policy: "conservative" },
    UnknownCallCallee -> decision { name: boundary_name(boundary), policy: "conservative apply" },
    EmptyListElement -> decision { name: boundary_name(boundary), policy: "conservative" },
    MissingTupleItem -> decision { name: boundary_name(boundary), policy: "conservative" },
    MissingRecordField -> decision { name: boundary_name(boundary), policy: "conservative" },
    UnknownMatchPattern -> decision { name: boundary_name(boundary), policy: "conservative" },
    IncompatibleIfBranches -> decision { name: boundary_name(boundary), policy: "conservative result" },
    IncompatibleMatchArms -> decision { name: boundary_name(boundary), policy: "conservative result" },
    ReceiveBinder -> decision { name: boundary_name(boundary), policy: "conservative" },
    FallbackCallSignature -> decision { name: boundary_name(boundary), policy: "conservative" },
    MissingFunctionSignature -> decision { name: boundary_name(boundary), policy: "conservative params/results" },
    LocalBindingFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    OperatorFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    AggregateLiteralFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    ConstructorFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    ImportedAggregateFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    ImportedConstructorFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    NamedSignatureFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    ImportedSignatureFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    ExternalSignatureFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    FunctionAnnotationFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    LambdaAnnotationFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    LetAnnotationFacts -> decision { name: boundary_name(boundary), policy: "concrete without expression facts" },
    ReceivePatternShapeFacts -> decision { name: boundary_name(boundary), policy: "concrete without message facts" },
    ApplyExpressionResult -> decision { name: boundary_name(boundary), policy: "needs inference facts" },
    LambdaExpressionValue -> decision { name: boundary_name(boundary), policy: "needs arrow facts" },
    UnannotatedLambdaParam -> decision { name: boundary_name(boundary), policy: "conservative" },
    SpawnWithoutReceiveShape -> decision { name: boundary_name(boundary), policy: "conservative actor id" },
    PartialReceiveWrapper -> decision { name: boundary_name(boundary), policy: "merge nested facts" }
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
  let boundaries = [UnknownPath, UnknownCallCallee, EmptyListElement, MissingTupleItem, MissingRecordField, UnknownMatchPattern, IncompatibleIfBranches, IncompatibleMatchArms, ReceiveBinder, FallbackCallSignature, MissingFunctionSignature, LocalBindingFacts, OperatorFacts, AggregateLiteralFacts, ConstructorFacts, ImportedAggregateFacts, ImportedConstructorFacts, NamedSignatureFacts, ImportedSignatureFacts, ExternalSignatureFacts, FunctionAnnotationFacts, LambdaAnnotationFacts, LetAnnotationFacts, ReceivePatternShapeFacts, ApplyExpressionResult, LambdaExpressionValue, UnannotatedLambdaParam, SpawnWithoutReceiveShape, PartialReceiveWrapper];
  dbg(render_all(boundaries))
}
