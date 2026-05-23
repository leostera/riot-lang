type boundary = UnknownPath | EmptyListElement | MissingTupleItem | MissingRecordField | UnknownMatchPattern | ReceiveBinder | FallbackCallSignature | ApplyExpressionResult | LambdaExpressionValue | UnannotatedLambdaParam | SpawnWithoutReceiveShape | PartialReceiveWrapper

type decision = { name: String, policy: String }

fn boundary_name(boundary: boundary) -> String {
  match boundary {
    UnknownPath -> "unknown path",
    EmptyListElement -> "empty list element",
    MissingTupleItem -> "missing tuple item",
    MissingRecordField -> "missing record field",
    UnknownMatchPattern -> "unknown match pattern",
    ReceiveBinder -> "receive binder",
    FallbackCallSignature -> "fallback call signature",
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
    EmptyListElement -> decision { name: boundary_name(boundary), policy: "conservative" },
    MissingTupleItem -> decision { name: boundary_name(boundary), policy: "conservative" },
    MissingRecordField -> decision { name: boundary_name(boundary), policy: "conservative" },
    UnknownMatchPattern -> decision { name: boundary_name(boundary), policy: "conservative" },
    ReceiveBinder -> decision { name: boundary_name(boundary), policy: "conservative" },
    FallbackCallSignature -> decision { name: boundary_name(boundary), policy: "conservative" },
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
  let boundaries = [UnknownPath, EmptyListElement, MissingTupleItem, MissingRecordField, UnknownMatchPattern, ReceiveBinder, FallbackCallSignature, ApplyExpressionResult, LambdaExpressionValue, UnannotatedLambdaParam, SpawnWithoutReceiveShape, PartialReceiveWrapper];
  dbg(render_all(boundaries))
}
