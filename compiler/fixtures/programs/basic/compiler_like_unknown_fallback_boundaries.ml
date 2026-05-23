type boundary = UnknownPath | EmptyListElement | MissingTupleItem | MissingRecordField | ReceiveBinder | PartialReceiveWrapper

type decision = { name: String, policy: String }

fn boundary_name(boundary: boundary) -> String {
  match boundary {
    UnknownPath -> "unknown path",
    EmptyListElement -> "empty list element",
    MissingTupleItem -> "missing tuple item",
    MissingRecordField -> "missing record field",
    ReceiveBinder -> "receive binder",
    PartialReceiveWrapper -> "partial receive wrapper"
  }
}

fn classify(boundary: boundary) -> decision {
  match boundary {
    UnknownPath -> decision { name: boundary_name(boundary), policy: "conservative" },
    EmptyListElement -> decision { name: boundary_name(boundary), policy: "conservative" },
    MissingTupleItem -> decision { name: boundary_name(boundary), policy: "conservative" },
    MissingRecordField -> decision { name: boundary_name(boundary), policy: "conservative" },
    ReceiveBinder -> decision { name: boundary_name(boundary), policy: "conservative" },
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
  let boundaries = [UnknownPath, EmptyListElement, MissingTupleItem, MissingRecordField, ReceiveBinder, PartialReceiveWrapper];
  dbg(render_all(boundaries))
}
