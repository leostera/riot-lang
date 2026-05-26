type pattern = PWildcard | PBind(String) | PTuple(List<pattern>) | PRecord(List<field_pattern>) | PConstructor(String, List<pattern>) | PInt(i64) | PList(List<pattern>) | PListTail(pattern)
type field_pattern = { name: String, pattern: pattern }

fn irrefutable(pattern: pattern) -> bool {
  match pattern {
    PWildcard -> true,
    PBind(_) -> true,
    PTuple(items) -> all_irrefutable(items),
    PRecord(fields) -> all_fields_irrefutable(fields),
    PListTail(tail) -> irrefutable(tail),
    PConstructor(_, _) -> false,
    PInt(_) -> false,
    PList(_) -> false
  }
}

fn all_irrefutable(patterns: List<pattern>) -> bool {
  match patterns {
    [] -> true,
    [head, ..tail] -> irrefutable(head) && all_irrefutable(tail)
  }
}

fn all_fields_irrefutable(fields: List<field_pattern>) -> bool {
  match fields {
    [] -> true,
    [head, ..tail] -> irrefutable(head.pattern) && all_fields_irrefutable(tail)
  }
}

fn render(value: bool) -> String {
  if value { "irrefutable" } else { "refutable" }
}

fn main() {
  let structural = PConstructor("Box", [PTuple([PBind("line"), PRecord([field_pattern { name: "text", pattern: PBind("text") }])])]);
  let literal = PTuple([PBind("line"), PInt(0)]);
  let tail = PListTail(PBind("items"));
  dbg(string_concat(render(all_irrefutable([PTuple([PBind("a"), PWildcard]), PRecord([field_pattern { name: "b", pattern: PBind("b") }])])), string_concat("; ", string_concat(render(irrefutable(structural)), string_concat("; ", string_concat(render(irrefutable(literal)), string_concat("; ", render(irrefutable(tail)))))))))
}
