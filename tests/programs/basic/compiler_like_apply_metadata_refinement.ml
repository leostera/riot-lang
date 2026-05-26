type typ = TUnknown | TInt | TString | TArrow(typ, typ)
type binding = { name: String, type_: typ }
type actor_slot = { name: String, type_: typ }

fn render_type(type_: typ) -> String {
  match type_ {
    TUnknown -> "Unknown",
    TInt -> "i64",
    TString -> "String",
    TArrow(parameter, result) -> string_concat("fn(", string_concat(render_type(parameter), string_concat(") -> ", render_type(result))))
  }
}

fn infer_apply(callee: typ, fallback: typ) -> typ {
  match callee {
    TArrow(_, result) -> result,
    _ -> fallback
  }
}

fn lower_binding(name: String, type_: typ) -> binding {
  binding { name: name, type_: type_ }
}

fn lower_capture(binding: binding) -> actor_slot {
  actor_slot { name: binding.name, type_: binding.type_ }
}

fn render_binding(phase: String, binding: binding) -> String {
  string_concat(phase, string_concat(":", string_concat(binding.name, string_concat(":", render_type(binding.type_)))))
}

fn render_slot(slot: actor_slot) -> String {
  string_concat("actor:", string_concat(slot.name, string_concat(":", render_type(slot.type_))))
}

fn main() {
  let inc_type = TArrow(TInt, TInt);
  let result = infer_apply(inc_type, TUnknown);
  let binding = lower_binding("y", result);
  let slot = lower_capture(binding);
  dbg(string_concat("apply:", render_type(result)));
  dbg(render_binding("lambda", binding));
  dbg(render_slot(slot))
}
