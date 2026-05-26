type typ = TUnknown | TInt | TString | TValue | TVariant(String, List<typ>)
type binding = { name: String, type_: typ }
type actor_slot = { name: String, type_: typ }

fn render_type(type_: typ) -> String {
  match type_ {
    TUnknown -> "Unknown",
    TInt -> "i64",
    TString -> "String",
    TValue -> "Value",
    TVariant(name, args) -> string_concat(name, render_args(args))
  }
}

fn render_args(args: List<typ>) -> String {
  match args {
    [] -> "",
    [arg] -> string_concat("<", string_concat(render_type(arg), ">")),
    [arg, ..rest] -> string_concat("<", string_concat(render_type(arg), string_concat(",", string_concat(render_arg_tail(rest), ">"))))
  }
}

fn render_arg_tail(args: List<typ>) -> String {
  match args {
    [] -> "",
    [arg] -> render_type(arg),
    [arg, ..rest] -> string_concat(render_type(arg), string_concat(",", render_arg_tail(rest)))
  }
}

fn refine_binding(binding: binding, observed: typ) -> binding {
  match binding.type_ {
    TUnknown -> binding { name: binding.name, type_: observed },
    TValue -> binding,
    TInt -> binding,
    TString -> binding,
    TVariant(_, _) -> binding
  }
}

fn bind_receive_payload(name: String, message: typ) -> binding {
  match message {
    TVariant(_, [payload]) -> binding { name: name, type_: payload },
    _ -> binding { name: name, type_: TUnknown }
  }
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
  let message = TVariant("option", [TUnknown]);
  let initial = bind_receive_payload("value", message);
  let refined = refine_binding(initial, TInt);
  let slot = lower_capture(refined);
  dbg(render_binding("receive", initial));
  dbg(render_binding("lambda", refined));
  dbg(render_slot(slot))
}
