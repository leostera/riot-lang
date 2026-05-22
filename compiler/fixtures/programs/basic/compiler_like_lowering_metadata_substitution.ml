type typ = TInt | TString | TValue | TRecord(String, List<typ>) | TVariant(String, List<typ>)
type binding = { phase: String, name: String, type_: typ }

fn render_type(type_: typ) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TValue -> "Value",
    TRecord(name, args) -> string_concat(name, render_args(args)),
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

fn substitute(type_: typ, param: String, replacement: typ) -> typ {
  match type_ {
    TInt -> TInt,
    TString -> TString,
    TValue -> TValue,
    TRecord(name, args) -> if name == param { replacement } else { TRecord(name, substitute_list(args, param, replacement)) },
    TVariant(name, args) -> if name == param { replacement } else { TVariant(name, substitute_list(args, param, replacement)) }
  }
}

fn substitute_list(args: List<typ>, param: String, replacement: typ) -> List<typ> {
  match args {
    [] -> [],
    [arg, ..rest] -> [substitute(arg, param, replacement), ..substitute_list(rest, param, replacement)]
  }
}

fn lower_binding(phase: String, name: String, type_: typ) -> binding {
  binding { phase: phase, name: name, type_: type_ }
}

fn render_binding(binding: binding) -> String {
  string_concat(binding.phase, string_concat(":", string_concat(binding.name, string_concat(":", render_type(binding.type_)))))
}

fn main() {
  let payload = TRecord("box", [TRecord("a", [])]);
  let concrete = substitute(payload, "a", TInt);
  let lambda = lower_binding("lambda", "value", concrete);
  let air = lower_binding("air", "item", TValue);
  let abi = lower_binding("abi", "option", TValue);
  dbg(render_binding(lambda));
  dbg(render_binding(air));
  dbg(render_binding(abi))
}
