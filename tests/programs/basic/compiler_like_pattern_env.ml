type pat_type = TInt | TString | TTuple(List<pat_type>) | TVariant(String)
type pattern = PBind(String) | PTuple(List<pattern>) | PConstructor(String, List<pattern>) | PWildcard
type binding = { name: String, type_name: String }
type constructor_shape = { name: String, payload: List<pat_type> }

fn type_name(type_: pat_type) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TTuple(_) -> "tuple",
    TVariant(name) -> name
  }
}

fn payload_type(index: i64, payload: List<pat_type>) -> pat_type {
  match payload {
    [] -> TString,
    [type_, ..rest] -> if index == 0 { type_ } else { payload_type(index - 1, rest) }
  }
}

fn bind_pattern(pattern: pattern, expected: pat_type, env: List<binding>) -> List<binding> {
  match pattern {
    PWildcard -> env,
    PBind(name) -> [binding { name: name, type_name: type_name(expected) }, ..env],
    PTuple(items) -> match expected {
      TTuple(types) -> bind_tuple(items, types, env),
      _ -> env
    },
    PConstructor(_, payload) -> match expected {
      TVariant(_) -> bind_payload(payload, [TInt, TTuple([TInt, TString])], env),
      _ -> env
    }
  }
}

fn bind_tuple(items: List<pattern>, types: List<pat_type>, env: List<binding>) -> List<binding> {
  match items {
    [] -> env,
    [item, ..rest] -> {
      let item_type = payload_type(0, types);
      bind_tuple(rest, tail_type(types), bind_pattern(item, item_type, env))
    }
  }
}

fn bind_payload(items: List<pattern>, types: List<pat_type>, env: List<binding>) -> List<binding> {
  match items {
    [] -> env,
    [item, ..rest] -> bind_payload(rest, tail_type(types), bind_pattern(item, payload_type(0, types), env))
  }
}

fn tail_type(types: List<pat_type>) -> List<pat_type> {
  match types {
    [] -> [],
    [_] -> [],
    [_, ..rest] -> rest
  }
}

fn render(env: List<binding>) -> String {
  match env {
    [] -> "done",
    [item] -> string_concat(item.name, string_concat(":", item.type_name)),
    [item, ..rest] -> string_concat(item.name, string_concat(":", string_concat(item.type_name, string_concat(", ", render(rest)))))
  }
}

fn main() {
  let pattern = PConstructor("Token", [PBind("kind"), PTuple([PBind("line"), PBind("text")])]);
  let env = bind_pattern(pattern, TVariant("token"), []);
  dbg(render(env))
}
