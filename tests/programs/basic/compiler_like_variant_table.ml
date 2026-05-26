type constructor =
  | Nullary(String)
  | Payload(String, String)

type entry = { name: String, arity: i64 }

fn constructor_entry(constructor: constructor) -> entry {
  match constructor {
    Nullary(name) -> entry { name: name, arity: 0 },
    Payload(name, _) -> entry { name: name, arity: 1 }
  }
}

fn constructor_table(constructors: List<constructor>) -> List<entry> {
  match constructors {
    [] -> [],
    [constructor, ..rest] -> [constructor_entry(constructor), ..constructor_table(rest)]
  }
}

fn render_entry(entry: entry) -> String {
  if entry.arity == 0 {
    string_concat(entry.name, "/0")
  } else {
    string_concat(entry.name, "/1")
  }
}

fn render_table(entries: List<entry>) -> String {
  match entries {
    [] -> "",
    [entry, ..rest] ->
      match rest {
        [] -> render_entry(entry),
        _ -> string_concat(render_entry(entry), string_concat(",", render_table(rest)))
      }
  }
}

fn main() {
  let constructors = [Nullary("Eof"), Payload("Ident", "String"), Payload("Int", "i64")];
  println(render_table(constructor_table(constructors)))
}
