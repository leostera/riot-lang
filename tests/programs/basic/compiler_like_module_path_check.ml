type path = { segments: List<String> }
type import = { module: String, exports: List<String> }
type diagnostic = { message: String }

fn contains(name: String, names: List<String>) -> bool {
  match names {
    [] -> false,
    [head, ..tail] -> if head == name { true } else { contains(name, tail) }
  }
}

fn find_exports(module: String, imports: List<import>) -> List<String> {
  match imports {
    [] -> [],
    [entry, ..rest] -> if entry.module == module { entry.exports } else { find_exports(module, rest) }
  }
}

fn length(items: List<String>) -> i64 {
  match items {
    [] -> 0,
    [_, ..tail] -> 1 + length(tail)
  }
}

fn check_path(path: path, locals: List<String>, imports: List<import>) -> List<diagnostic> {
  match path.segments {
    [] -> [diagnostic { message: "empty path" }],
    [name] -> if contains(name, locals) { [] } else { [diagnostic { message: string_concat("unknown local ", name) }] },
    [module, name] -> {
      let exports = find_exports(module, imports);
      match exports {
        [] -> [diagnostic { message: string_concat("unknown module ", module) }],
        _ -> if contains(name, exports) { [] } else { [diagnostic { message: string_concat("unknown export ", string_concat(module, string_concat(".", name))) }] }
      }
    },
    [module, name, .._] -> [diagnostic { message: string_concat("nested path ", string_concat(module, string_concat(".", name))) }],
    _ -> [diagnostic { message: string_concat("path length ", "unsupported") }]
  }
}

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
  }
}

fn check_all(paths: List<path>, locals: List<String>, imports: List<import>) -> List<diagnostic> {
  match paths {
    [] -> [],
    [head, ..tail] -> append(check_path(head, locals, imports), check_all(tail, locals, imports))
  }
}

fn join(messages: List<diagnostic>) -> String {
  match messages {
    [] -> "ok",
    [diagnostic { message: message }] -> message,
    [diagnostic { message: message }, ..rest] -> string_concat(message, string_concat("; ", join(rest)))
  }
}

fn main() {
  let imports = [import { module: "Result", exports: ["is_ok", "unwrap_or"] }];
  let paths = [
    path { segments: ["local"] },
    path { segments: ["missing"] },
    path { segments: ["Result", "is_ok"] },
    path { segments: ["Result", "missing"] },
    path { segments: ["Result", "is_ok", "extra"] }
  ];
  println(join(check_all(paths, ["local"], imports)))
}
