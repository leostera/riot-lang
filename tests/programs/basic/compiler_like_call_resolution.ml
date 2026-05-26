type callable = { module: String, name: String, arity: i64 }
type call = { path: List<String>, arg_count: i64 }
type diagnostic = { message: String }

fn same_path(path: List<String>, module: String, name: String) -> bool {
  match path {
    [local] -> module == "" && local == name,
    [head, tail] -> head == module && tail == name,
    _ -> false
  }
}

fn find_callable(path: List<String>, callables: List<callable>) -> i64 {
  match callables {
    [] -> -1,
    [candidate, ..rest] -> if same_path(path, candidate.module, candidate.name) {
      candidate.arity
    } else {
      find_callable(path, rest)
    }
  }
}

fn path_prefix(path: List<String>) -> String {
  match path {
    [] -> "<empty>",
    [name] -> name,
    [module, name] -> string_concat(module, string_concat(".", name)),
    [module, name, .._] -> string_concat(module, string_concat(".", name)),
    _ -> "<path>"
  }
}

fn path_length(path: List<String>) -> i64 {
  match path {
    [] -> 0,
    [_, ..rest] -> 1 + path_length(rest)
  }
}

fn check_call(call: call, callables: List<callable>) -> List<diagnostic> {
  if path_length(call.path) < 3 {
    let arity = find_callable(call.path, callables);
    if arity == -1 {
      [diagnostic { message: string_concat("unknown callee ", path_prefix(call.path)) }]
    } else if arity == call.arg_count {
      []
    } else {
      [diagnostic { message: string_concat("arity mismatch ", path_prefix(call.path)) }]
    }
  } else {
    [diagnostic { message: string_concat("nested call ", path_prefix(call.path)) }]
  }
}

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
  }
}

fn check_all(calls: List<call>, callables: List<callable>) -> List<diagnostic> {
  match calls {
    [] -> [],
    [head, ..tail] -> append(check_call(head, callables), check_all(tail, callables))
  }
}

fn join(diagnostics: List<diagnostic>) -> String {
  match diagnostics {
    [] -> "ok",
    [diagnostic { message: message }] -> message,
    [diagnostic { message: message }, ..rest] -> string_concat(message, string_concat("; ", join(rest)))
  }
}

fn main() {
  let callables = [
    callable { module: "", name: "print", arity: 1 },
    callable { module: "Result", name: "is_ok", arity: 1 }
  ];
  let calls = [
    call { path: ["print"], arg_count: 1 },
    call { path: ["print"], arg_count: 2 },
    call { path: ["Result", "is_ok"], arg_count: 1 },
    call { path: ["Result", "missing"], arg_count: 1 },
    call { path: ["Result", "is_ok", "extra"], arg_count: 1 }
  ];
  println(join(check_all(calls, callables)))
}
