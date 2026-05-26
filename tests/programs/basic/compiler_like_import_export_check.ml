type export = { name: String, typ: String }
type module_sig = { name: String, exports: List<export> }
type import_request = { module: String, value: String }
type diagnostic = { message: String }

fn find_module(name: String, modules: List<module_sig>) -> List<export> {
  match modules {
    [] -> [],
    [module, ..rest] -> if module.name == name { module.exports } else { find_module(name, rest) }
  }
}

fn has_export(name: String, exports: List<export>) -> bool {
  match exports {
    [] -> false,
    [export, ..rest] -> if export.name == name { true } else { has_export(name, rest) }
  }
}

fn check_import(request: import_request, modules: List<module_sig>) -> List<diagnostic> {
  let exports = find_module(request.module, modules);
  match exports {
    [] -> [diagnostic { message: string_concat("unknown module ", request.module) }],
    _ -> if has_export(request.value, exports) {
      []
    } else {
      [diagnostic { message: string_concat("unknown export ", string_concat(request.module, string_concat(".", request.value))) }]
    }
  }
}

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
  }
}

fn check_all(requests: List<import_request>, modules: List<module_sig>) -> List<diagnostic> {
  match requests {
    [] -> [],
    [head, ..tail] -> append(check_import(head, modules), check_all(tail, modules))
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
  let modules = [module_sig { name: "Result", exports: [export { name: "is_ok", typ: "Result<a,b> -> Bool" }] }];
  let requests = [
    import_request { module: "Result", value: "is_ok" },
    import_request { module: "Result", value: "missing" },
    import_request { module: "Missing", value: "value" }
  ];
  println(join(check_all(requests, modules)))
}
