type typ =
  | TInt
  | TString
  | TBool
  | TList(typ)

type branch_group = { origin: String, results: List<typ> }

fn render_type(typ: typ) -> String {
  match typ {
    TInt -> "Int",
    TString -> "String",
    TBool -> "Bool",
    TList(item) -> string_concat("List<", string_concat(render_type(item), ">"))
  }
}

fn same_type(left: typ, right: typ) -> bool {
  match left {
    TInt -> match right { TInt -> true, TString -> false, TBool -> false, TList(_) -> false },
    TString -> match right { TInt -> false, TString -> true, TBool -> false, TList(_) -> false },
    TBool -> match right { TInt -> false, TString -> false, TBool -> true, TList(_) -> false },
    TList(left_item) -> match right { TList(right_item) -> same_type(left_item, right_item), TInt -> false, TString -> false, TBool -> false }
  }
}

fn check_against(origin: String, expected: typ, actuals: List<typ>) -> List<String> {
  match actuals {
    [] -> [],
    [actual, ..rest] ->
      if same_type(expected, actual) {
        check_against(origin, expected, rest)
      } else {
        [string_concat(origin, string_concat(": ", string_concat(render_type(expected), string_concat(" != ", render_type(actual))))), ..check_against(origin, expected, rest)]
      }
  }
}

fn check_group(group: branch_group) -> List<String> {
  match group.results {
    [] -> [],
    [first, ..rest] -> check_against(group.origin, first, rest)
  }
}

fn append(left: List<String>, right: List<String>) -> List<String> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
  }
}

fn check_all(groups: List<branch_group>) -> List<String> {
  match groups {
    [] -> [],
    [group, ..rest] -> append(check_group(group), check_all(rest))
  }
}

fn join(messages: List<String>) -> String {
  match messages {
    [] -> "ok",
    [message] -> message,
    [message, ..rest] -> string_concat(message, string_concat("; ", join(rest)))
  }
}

fn main() {
  let groups = [
    branch_group { origin: "if branches", results: [TInt, TInt] },
    branch_group { origin: "match arms", results: [TString, TString, TBool] },
    branch_group { origin: "list branches", results: [TList(TInt), TList(TString)] }
  ];
  println(join(check_all(groups)))
}
