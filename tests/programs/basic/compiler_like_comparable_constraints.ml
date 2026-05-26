type typ =
  | TInt
  | TString
  | TBool
  | TList(typ)

type comparison = { origin: String, left: typ, right: typ }

fn render_type(typ: typ) -> String {
  match typ {
    TInt -> "i64",
    TString -> "String",
    TBool -> "Bool",
    TList(item) -> string_concat("List<", string_concat(render_type(item), ">"))
  }
}

fn comparable(typ: typ) -> bool {
  match typ {
    TInt -> true,
    TString -> true,
    TBool -> false,
    TList(_) -> false
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

fn check(comparison: comparison) -> String {
  if comparable(comparison.left) && same_type(comparison.left, comparison.right) {
    string_concat(comparison.origin, " ok")
  } else {
    string_concat(comparison.origin, string_concat(" rejects ", string_concat(render_type(comparison.left), string_concat(" < ", render_type(comparison.right)))))
  }
}

fn check_all(comparisons: List<comparison>) -> List<String> {
  match comparisons {
    [] -> [],
    [comparison, ..rest] -> [check(comparison), ..check_all(rest)]
  }
}

fn join(messages: List<String>) -> String {
  match messages {
    [] -> "",
    [message] -> message,
    [message, ..rest] -> string_concat(message, string_concat("; ", join(rest)))
  }
}

fn main() {
  let comparisons = [
    comparison { origin: "line 1", left: TInt, right: TInt },
    comparison { origin: "line 2", left: TString, right: TString },
    comparison { origin: "line 3", left: TBool, right: TBool },
    comparison { origin: "line 4", left: TList(TInt), right: TList(TInt) }
  ];
  println(join(check_all(comparisons)))
}
