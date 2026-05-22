type typ =
  | TInt
  | TString
  | TBool
  | TTuple(typ, typ)

type equality = { origin: String, left: typ, right: typ }

fn render_type(typ: typ) -> String {
  match typ {
    TInt -> "i64",
    TString -> "String",
    TBool -> "Bool",
    TTuple(left, right) -> string_concat("(", string_concat(render_type(left), string_concat(", ", string_concat(render_type(right), ")"))))
  }
}

fn same_type(left: typ, right: typ) -> bool {
  match left {
    TInt -> match right { TInt -> true, TString -> false, TBool -> false, TTuple(_, _) -> false },
    TString -> match right { TInt -> false, TString -> true, TBool -> false, TTuple(_, _) -> false },
    TBool -> match right { TInt -> false, TString -> false, TBool -> true, TTuple(_, _) -> false },
    TTuple(left_a, left_b) ->
      match right {
        TTuple(right_a, right_b) -> same_type(left_a, right_a) && same_type(left_b, right_b),
        TInt -> false,
        TString -> false,
        TBool -> false
      }
  }
}

fn check(equality: equality) -> String {
  if same_type(equality.left, equality.right) {
    string_concat(equality.origin, " ok")
  } else {
    string_concat(equality.origin, string_concat(" rejects ", string_concat(render_type(equality.left), string_concat(" == ", render_type(equality.right)))))
  }
}

fn check_all(equalities: List<equality>) -> List<String> {
  match equalities {
    [] -> [],
    [equality, ..rest] -> [check(equality), ..check_all(rest)]
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
  let equalities = [
    equality { origin: "line 1", left: TInt, right: TInt },
    equality { origin: "line 2", left: TTuple(TInt, TBool), right: TTuple(TInt, TBool) },
    equality { origin: "line 3", left: TString, right: TBool },
    equality { origin: "line 4", left: TTuple(TInt, TString), right: TTuple(TInt, TBool) }
  ];
  println(join(check_all(equalities)))
}
