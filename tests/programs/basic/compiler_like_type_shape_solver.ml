type ty = TInt | TString | TBool | TList(ty) | TTuple(List<ty>) | TFun(ty, ty)
type constraint = Equal(ty, ty)
type report = { messages: List<String> }

fn append_message(message: String, tail: List<String>) -> List<String> {
  if message == "" {
    tail
  } else {
    [message, ..tail]
  }
}

fn concat_messages(messages: List<String>) -> String {
  match messages {
    [] -> "",
    [message, ..rest] -> {
      let tail = concat_messages(rest);
      if tail == "" {
        message
      } else {
        string_concat(message, string_concat(";", tail))
      }
    }
  }
}

fn merge(left: report, right: report) -> report {
  match left.messages {
    [] -> right,
    [message, ..rest] -> {
      let merged = merge(report { messages: rest }, right);
      report { messages: [message, ..merged.messages] }
    }
  }
}

fn scalar_name(type_: ty) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TBool -> "Bool",
    TList(_) -> "List",
    TTuple(_) -> "Tuple",
    TFun(_, _) -> "Function"
  }
}

fn check_many(constraints: List<constraint>) -> report {
  match constraints {
    [] -> report { messages: [] },
    [Equal(left, right), ..rest] -> merge(check_type(left, right), check_many(rest))
  }
}

fn check_tuple_items(left: List<ty>, right: List<ty>) -> report {
  match left {
    [] -> match right {
      [] -> report { messages: [] },
      [_, .._] -> report { messages: ["tuple shapes do not match"] }
    },
    [left_item, ..left_rest] -> match right {
      [right_item, ..right_rest] -> merge(check_type(left_item, right_item), check_tuple_items(left_rest, right_rest)),
      [] -> report { messages: ["tuple shapes do not match"] }
    }
  }
}

fn mismatch(left: ty, right: ty) -> report {
  match left {
    TFun(_, _) -> report { messages: ["called value is not a function"] },
    _ -> match right {
      TFun(_, _) -> report { messages: ["called value is not a function"] },
      _ -> report { messages: [string_concat("inferred types do not match ", string_concat(scalar_name(left), string_concat(" vs ", scalar_name(right))))] }
    }
  }
}

fn check_type(left: ty, right: ty) -> report {
  match left {
    TInt -> match right { TInt -> report { messages: [] }, _ -> mismatch(left, right) },
    TString -> match right { TString -> report { messages: [] }, _ -> mismatch(left, right) },
    TBool -> match right { TBool -> report { messages: [] }, _ -> mismatch(left, right) },
    TList(left_item) -> match right { TList(right_item) -> check_type(left_item, right_item), _ -> mismatch(left, right) },
    TTuple(left_items) -> match right { TTuple(right_items) -> check_tuple_items(left_items, right_items), _ -> mismatch(left, right) },
    TFun(left_arg, left_result) -> match right { TFun(right_arg, right_result) -> check_many([Equal(left_arg, right_arg), Equal(left_result, right_result)]), _ -> mismatch(left, right) }
  }
}

fn main() {
  let nested = TFun(TTuple([TInt, TList(TString)]), TList(TBool));
  let expected = TFun(TTuple([TInt, TList(TInt), TBool]), TList(TBool));
  let wrong_call = Equal(TInt, TFun(TInt, TInt));
  let result = check_many([Equal(nested, expected), wrong_call, Equal(TList(TString), TList(TBool))]);
  dbg(string_concat(scalar_name(nested), string_concat(":", concat_messages(result.messages))))
}
