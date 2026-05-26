type typ =
  | TInt
  | TString
  | TBool
  | TList(typ)
  | TFun(typ, typ)

type constraint = { left: typ, right: typ }

type check_result =
  | Ok
  | Errors(List<String>)

fn type_eq(left: typ, right: typ) -> bool {
  match left {
    TInt ->
      match right {
        TInt -> true,
        TString -> false,
        TBool -> false,
        TList(_) -> false,
        TFun(_, _) -> false
      },
    TString ->
      match right {
        TInt -> false,
        TString -> true,
        TBool -> false,
        TList(_) -> false,
        TFun(_, _) -> false
      },
    TBool ->
      match right {
        TInt -> false,
        TString -> false,
        TBool -> true,
        TList(_) -> false,
        TFun(_, _) -> false
      },
    TList(left_item) ->
      match right {
        TList(right_item) -> type_eq(left_item, right_item),
        TInt -> false,
        TString -> false,
        TBool -> false,
        TFun(_, _) -> false
      },
    TFun(left_arg, left_result) ->
      match right {
        TFun(right_arg, right_result) -> type_eq(left_arg, right_arg) && type_eq(left_result, right_result),
        TInt -> false,
        TString -> false,
        TBool -> false,
        TList(_) -> false
      }
  }
}

fn render(typ: typ) -> String {
  match typ {
    TInt -> "Int",
    TString -> "String",
    TBool -> "Bool",
    TList(item) -> string_concat("List<", string_concat(render(item), ">")),
    TFun(arg, result) -> string_concat(render(arg), string_concat(" -> ", render(result)))
  }
}

fn check_constraint(constraint: constraint) -> check_result {
  if type_eq(constraint.left, constraint.right) {
    Ok
  } else {
    Errors([string_concat(render(constraint.left), string_concat(" != ", render(constraint.right)))])
  }
}

fn append_errors(left: List<String>, right: List<String>) -> List<String> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append_errors(tail, right)]
  }
}

fn check_all(constraints: List<constraint>) -> check_result {
  match constraints {
    [] -> Ok,
    [constraint, ..rest] ->
      match check_constraint(constraint) {
        Ok -> check_all(rest),
        Errors(head_errors) ->
          match check_all(rest) {
            Ok -> Errors(head_errors),
            Errors(tail_errors) -> Errors(append_errors(head_errors, tail_errors))
          }
      }
  }
}

fn render_errors(errors: List<String>) -> String {
  match errors {
    [] -> "",
    [message] -> message,
    [message, ..rest] -> string_concat(message, string_concat("; ", render_errors(rest)))
  }
}

fn render_result(result: check_result) -> String {
  match result {
    Ok -> "ok",
    Errors(errors) -> render_errors(errors)
  }
}

fn main() {
  let constraints = [
    constraint { left: TInt, right: TInt },
    constraint { left: TList(TString), right: TList(TString) },
    constraint { left: TFun(TInt, TString), right: TFun(TInt, TBool) },
    constraint { left: TList(TInt), right: TList(TString) }
  ];
  println(render_result(check_all(constraints)))
}
