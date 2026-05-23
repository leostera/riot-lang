type typ = TVar(String) | TInt | TString | TList(typ) | TArrow(typ, typ) | TVariant(String, List<typ>)

fn old_type_matches(expected: typ, actual: typ) -> bool {
  match (expected, actual) {
    (TVar(_), _) -> true,
    (_, TVar(_)) -> true,
    (TInt, TInt) -> true,
    (TString, TString) -> true,
    (TList(expected_item), TList(actual_item)) -> old_type_matches(expected_item, actual_item),
    (TVariant(expected_name, expected_args), TVariant(actual_name, actual_args)) -> expected_name == actual_name && match_args_old(expected_args, actual_args),
    _ -> false,
  }
}

fn new_type_matches(expected: typ, actual: typ) -> bool {
  match (expected, actual) {
    (TVar(_), _) -> true,
    (_, TVar(_)) -> true,
    (TInt, TInt) -> true,
    (TString, TString) -> true,
    (TArrow(expected_arg, expected_result), TArrow(actual_arg, actual_result)) -> new_type_matches(expected_arg, actual_arg) && new_type_matches(expected_result, actual_result),
    (TList(expected_item), TList(actual_item)) -> new_type_matches(expected_item, actual_item),
    (TVariant(expected_name, expected_args), TVariant(actual_name, actual_args)) -> expected_name == actual_name && match_args_new(expected_args, actual_args),
    _ -> false,
  }
}

fn match_args_old(expected: List<typ>, actual: List<typ>) -> bool {
  match (expected, actual) {
    ([], []) -> true,
    ([expected_arg, ..expected_rest], [actual_arg, ..actual_rest]) -> old_type_matches(expected_arg, actual_arg) && match_args_old(expected_rest, actual_rest),
    _ -> false,
  }
}

fn match_args_new(expected: List<typ>, actual: List<typ>) -> bool {
  match (expected, actual) {
    ([], []) -> true,
    ([expected_arg, ..expected_rest], [actual_arg, ..actual_rest]) -> new_type_matches(expected_arg, actual_arg) && match_args_new(expected_rest, actual_rest),
    _ -> false,
  }
}

fn render_bool(value: bool) -> String {
  if value { "match" } else { "reject" }
}

fn main() {
  let expected_list = TList(TArrow(TVar("a"), TInt));
  let actual_list = TList(TArrow(TString, TInt));
  let bad_list = TList(TArrow(TString, TString));
  let expected_cell = TVariant("cell", [TArrow(TVar("a"), TInt)]);
  let actual_cell = TVariant("cell", [TArrow(TString, TInt)]);

  dbg(string_concat("old list: ", render_bool(old_type_matches(expected_list, actual_list))));
  dbg(string_concat("new list: ", render_bool(new_type_matches(expected_list, actual_list))));
  dbg(string_concat("bad result: ", render_bool(new_type_matches(expected_list, bad_list))));
  dbg(string_concat("variant: ", render_bool(new_type_matches(expected_cell, actual_cell))));
  ()
}
