type typ =
  | TVar(String)
  | TInt
  | TString
  | TBool
  | TList(typ)
  | TFun(typ, typ)

type constraint = { left: typ, right: typ }
type subst = { name: String, value: typ }
type solver = { substitutions: List<subst>, diagnostics: List<String> }

fn render_type(typ: typ) -> String {
  match typ {
    TVar(name) -> name,
    TInt -> "Int",
    TString -> "String",
    TBool -> "Bool",
    TList(item) -> string_concat("List<", string_concat(render_type(item), ">")),
    TFun(arg, result) -> string_concat(render_type(arg), string_concat(" -> ", render_type(result)))
  }
}

fn same_type(left: typ, right: typ) -> bool {
  match left {
    TInt -> match right { TInt -> true, TString -> false, TBool -> false, TVar(_) -> false, TList(_) -> false, TFun(_, _) -> false },
    TString -> match right { TInt -> false, TString -> true, TBool -> false, TVar(_) -> false, TList(_) -> false, TFun(_, _) -> false },
    TBool -> match right { TInt -> false, TString -> false, TBool -> true, TVar(_) -> false, TList(_) -> false, TFun(_, _) -> false },
    TVar(left_name) -> match right { TVar(right_name) -> left_name == right_name, TInt -> false, TString -> false, TBool -> false, TList(_) -> false, TFun(_, _) -> false },
    TList(left_item) -> match right { TList(right_item) -> same_type(left_item, right_item), TInt -> false, TString -> false, TBool -> false, TVar(_) -> false, TFun(_, _) -> false },
    TFun(left_arg, left_result) -> match right { TFun(right_arg, right_result) -> same_type(left_arg, right_arg) && same_type(left_result, right_result), TInt -> false, TString -> false, TBool -> false, TVar(_) -> false, TList(_) -> false }
  }
}

fn find_subst(name: String, substitutions: List<subst>) -> typ {
  match substitutions {
    [] -> TVar(name),
    [entry, ..rest] -> if entry.name == name { entry.value } else { find_subst(name, rest) }
  }
}

fn apply_subst(typ: typ, substitutions: List<subst>) -> typ {
  match typ {
    TVar(name) -> find_subst(name, substitutions),
    TInt -> TInt,
    TString -> TString,
    TBool -> TBool,
    TList(item) -> TList(apply_subst(item, substitutions)),
    TFun(arg, result) -> TFun(apply_subst(arg, substitutions), apply_subst(result, substitutions))
  }
}

fn push_diag(message: String, solver: solver) -> solver {
  solver { substitutions: solver.substitutions, diagnostics: [message, ..solver.diagnostics] }
}

fn bind(name: String, value: typ, solver: solver) -> solver {
  solver { substitutions: [subst { name: name, value: value }, ..solver.substitutions], diagnostics: solver.diagnostics }
}

fn solve_one(constraint: constraint, solver: solver) -> solver {
  let left = apply_subst(constraint.left, solver.substitutions);
  let right = apply_subst(constraint.right, solver.substitutions);
  match left {
    TVar(name) -> bind(name, right, solver),
    TInt -> if same_type(left, right) { solver } else { push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver) },
    TString -> if same_type(left, right) { solver } else { push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver) },
    TBool -> if same_type(left, right) { solver } else { push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver) },
    TList(left_item) ->
      match right {
        TList(right_item) -> solve_one(constraint { left: left_item, right: right_item }, solver),
        TInt -> push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver),
        TString -> push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver),
        TBool -> push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver),
        TVar(name) -> bind(name, left, solver),
        TFun(_, _) -> push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver)
      },
    TFun(left_arg, left_result) ->
      match right {
        TFun(right_arg, right_result) -> solve_one(constraint { left: left_result, right: right_result }, solve_one(constraint { left: left_arg, right: right_arg }, solver)),
        TInt -> push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver),
        TString -> push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver),
        TBool -> push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver),
        TVar(name) -> bind(name, left, solver),
        TList(_) -> push_diag(string_concat(render_type(left), string_concat(" != ", render_type(right))), solver)
      }
  }
}

fn solve_all(constraints: List<constraint>, solver: solver) -> solver {
  match constraints {
    [] -> solver,
    [constraint, ..rest] -> solve_all(rest, solve_one(constraint, solver))
  }
}

fn reverse(messages: List<String>) -> List<String> {
  match messages {
    [] -> [],
    [head, ..tail] -> append(reverse(tail), [head])
  }
}

fn append(left: List<String>, right: List<String>) -> List<String> {
  match left {
    [] -> right,
    [head, ..tail] -> [head, ..append(tail, right)]
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
  let constraints = [
    constraint { left: TVar("a"), right: TInt },
    constraint { left: TList(TVar("a")), right: TList(TString) },
    constraint { left: TFun(TVar("b"), TBool), right: TFun(TString, TVar("c")) },
    constraint { left: TVar("c"), right: TInt }
  ];
  let result = solve_all(constraints, solver { substitutions: [], diagnostics: [] });
  println(join(reverse(result.diagnostics)))
}
