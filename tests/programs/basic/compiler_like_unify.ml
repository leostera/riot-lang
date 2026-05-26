type typ =
  | TInt
  | TString
  | TBool
  | TList(typ)
  | TFun(typ, typ)

type diagnostic = { message: String }

type result = { ok: bool, diagnostics: List<diagnostic> }

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [item, ..rest] -> [item, ..append(rest, right)]
  }
}

fn type_name(typ: typ) -> String {
  match typ {
    TInt -> "Int",
    TString -> "String",
    TBool -> "Bool",
    TList(item) -> string_concat("List<", string_concat(type_name(item), ">")),
    TFun(_, _) -> "Function"
  }
}

fn unify(left: typ, right: typ) -> result {
  match left {
    TInt ->
      match right {
        TInt -> result { ok: true, diagnostics: [] },
        _ -> result { ok: false, diagnostics: [diagnostic { message: string_concat("cannot unify Int with ", type_name(right)) }] }
      },
    TString ->
      match right {
        TString -> result { ok: true, diagnostics: [] },
        _ -> result { ok: false, diagnostics: [diagnostic { message: string_concat("cannot unify String with ", type_name(right)) }] }
      },
    TBool ->
      match right {
        TBool -> result { ok: true, diagnostics: [] },
        _ -> result { ok: false, diagnostics: [diagnostic { message: string_concat("cannot unify Bool with ", type_name(right)) }] }
      },
    TList(left_item) ->
      match right {
        TList(right_item) -> unify(left_item, right_item),
        _ -> result { ok: false, diagnostics: [diagnostic { message: string_concat("cannot unify List with ", type_name(right)) }] }
      },
    TFun(left_arg, left_result) ->
      match right {
        TFun(right_arg, right_result) -> {
          let arg = unify(left_arg, right_arg);
          let ret = unify(left_result, right_result);
          result { ok: arg.ok == ret.ok, diagnostics: append(arg.diagnostics, ret.diagnostics) }
        },
        _ -> result { ok: false, diagnostics: [diagnostic { message: string_concat("cannot unify Function with ", type_name(right)) }] }
      }
  }
}

fn render_diagnostics(diagnostics: List<diagnostic>) -> String {
  match diagnostics {
    [] -> "ok",
    [diagnostic, ..rest] ->
      match rest {
        [] -> diagnostic.message,
        _ -> string_concat(diagnostic.message, string_concat("; ", render_diagnostics(rest)))
      }
  }
}

fn main() {
  let expected = TFun(TList(TInt), TString);
  let actual = TFun(TList(TString), TBool);
  println(render_diagnostics(unify(expected, actual).diagnostics))
}
