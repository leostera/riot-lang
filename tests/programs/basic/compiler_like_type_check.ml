type typ =
  | TInt
  | TString
  | TBool
  | TUnknown

type expr =
  | IntLit
  | StringLit
  | BoolLit
  | Add(expr, expr)
  | Eq(expr, expr)

type diagnostic = { message: String }

type check = { typ: typ, diagnostics: List<diagnostic> }

fn same_type(left: typ, right: typ) -> bool {
  match left {
    TInt ->
      match right {
        TInt -> true,
        _ -> false
      },
    TString ->
      match right {
        TString -> true,
        _ -> false
      },
    TBool ->
      match right {
        TBool -> true,
        _ -> false
      },
    TUnknown -> true
  }
}

fn append(left: List<diagnostic>, right: List<diagnostic>) -> List<diagnostic> {
  match left {
    [] -> right,
    [item, ..rest] -> [item, ..append(rest, right)]
  }
}

fn require_int(typ: typ, label: String) -> List<diagnostic> {
  match typ {
    TInt -> [],
    _ -> [diagnostic { message: string_concat("expected int for ", label) }]
  }
}

fn infer(expr: expr) -> check {
  match expr {
    IntLit -> check { typ: TInt, diagnostics: [] },
    StringLit -> check { typ: TString, diagnostics: [] },
    BoolLit -> check { typ: TBool, diagnostics: [] },
    Add(left, right) -> {
      let left_check = infer(left);
      let right_check = infer(right);
      let left_errors = require_int(left_check.typ, "left operand");
      let right_errors = require_int(right_check.typ, "right operand");
      check { typ: TInt, diagnostics: append(append(left_check.diagnostics, right_check.diagnostics), append(left_errors, right_errors)) }
    },
    Eq(left, right) -> {
      let left_check = infer(left);
      let right_check = infer(right);
      let nested = append(left_check.diagnostics, right_check.diagnostics);
      if same_type(left_check.typ, right_check.typ) {
        check { typ: TBool, diagnostics: nested }
      } else {
        check { typ: TBool, diagnostics: [diagnostic { message: "equality type mismatch" }, ..nested] }
      }
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
  let expr = Eq(Add(StringLit, BoolLit), IntLit);
  let result = infer(expr);
  println(render_diagnostics(result.diagnostics))
}
