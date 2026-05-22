type typ =
  | TInt
  | TString
  | TBool
  | TList(typ)
  | TFun(typ, typ)

type origin =
  | IfCondition
  | CallArgument(String)
  | ListItem

type infer_step = { origin: origin, expected: typ, actual: typ }

fn render_type(typ: typ) -> String {
  match typ {
    TInt -> "Int",
    TString -> "String",
    TBool -> "Bool",
    TList(item) -> string_concat("List<", string_concat(render_type(item), ">")),
    TFun(arg, result) -> string_concat(render_type(arg), string_concat(" -> ", render_type(result)))
  }
}

fn render_origin(origin: origin) -> String {
  match origin {
    IfCondition -> "if condition",
    CallArgument(name) -> string_concat("call argument ", name),
    ListItem -> "list item"
  }
}

fn render_step(step: infer_step) -> String {
  string_concat(render_origin(step.origin), string_concat(": ", string_concat(render_type(step.expected), string_concat(" vs ", render_type(step.actual)))))
}

fn join(messages: List<String>) -> String {
  match messages {
    [] -> "",
    [message] -> message,
    [message, ..rest] -> string_concat(message, string_concat(" | ", join(rest)))
  }
}

fn render_steps(steps: List<infer_step>) -> List<String> {
  match steps {
    [] -> [],
    [step, ..rest] -> [render_step(step), ..render_steps(rest)]
  }
}

fn main() {
  let steps = [
    infer_step { origin: IfCondition, expected: TBool, actual: TInt },
    infer_step { origin: CallArgument("parse"), expected: TString, actual: TList(TInt) },
    infer_step { origin: ListItem, expected: TFun(TInt, TString), actual: TFun(TInt, TBool) }
  ];
  println(join(render_steps(steps)))
}
