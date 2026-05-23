type typ = TUnknown | TInt | TString | TActor(typ)
type receive_arm = { pattern: typ, body: typ }

fn render_type(type_: typ) -> String {
  match type_ {
    TUnknown -> "Unknown",
    TInt -> "i64",
    TString -> "String",
    TActor(message) -> string_concat("ActorId<", string_concat(render_type(message), ">"))
  }
}

fn merge_message_type(left: typ, right: typ) -> typ {
  match left {
    TUnknown -> right,
    TInt -> match right {
      TUnknown -> TInt,
      TInt -> TInt,
      TString -> TUnknown,
      TActor(_) -> TUnknown
    },
    TString -> match right {
      TUnknown -> TString,
      TString -> TString,
      TInt -> TUnknown,
      TActor(_) -> TUnknown
    },
    TActor(message) -> match right {
      TActor(other) -> TActor(merge_message_type(message, other)),
      _ -> TUnknown
    }
  }
}

fn infer_receive_message(arms: List<receive_arm>) -> typ {
  match arms {
    [] -> TUnknown,
    [arm] -> arm.pattern,
    [arm, ..rest] -> merge_message_type(arm.pattern, infer_receive_message(rest))
  }
}

fn lower_spawn(arms: List<receive_arm>) -> typ {
  let message = infer_receive_message(arms);
  TActor(message)
}

fn render_phase(phase: String, type_: typ) -> String {
  string_concat(phase, string_concat(":", render_type(type_)))
}

fn main() {
  let arms = [
    receive_arm { pattern: TInt, body: TUnknown },
    receive_arm { pattern: TInt, body: TUnknown }
  ];
  dbg(render_phase("inferred", lower_spawn(arms)));
  dbg(render_phase("fallback", lower_spawn([])))
}
