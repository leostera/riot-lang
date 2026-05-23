type typ = TUnknown | TInt | TString | TRecord(String, typ) | TActor(typ)
type receive_arm = { pattern: typ, body_constraint: typ }

fn render_type(type_: typ) -> String {
  match type_ {
    TUnknown -> "Unknown",
    TInt -> "i64",
    TString -> "String",
    TRecord(name, field) -> string_concat(name, string_concat("<", string_concat(render_type(field), ">"))),
    TActor(message) -> string_concat("ActorId<", string_concat(render_type(message), ">"))
  }
}

fn refine_pattern(pattern: typ, body_constraint: typ) -> typ {
  match pattern {
    TRecord(name, field) -> match field {
      TUnknown -> TRecord(name, body_constraint),
      _ -> pattern
    },
    _ -> pattern
  }
}

fn merge_message_type(left: typ, right: typ) -> typ {
  match left {
    TUnknown -> right,
    TInt -> match right {
      TUnknown -> TInt,
      TInt -> TInt,
      _ -> TUnknown
    },
    TString -> match right {
      TUnknown -> TString,
      TString -> TString,
      _ -> TUnknown
    },
    TRecord(name, field) -> match right {
      TUnknown -> left,
      TRecord(other_name, other_field) -> if name == other_name { TRecord(name, merge_message_type(field, other_field)) } else { TUnknown },
      _ -> TUnknown
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
    [arm] -> refine_pattern(arm.pattern, arm.body_constraint),
    [arm, ..rest] -> merge_message_type(refine_pattern(arm.pattern, arm.body_constraint), infer_receive_message(rest))
  }
}

fn lower_spawn(arms: List<receive_arm>) -> typ {
  TActor(infer_receive_message(arms))
}

fn render_phase(phase: String, type_: typ) -> String {
  string_concat(phase, string_concat(":", render_type(type_)))
}

fn main() {
  let generic_record_arm = receive_arm { pattern: TRecord("box", TUnknown), body_constraint: TInt };
  let string_arm = receive_arm { pattern: TString, body_constraint: TUnknown };
  let int_arm = receive_arm { pattern: TInt, body_constraint: TUnknown };
  dbg(render_phase("refined", lower_spawn([generic_record_arm])));
  dbg(render_phase("heterogeneous", lower_spawn([string_arm, int_arm])))
}
