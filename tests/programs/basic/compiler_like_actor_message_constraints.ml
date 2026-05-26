type typ = TUnknown | TVar | TInt | TString | TRecord(String, typ) | TVariant(String, typ) | TList(typ) | TActor(typ)
type merge_depth = Top | Nested
type receive_arm = { pattern: typ, body_constraint: typ }

fn render_type(type_: typ) -> String {
  match type_ {
    TUnknown -> "Unknown",
    TVar -> "'msg",
    TInt -> "i64",
    TString -> "String",
    TRecord(name, field) -> string_concat(name, string_concat("<", string_concat(render_type(field), ">"))),
    TVariant(name, payload) -> string_concat(name, string_concat("<", string_concat(render_type(payload), ">"))),
    TList(item) -> string_concat("List<", string_concat(render_type(item), ">")),
    TActor(message) -> string_concat("ActorId<", string_concat(render_type(message), ">"))
  }
}

fn refine_pattern(pattern: typ, body_constraint: typ) -> typ {
  match pattern {
    TRecord(name, field) -> match field {
      TUnknown -> TRecord(name, body_constraint),
      _ -> pattern
    },
    TVariant(name, payload) -> match payload {
      TUnknown -> TVariant(name, body_constraint),
      _ -> pattern
    },
    TList(item) -> match item {
      TUnknown -> TList(body_constraint),
      _ -> pattern
    },
    _ -> pattern
  }
}

fn is_nested(depth: merge_depth) {
  match depth {
    Top -> false,
    Nested -> true
  }
}

fn merge_message_type_nested(left: typ, right: typ, depth: merge_depth) -> typ {
  match left {
    TUnknown -> right,
    TVar -> if is_nested(depth) { right } else { TUnknown },
    TInt -> match right {
      TUnknown -> TInt,
      TVar -> if is_nested(depth) { TInt } else { TUnknown },
      TInt -> TInt,
      _ -> TUnknown
    },
    TString -> match right {
      TUnknown -> TString,
      TVar -> if is_nested(depth) { TString } else { TUnknown },
      TString -> TString,
      _ -> TUnknown
    },
    TRecord(name, field) -> match right {
      TUnknown -> left,
      TVar -> if is_nested(depth) { left } else { TUnknown },
      TRecord(other_name, other_field) -> if name == other_name { TRecord(name, merge_message_type_nested(field, other_field, Nested)) } else { TUnknown },
      _ -> TUnknown
    },
    TVariant(name, payload) -> match right {
      TUnknown -> left,
      TVar -> if is_nested(depth) { left } else { TUnknown },
      TVariant(other_name, other_payload) -> if name == other_name { TVariant(name, merge_message_type_nested(payload, other_payload, Nested)) } else { TUnknown },
      _ -> TUnknown
    },
    TList(item) -> match right {
      TUnknown -> left,
      TVar -> if is_nested(depth) { left } else { TUnknown },
      TList(other_item) -> TList(merge_message_type_nested(item, other_item, Nested)),
      _ -> TUnknown
    },
    TActor(message) -> match right {
      TUnknown -> left,
      TVar -> if is_nested(depth) { left } else { TUnknown },
      TActor(other) -> TActor(merge_message_type_nested(message, other, Nested)),
      _ -> TUnknown
    }
  }
}

fn merge_message_type(left: typ, right: typ) -> typ {
  merge_message_type_nested(left, right, Top)
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
  let generic_record_empty_arm = receive_arm { pattern: TRecord("box", TVar), body_constraint: TUnknown };
  let generic_variant_arm = receive_arm { pattern: TVariant("option", TUnknown), body_constraint: TInt };
  let generic_variant_empty_arm = receive_arm { pattern: TVariant("option", TVar), body_constraint: TUnknown };
  let list_head_arm = receive_arm { pattern: TList(TUnknown), body_constraint: TInt };
  let list_tail_arm = receive_arm { pattern: TList(TVar), body_constraint: TUnknown };
  let wildcard_arm = receive_arm { pattern: TVar, body_constraint: TUnknown };
  let string_arm = receive_arm { pattern: TString, body_constraint: TUnknown };
  let int_arm = receive_arm { pattern: TInt, body_constraint: TUnknown };
  dbg(render_phase("record", lower_spawn([generic_record_arm, generic_record_empty_arm])));
  dbg(render_phase("variant", lower_spawn([generic_variant_arm, generic_variant_empty_arm])));
  dbg(render_phase("list", lower_spawn([list_head_arm, list_tail_arm])));
  dbg(render_phase("heterogeneous", lower_spawn([string_arm, int_arm])));
  dbg(render_phase("wildcard", lower_spawn([string_arm, wildcard_arm])))
}
