type rsig_type = TVar | TInt | TString | TUnit | TList(rsig_type) | TActorId(rsig_type) | TArrow(rsig_type, rsig_type)

type abi = Concrete | UnknownAbi

fn render_abi(abi: abi) -> String {
  match abi {
    Concrete -> "concrete",
    UnknownAbi -> "unknown"
  }
}

fn old_has_unknown_abi(type_: rsig_type) -> bool {
  match type_ {
    TVar -> true,
    TInt -> false,
    TString -> false,
    TUnit -> false,
    TActorId(_) -> false,
    TList(item) -> old_has_unknown_abi(item),
    TArrow(parameter, result) -> if old_has_unknown_abi(parameter) { true } else { old_has_unknown_abi(result) }
  }
}

fn new_has_unknown_abi(type_: rsig_type) -> bool {
  match type_ {
    TVar -> true,
    TInt -> false,
    TString -> false,
    TUnit -> false,
    TActorId(_) -> false,
    TList(item) -> new_has_unknown_abi(item),
    TArrow(_, _) -> false
  }
}

fn classify(is_unknown: bool) -> abi {
  if is_unknown { UnknownAbi } else { Concrete }
}

fn imported_call_result(type_: rsig_type) -> String {
  string_concat("old=", string_concat(render_abi(classify(old_has_unknown_abi(type_))), string_concat(", new=", render_abi(classify(new_has_unknown_abi(type_))))))
}

fn main() {
  let polymorphic_identity = TArrow(TVar, TVar);
  let actor_factory = TArrow(TUnit, TActorId(TVar));
  let nested_actor_factory = TArrow(TVar, TArrow(TUnit, TActorId(TString)));
  let raw_unknown = TVar;
  let unknown_list = TList(TVar);
  dbg(string_concat("identity closure: ", imported_call_result(polymorphic_identity)));
  dbg(string_concat("actor factory closure: ", imported_call_result(actor_factory)));
  dbg(string_concat("nested actor factory closure: ", imported_call_result(nested_actor_factory)));
  dbg(string_concat("raw type variable: ", imported_call_result(raw_unknown)));
  dbg(string_concat("list of unknown: ", imported_call_result(unknown_list)))
}
