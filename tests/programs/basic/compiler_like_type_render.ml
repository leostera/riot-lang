type ty =
  | TInt
  | TString
  | TList(ty)
  | TArrow(ty, ty)
  | TNamed(String)

fn render(type_: ty) -> String {
  match type_ {
    TInt -> "i64",
    TString -> "String",
    TList(item) -> string_concat("List<", string_concat(render(item), ">")),
    TArrow(param, result) -> string_concat(render(param), string_concat(" -> ", render(result))),
    TNamed(name) -> name
  }
}

fn main() {
  println(render(TArrow(TList(TNamed("token")), TString)))
}
