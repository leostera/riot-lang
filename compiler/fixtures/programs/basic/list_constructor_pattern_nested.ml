type token = Ident(String) | Number(i64)

fn describe(tokens: List<token>) -> String {
  match tokens {
    [Ident("ok"), Number(7)] -> "ok:7",
    [Ident("bad"), Number(_)] -> "bad:number",
    _ -> "fallback"
  }
}

fn main() {
  dbg(string_concat(describe([Ident("ok"), Number(7)]), string_concat(";", describe([Ident("bad"), Ident("not-number")]))))
}
