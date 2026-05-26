type token = Ident(String) | Number(i64)
type entry = { head: token, tail: token }

fn describe(entry: entry) -> String {
  match entry {
    entry { head: Ident("ok"), tail: Number(7) } -> "ok:7",
    entry { head: Ident("bad"), tail: Number(_) } -> "bad:number",
    _ -> "fallback"
  }
}

fn main() {
  dbg(string_concat(describe(entry { head: Ident("ok"), tail: Number(7) }), string_concat(";", describe(entry { head: Ident("bad"), tail: Ident("not-number") }))))
}
