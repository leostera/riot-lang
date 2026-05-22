type token = Ident(String) | Number(i64)
type entry = { head: token, tail: token }

fn main() {
  let worker = spawn {
    receive {
      entry { head: Ident("ok"), tail: Number(7) } -> dbg("ok:7"),
      entry { head: Ident("bad"), tail: Number(_) } -> dbg("bad:number"),
      _ -> dbg("fallback")
    }
  };
  send(worker, entry { head: Ident("bad"), tail: Ident("not-number") })
}
