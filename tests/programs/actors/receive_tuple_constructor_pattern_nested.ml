type token = Ident(String) | Number(i64)

fn main() {
  let worker = spawn {
    receive {
      (Ident("ok"), Number(7)) -> dbg("ok:7"),
      (Ident("bad"), Number(_)) -> dbg("bad:number"),
      _ -> dbg("fallback")
    }
  };
  send(worker, (Ident("bad"), Ident("not-number")))
}
