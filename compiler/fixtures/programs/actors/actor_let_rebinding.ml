fn main() {
  let actor_id = spawn {
    let x = 1;
    let x = 2;
    receive { msg -> dbg(msg) }
  };
  send(actor_id, "ok")
}
