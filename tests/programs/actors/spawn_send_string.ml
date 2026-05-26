fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) }
  };
  send(actor_id, "hello actor")
}
