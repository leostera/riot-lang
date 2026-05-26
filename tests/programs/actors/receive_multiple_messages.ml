fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) }
  };
  send(actor_id, "first");
  send(actor_id, "second");
  send(actor_id, "third")
}
