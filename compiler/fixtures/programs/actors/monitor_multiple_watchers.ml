fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) }
  };
  monitor(actor_id);
  monitor(actor_id);
  send(actor_id, "watched twice")
}
