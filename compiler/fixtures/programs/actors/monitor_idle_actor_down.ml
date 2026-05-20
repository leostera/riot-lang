fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) }
  };
  monitor(actor_id)
}
