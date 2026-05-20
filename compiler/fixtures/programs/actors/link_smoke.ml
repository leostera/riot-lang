fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) }
  };
  link(actor_id);
  send(actor_id, "linked")
}
