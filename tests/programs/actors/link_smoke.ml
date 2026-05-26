fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) }
  };
  spawn {
    link(actor_id);
    send(actor_id, "linked");
    dbg("survived");
    receive { msg -> dbg(msg) }
  };
}
