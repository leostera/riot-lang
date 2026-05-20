type monitor_down = Down(actor_id<_>)

fn main() {
  let out = spawn {
    receive { _ -> dbg("down") }
  };
  let actor_id = spawn {
    receive { msg -> dbg(msg) }
  };
  spawn {
    monitor(actor_id);
    send(actor_id, "watched");
    receive {
      Down(id) -> send(out, id)
    }
  };
}
