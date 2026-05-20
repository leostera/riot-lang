type monitor_down = Down(actor_id<_>)

fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) }
  };
  spawn {
    monitor(actor_id);
    receive {
      Down(_) -> dbg("down")
    }
  };
}
