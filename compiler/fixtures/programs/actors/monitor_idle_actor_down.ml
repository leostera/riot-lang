type monitor_down = Down(i64)

fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) }
  };
  spawn {
    monitor(actor_id);
    receive {
      Down(id) -> dbg(id)
    }
  };
}
