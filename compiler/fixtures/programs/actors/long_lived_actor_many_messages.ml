fn main() {
  let actor_id = spawn {
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) }
  };
  send(actor_id, "one");
  send(actor_id, "two");
  send(actor_id, "three");
  send(actor_id, "four");
  send(actor_id, "five")
}
