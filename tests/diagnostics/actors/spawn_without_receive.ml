fn main() {
  let actor_id = spawn {
    dbg("not a receive loop")
  };
  send(actor_id, "hello")
}
