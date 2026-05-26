fn main() {
  let worker: actor_id<_> = spawn {
    receive { "go" -> dbg("string") };
    receive { 1 -> dbg("int") }
  };
  send(worker, "go");
  send(worker, 1)
}
