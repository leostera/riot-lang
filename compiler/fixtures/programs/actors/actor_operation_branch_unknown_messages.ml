fn main() {
  let worker = if true {
    spawn {
      receive { "go" -> dbg("branch unknown string") };
      receive { 1 -> dbg("branch unknown i64") }
    }
  } else {
    spawn {
      receive { "go" -> dbg("branch unknown string fallback") };
      receive { 1 -> dbg("branch unknown i64 fallback") }
    }
  };
  send(worker, "go");
  send(worker, 1);
  ()
}
