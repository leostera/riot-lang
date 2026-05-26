type choice = Primary | Backup

fn make_mixed() {
  spawn {
    receive { "go" -> dbg("match unknown string") };
    receive { 1 -> dbg("match unknown i64") }
  }
}

fn main() {
  let worker = match Primary {
    Primary -> make_mixed(),
    Backup -> make_mixed()
  };
  send(worker, "go");
  send(worker, 1);
  ()
}
