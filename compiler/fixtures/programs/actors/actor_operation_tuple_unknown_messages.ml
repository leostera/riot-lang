fn make_mixed(label: String) {
  spawn {
    receive { "go" -> dbg(string_concat(label, " string")) };
    receive { 1 -> dbg(string_concat(label, " i64")) }
  }
}

fn main() {
  let workers = (make_mixed("tuple unknown"), make_mixed("tuple unused"));
  send(workers.0, "go");
  send(workers.0, 1);
  ()
}
