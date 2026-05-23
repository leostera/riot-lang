fn make_string_worker() {
  spawn { receive { "go" -> () } }
}

fn main() {
  let worker = make_string_worker();
  send(worker, 1);
  dbg("done")
}
