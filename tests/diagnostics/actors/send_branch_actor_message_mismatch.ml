fn main() {
  let worker = if true {
    spawn { receive { "go" -> () } }
  } else {
    spawn { receive { "go" -> () } }
  };
  send(worker, 1);
  dbg("done")
}
