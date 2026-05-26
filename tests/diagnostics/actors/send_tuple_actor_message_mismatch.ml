fn make_sender() {
  spawn { receive { "go" -> () } }
}

fn main() {
  let senders = (make_sender(), make_sender());
  send(senders.1, 1);
  dbg("done")
}
