type sender_box = { sender: actor_id<String> }

fn make_sender() {
  spawn { receive { "go" -> () } }
}

fn main() {
  let senders = sender_box { sender: make_sender() };
  send(senders.sender, 1);
  dbg("done")
}
