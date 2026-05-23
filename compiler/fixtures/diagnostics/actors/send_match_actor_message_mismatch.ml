type choice = Sender | Backup

fn make_sender() {
  spawn { receive { "go" -> () } }
}

fn main() {
  let worker = match Sender {
    Sender -> make_sender(),
    Backup -> make_sender()
  };
  send(worker, 1);
  dbg("done")
}
