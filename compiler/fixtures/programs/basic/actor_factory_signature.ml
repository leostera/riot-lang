fn make_worker() {
  spawn {
    receive {
      "go" -> dbg("ok")
    }
  }
}

fn main() {
  dbg("ready")
}
