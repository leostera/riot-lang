type box<'a> = { value: 'a }

fn string_worker() {
  spawn {
    receive { "go" -> dbg("ok") }
  }
}

fn boxed_worker() {
  spawn {
    receive { box { value } -> dbg(value + 1) }
  }
}

fn mixed_worker() {
  spawn {
    receive { "go" -> dbg("ok") };
    receive { 1 -> dbg("one") }
  }
}

fn main() {
  dbg("ready")
}
