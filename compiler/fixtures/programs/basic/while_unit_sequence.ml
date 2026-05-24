fn main() {
  let skipped = while false {
    dbg("loop")
  };
  dbg("done")
}
