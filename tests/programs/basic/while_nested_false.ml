fn main() {
  while false {
    while false {
      dbg("inner")
    };
    dbg("outer")
  };
  dbg("done")
}
