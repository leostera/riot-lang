fn main() {
  let x = if true { if false { 1 } else { 2 } } else { 3 };
  dbg(x)
}
