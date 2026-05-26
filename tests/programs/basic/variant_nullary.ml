type color = Red | Green | Blue

fn choose(n) {
  if n < 1 { Red } else { Green }
}

fn main() {
  dbg(choose(0));
  dbg(Blue);
  dbg(match choose(2) {
    Red -> "red",
    Green -> "green",
    _ -> "other"
  })
}
