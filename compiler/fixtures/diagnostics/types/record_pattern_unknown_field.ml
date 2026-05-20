type point = { x: i64, y: i64 }

fn main() {
  let value = point { x: 1, y: 2 };
  dbg(match value {
    point { z } -> z,
    _ -> 0
  })
}
