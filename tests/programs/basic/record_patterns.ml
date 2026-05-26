type point = { x: i64, y: i64 }

fn make_point(n) {
  point { x: n, y: n + 1 }
}

fn main() {
  let sum = match make_point(20) {
    point { x, y: right } -> x + right,
    _ -> 0
  };
  dbg(sum);

  let projected = match point { x: 1, y: 2 } {
    point { y } -> y,
    _ -> 0
  };
  dbg(projected)
}
