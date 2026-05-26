type point = { x: i64, y: i64 }

fn make_point(x: i64) {
  point { x: x, y: 20 }
}

fn main() {
  let point = make_point(10);
  dbg(point.x);
  dbg(make_point(30).y)
}
