fn make_point(x: i64) -> Point { Point { x: x, y: 20 } }

fn main() {
  let point = make_point(10);
  dbg(point.x);
  dbg(make_point(30).y)
}
