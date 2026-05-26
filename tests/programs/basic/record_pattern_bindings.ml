type point = { x: i64, y: i64 }
type boxed_point = Boxed(point)

fn main() {
  let record = point { x: 2, y: 3 };
  dbg(match record {
    point { x, y } -> x + y
  });

  let boxed = Boxed(record);
  dbg(match boxed {
    Boxed(point { x: left, y: right }) -> left * right
  })
}
