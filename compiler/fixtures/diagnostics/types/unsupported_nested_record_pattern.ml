use Result

type point = { x: i64 }

fn main() {
  let value = point { x: 1 };
  dbg(match value {
    Result.point.extra { x: x } -> x,
    _ -> 0
  })
}
