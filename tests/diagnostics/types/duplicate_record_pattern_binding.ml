type pair = { left: i64, right: i64 }

fn main() {
  let pair = pair { left: 1, right: 2 };
  dbg(match pair {
    pair { left: value, right: value } -> value,
    _ -> 0
  })
}
