type wrapped = Wrapped((i64, i64))

fn main() {
  let value = Wrapped((1, 2));
  dbg(match value {
    Wrapped((left, right)) -> left + right
  })
}
