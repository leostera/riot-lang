type pair_box<'a, 'b> = Pair(('a, 'b))

fn main() {
  let pair = Pair((1, "one"));
  let label = match pair {
    Pair((value, name)) -> if value == 1 { name } else { "other" }
  };
  dbg(label)
}
