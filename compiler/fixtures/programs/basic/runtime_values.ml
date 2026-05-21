fn make_pair(name: String) -> (String, i64) {
  (name, 42)
}

fn main() {
  dbg((make_pair("riot"), [1, 2, 3], Point { x: 10, y: 20 }))
}
