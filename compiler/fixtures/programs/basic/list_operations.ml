fn words() -> List<String> { ["alpha", "beta"] }

fn main() {
  dbg(list_len([]));
  dbg(list_len([1, 2]));
  dbg(list_get([10, 20], 1));
  dbg(list_get(words(), 0))
}
