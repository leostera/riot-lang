type symbol = { name: String, value: i64 }

fn make_symbol(name: String, value: i64) {
  symbol { name, value }
}

fn main() {
  let symbol = make_symbol("input", 42);
  dbg(symbol.name);
  dbg(symbol.value)
}
