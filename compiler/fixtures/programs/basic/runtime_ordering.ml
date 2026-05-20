fn word_a() -> string { "alpha" }
fn word_b() -> string { "beta" }
fn number_a() -> i64 { 1 }
fn number_b() -> i64 { 2 }

fn main() {
  dbg(number_a() < number_b());
  dbg(word_a() < word_b());
  dbg("beta" < "alpha")
}
