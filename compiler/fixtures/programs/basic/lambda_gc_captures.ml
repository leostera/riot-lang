external gc_collect : i64 = "riot_rt_gc_collect"

fn main() {
  let captured = string_concat("capt", "ured");
  let f = fn(ignored) { captured };
  gc_collect();
  dbg(f(()))
}
