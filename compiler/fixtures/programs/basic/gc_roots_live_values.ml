external gc_collect : i64 = "riot_rt_gc_collect"

fn main() {
  let early = string_concat("ear", "ly");
  gc_collect();
  let later = string_concat("lat", "er");
  gc_collect();
  dbg(early);
  dbg(later)
}
