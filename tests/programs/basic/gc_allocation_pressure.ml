external gc_set_threshold : i64 -> unit = "riot_rt_gc_set_threshold"

fn main() {
  gc_set_threshold(1);
  let keep = string_concat("he", "ap");
  let trash = string_concat("tr", "ash");
  let more = string_concat("mo", "re");
  dbg(keep);
  dbg(trash);
  dbg(more)
}
