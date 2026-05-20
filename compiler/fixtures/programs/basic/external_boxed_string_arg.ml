external string_len_value : string -> i64 = "riot_rt_value_string_len"

fn main() {
  dbg(string_len_value("hello"))
}
