external string_len_value : String -> i64 = "riot_rt_value_string_len"

fn main() {
  dbg(string_len_value("hello"))
}
