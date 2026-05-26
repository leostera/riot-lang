external concat_value : 'a -> 'a -> String = "riot_rt_value_string_concat"

fn main() {
  dbg(concat_value("riot", " lang"))
}
