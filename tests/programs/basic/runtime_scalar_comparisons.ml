fn main() {
  let char_result = if 'q' == 'q' { "char eq" } else { "bad" };
  let float_result = if 2.0 == 2.0 { "float eq" } else { "bad" };
  dbg(char_result);
  dbg(float_result)
}
