type color = Red | Blue | Named(String)
type box = { label: String, count: i64 }

fn main() {
  let string_same = "riot" == "riot";
  let tuple_same = (1, "one") == (1, "one");
  let list_same = [1, 2, 3] == [1, 2, 3];
  let record_same = box { label: "items", count: 3 } == box { label: "items", count: 3 };
  let variant_same = Named("red") == Named("red");
  let variant_diff = Red == Blue;
  let label = if string_same && tuple_same && list_same && record_same && variant_same && !variant_diff {
    "structural equality"
  } else {
    "bad"
  };
  dbg(label)
}
