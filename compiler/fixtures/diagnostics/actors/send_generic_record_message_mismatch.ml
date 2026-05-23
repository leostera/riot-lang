type box<'a> = { value: 'a }

fn main() {
  let worker = spawn { receive { box { value } -> value + 1 } };
  send(worker, box { value: "oops" })
}
