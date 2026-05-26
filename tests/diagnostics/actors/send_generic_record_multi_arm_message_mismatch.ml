type box<'a> = { value: 'a }

fn main() {
  let worker = spawn {
    receive {
      box { value } -> value + 1,
      box { value: _ } -> (),
    }
  };
  send(worker, box { value: "oops" })
}
