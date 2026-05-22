type int_list = Ints(List<i64>)

fn sum(items: List<i64>) -> i64 {
  match items {
    [] -> 0,
    [head, ..tail] -> head + sum(tail)
  }
}

fn main() {
  let worker = spawn {
    receive {
      Ints([..items]) -> dbg(sum(items))
    }
  };
  send(worker, Ints([1, 2, 3, 4]))
}
