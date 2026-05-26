type box<'a> = { value: 'a }

fn main() {
  let worker = spawn {
    receive {
      box { value } -> {
        let child = spawn {
          receive {
            () -> dbg(value + 1)
          }
        };
        send(child, ())
      }
    }
  };
  send(worker, box { value: 41 })
}
