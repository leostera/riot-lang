type option<'a> = Some('a) | None

fn main() {
  let worker = spawn {
    receive {
      Some(value) -> {
        let child = spawn {
          receive {
            () -> dbg(value + 1)
          }
        };
        send(child, ())
      },
      None -> ()
    }
  };
  send(worker, Some(41))
}
