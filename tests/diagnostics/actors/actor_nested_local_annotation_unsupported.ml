fn main() {
  let outer = spawn {
    let inner = spawn {
      let count: i64 = 1;
      receive {
        _ -> println("inner")
      }
    };
    receive {
      _ -> monitor(inner)
    }
  };
  monitor(outer)
}
