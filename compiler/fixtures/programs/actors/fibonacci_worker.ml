fn fib(n) {
  if n < 2 {
    n
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

fn main() {
  let out = spawn {
    receive { msg -> dbg(msg) }
  };
  let fib6 = spawn {
    receive { start -> send(out, fib(6)) }
  };
  send(fib6, "go")
}
