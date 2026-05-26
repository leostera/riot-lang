fn fib(n) {
  dbg(n);
  if n < 2 {
     1 
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

fn main() {
  let out = spawn {
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) };
    receive { msg -> dbg(msg) }
  };
  let slow = spawn {
    receive { start -> send(out, fib(5)) }
  };
  let fast = spawn {
    receive { start -> send(out, fib(3)) }
  };
  let mid = spawn {
    receive { start -> send(out, fib(4)) }
  };
  send(slow, "go");
  send(fast, "go");
  send(mid, "go")
}
