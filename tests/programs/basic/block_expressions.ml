fn nested(flag) {
  {
    let outer = 10;
    if flag {
      let inner = outer + 32;
      inner
    } else {
      {
        let inner = outer + 1;
        inner
      }
    }
  }
}

fn main() {
  let value = {
    let base = 40;
    let add = 2;
    base + add
  };
  dbg(value);
  dbg(if true {
    let branch = 5;
    branch + nested(false)
  } else {
    0
  });
  dbg({
    let text = "scoped";
    text
  })
}
