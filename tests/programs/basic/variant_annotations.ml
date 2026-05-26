type color = Red | Green | Blue

fn choose(flag: bool) -> color {
  if flag { Green } else { Blue }
}

fn echo(value: color) -> color {
  value
}

fn main() {
  let favorite: color = echo(choose(true));
  dbg(favorite)
}
