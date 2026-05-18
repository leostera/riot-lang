fn main() {
  let flag = true;
  let left = (0, true);
  let right = (1, false);
  let answer = if flag { left } else { right };
  dbg(answer)
}
