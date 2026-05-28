let foo ((a, b): (u8, u32)) = a * 2

let block = {
  let (x: u8) = 1;
  x
}
